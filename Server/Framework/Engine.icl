implementation module Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool, Func
from StdFunc import o, seqList, ::St
import	Map, Time, CommandLine, Environment, Error, File, FilePath, Directory, HTTP, OSError, Text, MIME, UrlEncoding
import	Util, HtmlUtil
import	IWorld
import	WebService

CLEAN_HOME_VAR	:== "CLEAN_HOME"

// The iTasks engine consist of a set of HTTP request handlers
engine :: !FilePath publish -> [(!String -> Bool,!HTTPRequest *World -> (!HTTPResponse, !*World))] | Publishable publish
engine sdkPath publishable
	= taskHandlers (publishAll publishable) sdkPath ++ defaultHandlers sdkPath
where
	taskHandlers published sdkPath
		= [((==) url, taskDispatch sdkPath task defaultFormat) \\ {url,task=TaskWrapper task,defaultFormat} <- published]	
	
	taskDispatch sdkPath task defaultFormat req world
		# iworld 			= initIWorld sdkPath world
		# (response,iworld)	= webService task defaultFormat req iworld
		= (response, finalizeIWorld iworld)
	
	defaultHandlers sdkPath
		= [((==) "/stop", handleStopRequest),(\_ -> True, handleStaticResourceRequest sdkPath)]
		
initIWorld :: !FilePath !*World -> *IWorld
initIWorld sdkPath world
	# (appName,world) 			= determineAppName world
	# (appPath,world)			= determineAppPath world
	# appDir					= takeDirectory appPath
	# dataDir					= appDir </> appName +++ "-data"
	# (res,world)				= getFileInfo appPath world
	| isError res				= abort "Cannot get executable info."
	# tm						= (fromOk res).lastModifiedTime
	# build						= strfTime "%Y%m%d-%H%M%S" tm
	# (timestamp,world)			= time world
	# (localDateTime,world)		= currentDateTimeWorld world
	# (_,world)					= ensureDir "data" dataDir world
	# tmpDir					= dataDir </> "tmp-" +++ build
	# (_,world)					= ensureDir "tmp" tmpDir world
	# storeDir					= dataDir </> "store-"+++ build
	# (exists,world)			= ensureDir "store" storeDir world
	= {IWorld
	  |application			= appName
	  ,build				= build
	  ,appDirectory			= appDir
	  ,sdkDirectory			= sdkPath
	  ,dataDirectory		= dataDir
	  ,config				= defaultConfig
	  ,timestamp			= timestamp
	  ,latestEvent			= Nothing
	  ,localDateTime		= localDateTime
	  ,currentUser			= AnyUser
	  ,nextTaskNo			= 0
	  ,evalStack			= []
	  ,parallelLists		= newMap
	  ,parallelControls		= newMap
	  ,localShares			= newMap
	  ,readShares			= Nothing
	  ,world				= world
	  }
where
	defaultConfig :: Config
	defaultConfig =
		{ rootPassword		= "root"
		, rootEmail			= "root@localhost"
		, sessionTime		= 3600
		, smtpServer		= "localhost"
		}
		
	padZero :: !Int -> String
	padZero number = (if (number < 10) "0" "") +++ toString number

	ensureDir :: !String !FilePath *World -> (!Bool,!*World)
	ensureDir name path world
		# (exists, world) = fileExists path world
		| exists = (True,world)
		# (res, world) = createDirectory path world
		| isError res = abort ("Cannot create " +++ name +++ " directory" +++ path +++ " : "  +++ snd (fromError res))
		= (False,world)

finalizeIWorld :: !*IWorld -> *World
finalizeIWorld iworld=:{IWorld|world} = world

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...
handleStaticResourceRequest :: !FilePath !HTTPRequest *World -> (!HTTPResponse,!*World)
handleStaticResourceRequest sdkPath req world
	# (appPath,world)		= determineAppPath world
	# path					= if (req.req_path == "/") "/index.html" req.req_path
	# filename				= sdkPath </> "Client" </> filePath path
	# type					= mimeType filename
	# (mbContent, world)	= readFile filename world
	| isOk mbContent		= ({rsp_headers = fromList [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size (fromOk mbContent)))]
							   	,rsp_data = fromOk mbContent}, world)
	# filename				= takeDirectory appPath </> "Static" </> filePath path
	# type					= mimeType filename
	# (mbContent, world)	= readFile filename world
	| isOk mbContent 		= ({rsp_headers = fromList [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size (fromOk mbContent)))											   
											   ]
							   	,rsp_data = fromOk mbContent}, world)						   								 	 							   
	= (notFoundResponse req,world)
where
	//Translate a URL path to a filesystem path
	filePath path	= ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) path
	mimeType path	= extensionToMimeType (takeExtension path)

handleStopRequest :: HTTPRequest *World -> (!HTTPResponse,!*World)
handleStopRequest req world = ({newHTTPResponse & rsp_headers = fromList [("X-Server-Control","stop")], rsp_data = "Server stopped..."}, world) //Stop

path2name path = last (split "/" path)

publish :: String ServiceFormat (Task a) -> PublishedTask | iTask a
publish url format task = {url = url, task = TaskWrapper task, defaultFormat = format}

instance Publishable (Task a) | iTask a
where
	publishAll task = [publish "/" WebApp task]

instance Publishable [PublishedTask]
where
	publishAll list = list

// Determines the server executables path
determineAppPath :: !*World -> (!FilePath, !*World)
determineAppPath world
	# ([arg:_],world) = getCommandLine world
	| dropDirectory arg <> "ConsoleClient.exe" = (arg, world)
	//Using dynamic linker:	
	# (res, world)				= getCurrentDirectory world	
	| isError res				= abort "Cannot get current directory."	
	# currentDirectory			= fromOk res
	# (res, world)				= readDirectory currentDirectory world	
	| isError res				= abort "Cannot read current directory."	
	# batchfiles				= [f \\ f <- fromOk res | takeExtension f == "bat" ]
	| isEmpty batchfiles		= abort "No dynamic linker batch file found."	
	# (infos, world)			= seqList (map getFileInfo batchfiles) world	
	| any isError infos	 		= abort "Cannot get file information."	
	= (currentDirectory </> (fst o hd o sortBy cmpFileTime) (zip2 batchfiles infos), world)	
	where		
		cmpFileTime (_,Ok {FileInfo | lastModifiedTime = x})
					(_,Ok {FileInfo | lastModifiedTime = y}) = mkTime x > mkTime y
	
// Determines the server executables name
determineAppName :: !*World -> (!String,!*World)
determineAppName world 
	# (appPath, world) = determineAppPath world
	= ((dropExtension o dropDirectory) appPath, world)

determineSDKPath :: ![FilePath] !*World -> (!Maybe FilePath, !*World)
determineSDKPath paths world
	//Try environment var first
	# (mbCleanHome,world) = getEnvironmentVariable CLEAN_HOME_VAR world
	= case mbCleanHome of
		Nothing			= searchPaths paths world
		Just cleanHome	= searchPaths [cleanHome] world
where	
	searchPaths [] world = (Nothing, world)
	searchPaths [p:ps] world
		# (mbInfo,world) = getFileInfo path world
		= case mbInfo of
			Ok info	| info.directory	= (Just path,world)
			_							= searchPaths ps world
	where
		path = (p </> "iTasks-SDK")
	
