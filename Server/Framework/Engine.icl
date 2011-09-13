implementation module Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool, Func
from StdFunc import o, seqList, ::St
import	Map, Time, CommandLine, Error, File, FilePath, Directory, HTTP, OSError, Text, MIME, UrlEncoding
import	Util, HtmlUtil
import	TuningCombinators
import	Setup
import	Config
import	IWorld
import	TaskService

// The iTasks engine consist of a set of HTTP request handlers
engine :: !(Maybe Config) (Task a) ![Handler] -> [(!String -> Bool,!HTTPRequest *World -> (!HTTPResponse, !*World))] | iTask a
engine mbConfig task handlers
	= case mbConfig of
		Just config
			= handlers` config
		Nothing
			= [(\_ -> True, setupHandler)]
where
	handlers` config
		= [
		  // Handler to stop the server nicely
		   ((==) "/stop", handleStopRequest)
		  // Webservices
		  ,((==) "/", taskDispatch config task)
		  ,(startsWith config.serverPath, serviceDispatch config)
		  ,(\_ -> True, handleStaticResourceRequest config)
		  ]

	serviceDispatch config req world
		# iworld			= initIWorld config world
		# reqpath			= (urlDecode req.req_path)
		# reqpath			= reqpath % (size config.serverPath, size reqpath)
		# (response,iworld)	= case (split "/" reqpath) of
			[""]								= (redirectResponse (req.req_path +++ "/html"), iworld)
			["","html"]							= (overviewResponse, iworld)
			["",format,name:path] = case filter (\(name`,formats,_) -> name` == name && isMember format formats) handlers of
				[(_,_,handler):_]	= handler req.req_path format path req iworld
				[]					= (notFoundResponse req, iworld)
			_
				= (notFoundResponse req, iworld)
		= (response, finalizeIWorld iworld)

	taskDispatch config task req world
		# iworld 			= initIWorld config world
		# (response,iworld)	= taskService task req iworld
		= (response, finalizeIWorld iworld)
		
initIWorld :: !Config !*World -> *IWorld
initIWorld config world
	# (appName,world) 			= determineAppName world
	# (appPath,world)			= determineAppPath world
	# appDir					= takeDirectory appPath
	# (res,world)				= getFileInfo appPath world
	| isError res				= abort "Cannot get executable info."
	# tm						= (fromOk res).lastModifiedTime
	# datestr					= strfTime "%Y.%m.%d-%H.%M.%S" tm
	# (timestamp,world)			= time world
	# (localDateTime,world)		= currentDateTimeWorld world
	# (_,world)					= ensureDir "data" (appDir </> appName) world
	# tmpPath					= appDir </> appName </> "tmp-" +++ datestr
	# (_,world)					= ensureDir "tmp" tmpPath world
	# storePath					= appDir </> appName </> "store-"+++ datestr
	# (exists,world)			= ensureDir "store" storePath world
	= {IWorld
	  |application			= appName
	  ,storeDirectory		= storePath
	  ,tmpDirectory			= tmpPath
	  ,config				= config
	  ,timestamp			= timestamp
	  ,latestEvent			= Nothing
	  ,localDateTime		= localDateTime
	  ,currentUser			= AnyUser
	  ,evalStack			= []
	  ,parallelStates		= newMap
	  ,parallelControls		= newMap
	  ,readShares			= Nothing
	  ,world				= world
	  }
where 
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
handleStaticResourceRequest :: !Config !HTTPRequest *World -> (!HTTPResponse,!*World)
handleStaticResourceRequest config req world
	# path					= if (req.req_path == "/") "/index.html" req.req_path
	# filename				= config.clientPath +++ filePath path
	# type					= mimeType filename
	# (mbContent, world)	= readFile filename world
	| isOk mbContent		= ({rsp_headers = fromList [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size (fromOk mbContent)))]
							   	,rsp_data = fromOk mbContent}, world)
	# filename				= config.staticPath +++ filePath path
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

config :: !*World -> (!Maybe Config,!*World)
config world
	# (appName,world) = determineAppName world
	= loadConfig appName world
									  
// Determines the server executables path
determineAppPath :: !*World -> (!String, !*World)
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
