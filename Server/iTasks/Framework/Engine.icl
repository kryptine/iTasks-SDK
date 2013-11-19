implementation module iTasks.Framework.Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool
from StdFunc import o, seqList, ::St
import Data.Map, Data.Error, Data.Func, Data.Tuple, Internet.HTTP, Text, Text.Encodings.MIME, Text.Encodings.UrlEncoding
import System.Time, System.CommandLine, System.Environment, System.OSError, System.File, System.FilePath, System.Directory
import iTasks.Framework.Util, iTasks.Framework.HtmlUtil
import iTasks.Framework.IWorld, iTasks.Framework.WebService

CLEAN_HOME_VAR	:== "CLEAN_HOME"

import StdFile, StdInt, StdList, StdChar, StdBool, StdString, StdFunc
import tcp
import Internet.HTTP, System.Time, System.CommandLine, Data.Func

import iTasks.Framework.Engine, iTasks.Framework.IWorld, iTasks.Framework.TaskEval, iTasks.Framework.TaskStore
import iTasks.Framework.Util
import iTasks.Framework.TaskServer

from Data.Set import :: Set, newSet
from Sapl.Linker.LazyLinker import generateLoaderState, :: LoaderStateExt
from Sapl.Linker.SaplLinkerShared import :: SkipSet
from Sapl.Target.Flavour import :: Flavour, toFlavour

startEngine :: a !*World -> *World | Publishable a
startEngine publishable world
	# (opts,world)			= getCommandLine world
	# (app,world)			= determineAppName world
	# (mbSDKPath,world)		= determineSDKPath SEARCH_PATHS world
	// Show server name
	# world					= show (infoline app) world
	//Check options
	# port 					= fromMaybe DEFAULT_PORT (intOpt "-port" opts)
	# keepalive				= fromMaybe DEFAULT_KEEPALIVE_TIME (intOpt "-keepalive" opts)
    # theme                 = fromMaybe DEFAULT_THEME (stringOpt "-theme" opts)
	# help					= boolOpt "-help" opts
	# sdkOpt				= stringOpt "-sdk" opts
	//If -help option is given show help and stop
	| help					= show instructions world
	//Check sdkpath
	# mbSDKPath				= maybe mbSDKPath Just sdkOpt //Commandline SDK option overrides found paths
	| isNothing mbSDKPath	= show sdkpatherror world
	//Normal execution
	# world					= show (running port) world
	# iworld				= initIWorld (fromJust mbSDKPath) theme world
	// mark all instance as outdated initially
	# (maxNo,iworld)			= maxInstanceNo iworld
	# iworld				= addOutdatedInstances [(instanceNo, Nothing) \\ instanceNo <- [1..maxNo]] iworld
	# iworld				= startHTTPServer port keepalive (engine publishable) timeout background iworld
	= finalizeIWorld iworld
where
	infoline :: !String -> [String]
	infoline app	= ["*** " +++ app +++ " HTTP server ***",""]
	
	instructions :: [String]
	instructions =
		["Available commandline options:"
		," -help             : Show this message and exit" 
		," -sdk <path>       : Use <path> as location of the iTasks SDK"
		," -port <port>      : Set port number (default " +++ toString DEFAULT_PORT +++ ")"
		," -keepalive <time> : Set connection keepalive time in seconds (default " +++ toString DEFAULT_KEEPALIVE_TIME +++ ")"
		,""
		]
	
	sdkpatherror :: [String]
	sdkpatherror =
		["Oops! Could not find the iTasks SDK."
		,"The server needs to know the location of the SDK to serve static content"
		,"and run its various utility programs."
		,""
		,"Please put the \"iTasks-SDK\" folder in one of the search locations"
		,"or use the -sdk commandline flag to set the path."
		,"Example: -sdk C:\\Users\\johndoe\\Desktop\\Clean2.4\\iTasks-SDK"
		,""
		,"Tried to find a folder named \"iTasks-SDK\" in the following search locations:"
		:SEARCH_PATHS]
		
	running :: !Int -> [String]
	running port = ["Running at http://localhost" +++ (if (port == 80) "/" (":" +++ toString port +++ "/"))]
	
	show :: ![String] !*World -> *World
	show lines world
		# (console,world)	= stdio world
		# console			= seqSt (\s c -> fwrites (s +++ "\n") c) lines console
		# (_,world)			= fclose console world
		= world
		
	boolOpt :: !String ![String] -> Bool
	boolOpt key opts = isMember key opts
	
	intOpt :: !String ![String] -> Maybe Int
	intOpt key []	= Nothing
	intOpt key [_]	= Nothing
	intOpt key [n,v:r]
		| n == key && isInteger v	= Just (toInt v)
									= intOpt key [v:r]
	where								
		isInteger v = and (map isDigit (fromString v))

	stringOpt :: !String [String] -> Maybe String
	stringOpt key [] = Nothing
	stringOpt key [_] = Nothing
	stringOpt key [n,v:r]
		| n == key	= Just v
					= stringOpt key [v:r]
					
	timeout :: !*IWorld -> (!Maybe Timeout,!*IWorld)
	timeout iworld = (Just 100, iworld)					//Run at least 10 times a second

	background :: !*IWorld -> (!Bool,!*IWorld)
	background iworld=:{IWorld|shutdown=True}
		= (True,iworld)
	background iworld
		# iworld			= updateCurrentDateTime iworld
		# (mbWork, iworld)	= dequeueWork iworld
		# iworld = case mbWork of
			Empty
				= iworld
			Work work
				# iworld = case work of
					(Evaluate instanceNo)		= refreshTaskInstance instanceNo iworld
					(EvaluateUrgent instanceNo)	= refreshTaskInstance instanceNo iworld
					(TriggerSDSChange sdsId)	= addOutdatedOnShareChange sdsId (const True) iworld
					(CheckSDS sdsId hash checkF)
						# (checkRes,iworld)		= checkF iworld
						= case checkRes of
							Changed				= addOutdatedOnShareChange sdsId (const True) iworld
							(CheckAgain time)	= queueWork (CheckSDS sdsId hash checkF, Just time) iworld
				= iworld // give http server the chance to handle request
			WorkAt time
				= iworld
				/*
				# (curTime, iworld) = currentTimestamp iworld
				= (Just (toTimeout curTime time), iworld)
				*/
		= (False,iworld)

	toTimeout (Timestamp curTime) (Timestamp nextRefresh)
		# delta = nextRefresh - curTime
		| delta < 0					= 0
		| delta > MAX_TIMEOUT/1000	= MAX_TIMEOUT
		| otherwise					= delta*1000
	
MAX_TIMEOUT :== 86400000 // one day

// The iTasks engine consist of a set of HTTP request handlers
engine :: publish -> [(!String -> Bool
					  ,!Bool
					  ,!(HTTPRequest *IWorld -> (!HTTPResponse,!Maybe SessionId, !*IWorld))
					  ,!(HTTPRequest (Maybe {#Char}) SessionId *IWorld -> (!Maybe {#Char}, !Bool, !SessionId, !*IWorld))
					  ,!(HTTPRequest SessionId *IWorld -> *IWorld)
					  )] | Publishable publish
engine publishable
	= taskHandlers (publishAll publishable) ++ defaultHandlers
where
	taskHandlers published
		= [let (reqF,dataF,disconnectF) = webService task defaultFormat in ((==) (URL_PREFIX +++ url),True,reqF,dataF,disconnectF)
		  \\ {url,task=TaskWrapper task,defaultFormat} <- published]	
	
	defaultHandlers = [simpleHTTPResponse (startsWith URL_PREFIX, handleStaticResourceRequest)]

readFlavour :: !String !*World -> *(!Flavour, !*World)
readFlavour sdkPath world
	# flavfile 			= sdkPath </> "Server" </> "lib" </> "SAPL" </>"clean.f"
	# (flavres, world) 	= readFile flavfile world
	| isError flavres
		= abort ("JavaScript Flavour file cannot be found at " +++ flavfile)
	# mbFlav 			= toFlavour (fromOk flavres)
	| isNothing mbFlav
		= abort "Error in JavaScript flavour file"	
	= (fromJust mbFlav, world)
		
initIWorld :: !FilePath !String !*World -> *IWorld
initIWorld sdkDir theme world
	# (appName,world) 			= determineAppName world
	# (appPath,world)			= determineAppPath world
	# appDir					= takeDirectory appPath
	# dataDir					= appDir </> appName +++ "-data"
    # (extensionsWeb,world)     = determineWebPublicDirs (sdkDir </>"Server"</>"iTasks"</>"API"</>"Extensions") world
	# (res,world)				= getFileInfo appPath world
	| isError res				= abort "Cannot get executable info."
	# tm						= (fromOk res).lastModifiedTime
	# build						= strfTime "%Y%m%d-%H%M%S" tm
	# (timestamp,world)			= time world
	# (currentDateTime,world)	= currentDateTimeWorld world
	# (_,world)					= ensureDir "data" dataDir world
	# tmpDir					= dataDir </> "tmp-" +++ build
	# (_,world)					= ensureDir "tmp" tmpDir world
	# storeDir					= dataDir </> "store-"+++ build
	# (exists,world)			= ensureDir "store" storeDir world
	
	# ((lst, ftmap, _), world)  = generateLoaderState ["sapl","sapl-override"] [] 
										["iTasks.Framework.Client.Override"
										,"_SystemDynamic"
										,"dynamic_string"
										,"graph_to_string_with_descriptors"
										,"graph_to_sapl_string"
										,"Text.Encodings.Base64"
										,"Sapl.LazyLinker"
										,"Sapl.Target.JS.CodeGeneratorJS"
										,"System.Time"
										,"System.Pointer"
										,"System.File"
										,"System.Directory"] world
										
	# (flavour, world)			= readFlavour sdkDir world
	
	= {IWorld
	  |application			= appName
	  ,build				= build
	  ,systemDirectories    =
            {appDirectory		    = appDir
	        ,sdkDirectory		    = sdkDir
	        ,dataDirectory		    = dataDir
            ,publicWebDirectories   = [sdkDir </> "Client", appDir </> "Static":extensionsWeb]
            }
	  ,config				= initialConfig theme
	  ,taskTime				= 0
	  ,timestamp			= timestamp
	  ,currentDateTime		= currentDateTime
	  ,currentUser			= AnonymousUser ""
	  ,currentInstance		= 0
      ,currentSession       = Nothing
      ,currentAttachment    = []
	  ,nextTaskNo			= 0
	  ,localShares			= newMap
	  ,localLists			= newMap
	  ,localTasks			= newMap
      ,eventRoute			= newMap
	  ,readShares			= []
	  ,sessions				= newMap
	  ,jsCompilerState		= (lst, ftmap, flavour, Nothing, newMap)
      ,editletDiffs         = newMap
	  ,workQueue			= []
	  ,uiMessages           = newMap
	  ,shutdown				= False
	  ,world				= world
      ,resources            = Nothing
      ,onClient				= False
	  }
where
	initialConfig :: String -> Config
	initialConfig theme =
		{ sessionTime		= 3600
		, smtpServer		= "localhost"
        , theme             = theme
		}
		
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
handleStaticResourceRequest :: !HTTPRequest *IWorld -> (!HTTPResponse,!*IWorld)
handleStaticResourceRequest req iworld=:{IWorld|systemDirectories={publicWebDirectories}}
    = serveStaticResource req publicWebDirectories iworld
where
    serveStaticResource req [] iworld
	    = (notFoundResponse req,iworld)
    serveStaticResource req [d:ds] iworld=:{IWorld|world}
	    # path			= subString (size URL_PREFIX) (size req.req_path) req.req_path
	    # filename		= d +++ filePath path
	    # type			= mimeType filename
	    # (mbContent, world)	= readFile filename world
	    | isOk mbContent		= ({rsp_headers = fromList [("Status","200 OK"),
								    ("Content-Type", type),
								    ("Content-Length", toString (size (fromOk mbContent)))]
							   	    ,rsp_data = fromOk mbContent}, {IWorld|iworld & world = world})
        | otherwise
            = serveStaticResource req ds {IWorld|iworld & world = world}

	//Translate a URL path to a filesystem path
	filePath path	= ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) path
	mimeType path	= extensionToMimeType (takeExtension path)

//path2name path = last (split "/" path)

publish :: String ServiceFormat (HTTPRequest -> Task a) -> PublishedTask | iTask a
publish url format task = {url = url, task = TaskWrapper task, defaultFormat = format}

instance Publishable (Task a) | iTask a
where
	publishAll task = [publish "/" WebApp (\_ -> task)]

instance Publishable (HTTPRequest -> Task a) | iTask a
where
	publishAll task = [publish "/" WebApp task]
	
instance Publishable [PublishedTask]
where
	publishAll list = list

// Determines the server executables path
determineAppPath :: !*World -> (!FilePath, !*World)
determineAppPath world
	# ([arg:_],world) = getCommandLine world 
	| dropDirectory arg <> "ConsoleClient.exe"	= toCanonicalPath arg world
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
		Just cleanHome	= searchPaths [cleanHome, cleanHome </> "lib", cleanHome </> "Libraries"] world
where	
	searchPaths [] world = (Nothing, world)
	searchPaths [p:ps] world
		# (mbInfo,world) = getFileInfo path world
		= case mbInfo of
			Ok info	| info.directory	= (Just path,world)
			_							= searchPaths ps world
	where
		path = (p </> "iTasks-SDK")

//Do a recursive scan of a directory for subdirectories with the name "WebPublic"
//Files in these directories are meant to be publicly served by an iTask webserver
determineWebPublicDirs :: !FilePath !*World -> (![FilePath], !*World)
determineWebPublicDirs path world
	# (dir, world)	= readDirectory path world	
    = case dir of
        Ok entries
            = appFst flatten (mapSt (checkEntry path) entries world)
        _   = ([],world) //TODO pass error up instead of just returning an empty list
where
    checkEntry :: !FilePath !String !*World -> (![FilePath], !*World)
    checkEntry dir name world
        # path = dir </> name
        | name == "." || name == ".." = ([],world)
        | name == "WebPublic"   = ([path],world) //Dont' recurse into a found WebPublic dir
        | otherwise
		    # (mbInfo,world) = getFileInfo path world
		    = case mbInfo of
			    Ok info	| info.directory	= determineWebPublicDirs path world //Continue search
                _                           = ([],world)
	
