implementation module iTasks._Framework.Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool, StdEnum
from StdFunc import o, seqList, ::St, const
import Data.Map, Data.Error, Data.Func, Data.Tuple, Math.Random, Internet.HTTP, Text, Text.Encodings.MIME, Text.Encodings.UrlEncoding
import System.Time, System.CommandLine, System.Environment, System.OSError, System.File, System.FilePath, System.Directory
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil
import iTasks._Framework.IWorld, iTasks._Framework.WebService, iTasks._Framework.SDSService
import iTasks.API.Common.SDSCombinators

CLEAN_HOME_VAR	:== "CLEAN_HOME"
SESSION_TIMEOUT :== fromString "0000-00-00 00:10:00"

//The following modules are excluded by the SAPL -> Javascript compiler
//because they contain functions implemented in ABC code that cannot
//be compiled to javascript anyway. Handwritten Javascript overrides need
//to be provided for them.
JS_COMPILER_EXCLUDES :==
	["iTasks._Framework.Client.Override"
	,"dynamic_string"
	,"graph_to_string_with_descriptors"
	,"graph_to_sapl_string"
	,"Text.Encodings.Base64"
	,"Sapl.LazyLinker"
	,"Sapl.Target.JS.CodeGeneratorJS"
	,"System.Pointer"
	,"System.File"
	,"System.Directory"
	]

import StdInt, StdChar, StdString
import tcp
import Internet.HTTP, System.Time, System.CommandLine, Data.Func

import iTasks._Framework.Engine, iTasks._Framework.IWorld, iTasks._Framework.TaskEval, iTasks._Framework.TaskStore
import iTasks._Framework.Util
import iTasks._Framework.TaskServer

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
	# help					= boolOpt "-help" opts
	# sdkOpt				= stringOpt "-sdk" opts
	# webDirsOpt		    = stringOpt "-webpublic" opts
	# webDirPaths 			= fmap (split ":") webDirsOpt
	# storeOpt		    	= stringOpt "-store" opts
	# saplOpt		    	= stringOpt "-sapl" opts
	//If -help option is given show help and stop
	| help					= show instructions world
	//Check sdkpath
	# mbSDKPath				= maybe mbSDKPath Just sdkOpt //Commandline SDK option overrides found paths
	//Normal execution
	# world					= show (running port) world
	# iworld				= initIWorld mbSDKPath webDirPaths storeOpt saplOpt world
    //Reset connectedTo for all task instances
    # iworld                = clearConnections iworld
	// mark all instance as outdated initially
    # iworld                = queueAllPersistent iworld
    //Start task server
	# iworld				= serve port (httpServer port keepalive (engine publishable) taskOutput) [BackgroundTask removeOutdatedSessions,BackgroundTask refreshTaskInstances] timeout iworld
	= finalizeIWorld iworld
where
	infoline :: !String -> [String]
	infoline app	= ["*** " +++ app +++ " HTTP server ***",""]
	
	instructions :: [String]
	instructions =
		["Available commandline options:"
		," -help             : Show this message and exit"
		," -sdk <path>       : Use <path> as location of the iTasks SDK (optional)"
		," -webpublic <path> : Use <path> to point to the folders that contain the application's static web content"
	    ," -store <path> 	 : Use <path> as data store location"
	    ," -sapl <path> 	 : Use <path> to point to the folders that hold the sapl version of the application"
		," -port <port>      : Set port number (default " +++ toString DEFAULT_PORT +++ ")"
		," -keepalive <time> : Set connection keepalive time in seconds (default " +++ toString DEFAULT_KEEPALIVE_TIME +++ ")"
		,""
		]

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
    timeout iworld = (Just 25, iworld)					//Run 40 times a second, using blocking behaviour

    //Read the content of the master instance index on disk to the "ti" field in the iworld
    clearConnections :: !*IWorld -> *IWorld
    clearConnections iworld = snd (modify clear (sdsFocus filter filteredInstanceIndex) iworld)
    where
        //When the server starts we make sure all have a blank connectedTo field
        filter = {InstanceFilter|defaultValue & includeProgress = True}
        clear index = [(n,c,Just {InstanceProgress|p & connectedTo = Nothing},a) \\(n,c,Just p,a) <-index]

queueAllPersistent :: !*IWorld -> *IWorld
queueAllPersistent iworld
    # (mbIndex,iworld) = read (sdsFocus {InstanceFilter|defaultValue & onlySession=Just False} filteredInstanceIndex) iworld
    = case mbIndex of
        Ok index    = queueRefresh [instanceNo \\ (instanceNo,_,_,_)<- index] [] iworld
        _           = iworld

refreshTaskInstances :: !*IWorld -> *IWorld
refreshTaskInstances iworld
	# iworld			= updateClocks iworld
    = case dequeueRefresh iworld of
        (Just instanceNo,mbReason,iworld)   = refreshTaskInstance instanceNo mbReason iworld
        (_,_,iworld)                        = iworld

removeOutdatedSessions :: !*IWorld -> *IWorld
removeOutdatedSessions iworld
    # (mbIndex,iworld) = read (sdsFocus {InstanceFilter|defaultValue & onlySession=Just True,includeProgress=True} filteredInstanceIndex) iworld
    = case mbIndex of
        Ok index    = foldr removeIfOutdated iworld index
        _           = iworld
where
    removeIfOutdated (instanceNo,_,Just {InstanceProgress|connectedTo,lastIO},_) iworld=:{clocks={localDate,localTime}}
        | connectedTo=:Nothing && maybe True (\t -> ((DateTime localDate localTime) - t) > SESSION_TIMEOUT) lastIO
            = deleteTaskInstance instanceNo iworld
        | otherwise
            = iworld

//HACK FOR RUNNING BACKGROUND TASKS ON A CLIENT
background :: !*IWorld -> *IWorld
background iworld = removeOutdatedSessions (refreshTaskInstances iworld)

// The iTasks engine consist of a set of HTTP request handlers
engine :: publish -> [(!String -> Bool
					  ,!Bool
					  ,!(HTTPRequest (Map InstanceNo [UIUpdate]) *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
					  ,!(HTTPRequest (Map InstanceNo [UIUpdate]) (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
					  ,!(HTTPRequest (Map InstanceNo [UIUpdate]) ConnectionType *IWorld -> (!Maybe (Map InstanceNo [UIUpdate]), !*IWorld))
					  )] | Publishable publish
engine publishable
	= taskHandlers (publishAll publishable) ++ defaultHandlers
where
	taskHandlers published
		= [let (matchF,reqF,dataF,disconnectF) = webService url task defaultFormat in (matchF,True,reqF,dataF,disconnectF)
		  \\ {url,task=TaskWrapper task,defaultFormat} <- published]	
	
	defaultHandlers = [sdsService, simpleHTTPResponse (const True, handleStaticResourceRequest)]

initIWorld :: !(Maybe FilePath) !(Maybe [FilePath]) !(Maybe FilePath) !(Maybe FilePath) !*World -> *IWorld
initIWorld mbSDKPath mbWebdirPaths mbStorePath mbSaplPath world
	# (appName,world) 			= determineAppName world
	# (appPath,world)			= determineAppPath world
	# appDir					= takeDirectory appPath
	# dataDir					= case mbStorePath of
		Just path 				= path	
		Nothing 				= appDir </> appName +++ "-data"
	# (webdirPaths,world) 	 	= case mbWebdirPaths of
		Just paths 				= (paths,world)
		Nothing 
			# appWebDirs = [appDir </> "WebPublic"]
			= case mbSDKPath of 
				Just sdkDir	//Scan extensions for public web files
					# (libWebDirs,world) = determineWebPublicDirs (sdkDir </>"Server"</>"iTasks"</>"API"</>"Extensions") world
					= ([sdkDir</>"Client"] ++ appWebDirs ++ libWebDirs,world)	
				Nothing
					= (appWebDirs,world)
    # (customCSS,world)    = checkCustomCSS appName webdirPaths world 
	# saplPath = case mbSaplPath of
		Just path 	= path
		Nothing 	= appDir</>"sapl"
	# flavourPath = case mbSDKPath of
		Just sdkPath 	= sdkPath </> "Dependencies" </> "SAPL" </>"clean.f"
		Nothing 		= saplPath </> "clean.f"
	# (res,world)				= getFileInfo appPath world
	| isError res				= abort "Cannot get executable info."
	# tm						= (fromOk res).lastModifiedTime
	# build						= strfTime "%Y%m%d-%H%M%S" tm
	# (DateTime localDate localTime,world)	= currentLocalDateTimeWorld world
	# (DateTime utcDate utcTime,world)	    = currentUTCDateTimeWorld world
	# (_,world)					= ensureDir "data" dataDir world
	# tmpDir					= dataDir </> "tmp"
	# (_,world)					= ensureDir "tmp" tmpDir world
	# storeDir					= dataDir </> "stores"
	# (exists,world)			= ensureDir "stores" storeDir world
	# ((lst, ftmap, _), world)  = generateLoaderState [saplPath] [] JS_COMPILER_EXCLUDES world
	# (flavour, world)			= readFlavour flavourPath world
	# (Timestamp seed, world)	= time world
	= {IWorld
	  |server =
        {serverName = appName
	    ,serverURL	= "//127.0.0.1:80"
	    ,buildID	= build
        ,paths      =
            {appDirectory		    = appDir
	        ,dataDirectory		    = dataDir
            ,publicWebDirectories   = webdirPaths 
            }
        ,customCSS  = customCSS 
        }
	  ,config				= initialConfig
      ,clocks =
        {SystemClocks
        |localDate=localDate
        ,localTime=localTime
        ,utcDate=utcDate
        ,utcTime=utcTime
        }
      ,current =
	    {TaskEvalState
        |taskTime				= 0
	    ,taskInstance		    = 0
        ,sessionInstance        = Nothing
        ,attachmentChain        = []
	    ,nextTaskNo			    = 0
        ,eventRoute			    = newMap
        ,editletDiffs           = newMap
        }
      ,sdsNotifyRequests    = []
      ,memoryShares         = newMap
      ,cachedShares         = newMap
	  ,exposedShares		= newMap
	  ,jsCompilerState		= (lst, ftmap, flavour, Nothing, newMap)
	  ,refreshQueue			= []
	  ,shutdown				= False
      ,ioTasks              = {done = [], todo = []}
      ,ioStates             = newMap
	  ,world				= world
      ,resources            = Nothing
      ,random               = genRandInt seed
      ,onClient				= False
	  }
where
	initialConfig :: Config
	initialConfig =
		{ sessionTime		= 3600
		, smtpServer		= "localhost"
		}
		
	ensureDir :: !String !FilePath *World -> (!Bool,!*World)
	ensureDir name path world
		# (exists, world) = fileExists path world
		| exists = (True,world)
		# (res, world) = createDirectory path world
		| isError res = abort ("Cannot create " +++ name +++ " directory" +++ path +++ " : "  +++ snd (fromError res))
		= (False,world)

    readFlavour :: !String !*World -> *(!Flavour, !*World)
    readFlavour flavourPath world
	    # (flavres, world) 	= readFile flavourPath world
	    | isError flavres
		    = abort ("JavaScript Flavour file cannot be found at " +++ flavourPath)
	    # mbFlav 			= toFlavour (fromOk flavres)
	    | isNothing mbFlav
		    = abort "Error in JavaScript flavour file"	
	    = (fromJust mbFlav, world)

finalizeIWorld :: !*IWorld -> *World
finalizeIWorld iworld=:{IWorld|world} = world

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...

handleStaticResourceRequest :: !HTTPRequest *IWorld -> (!HTTPResponse,!*IWorld)
handleStaticResourceRequest req iworld=:{IWorld|server={paths={publicWebDirectories}}}
    = serveStaticResource req publicWebDirectories iworld
where
    serveStaticResource req [] iworld
	    = (notFoundResponse req,iworld)
    serveStaticResource req [d:ds] iworld=:{IWorld|world}
	    # filename		= d +++ filePath req.HTTPRequest.req_path
	    # type			= mimeType filename
	    # (mbContent, world)	= readFile filename world
	    | isOk mbContent		= ({ okResponse &
	    							 rsp_headers = [("Content-Type", type),
												    ("Content-Length", toString (size (fromOk mbContent)))]
							   	   , rsp_data = fromOk mbContent}, {IWorld|iworld & world = world})
        | otherwise
            = serveStaticResource req ds {IWorld|iworld & world = world}

	//Translate a URL path to a filesystem path
	filePath path	= ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) path
	mimeType path	= extensionToMimeType (takeExtension path)

simpleHTTPResponse ::
	(!(String -> Bool),HTTPRequest *IWorld -> (!HTTPResponse,*IWorld))
	->
	(!(String -> Bool),!Bool,!(HTTPRequest (Map InstanceNo [UIUpdate]) *IWorld -> (HTTPResponse, Maybe loc, Maybe (Map InstanceNo [UIUpdate]) ,*IWorld))
							,!(HTTPRequest (Map InstanceNo [UIUpdate]) (Maybe {#Char}) loc *IWorld -> (![{#Char}], !Bool, loc, Maybe (Map InstanceNo [UIUpdate]) ,!*IWorld))
							,!(HTTPRequest (Map InstanceNo [UIUpdate]) loc *IWorld -> (!Maybe (Map InstanceNo [UIUpdate]),!*IWorld)))
simpleHTTPResponse (pred,responseFun) = (pred,True,initFun,dataFun,lostFun)
where
	initFun req _ env
		# (rsp,env) = responseFun req env
		= (rsp,Nothing,Nothing,env)
		
	dataFun _ _ _ s env = ([],True,s,Nothing,env)
	lostFun _ _ s env = (Nothing,env)


publish :: String ServiceFormat (HTTPRequest -> Task a) -> PublishedTask | iTask a
publish url format task = {url = url, task = TaskWrapper task, defaultFormat = format}

instance Publishable (Task a) | iTask a
where
	publishAll task = [publish "/" (WebApp []) (\_ -> task)]

instance Publishable (HTTPRequest -> Task a) | iTask a
where
	publishAll task = [publish "/" (WebApp []) task]
	
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
        _   = ([],world)
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

checkCustomCSS :: !String ![FilePath] !*World -> (!Bool, !*World)
checkCustomCSS appName [] world = (False,world)
checkCustomCSS appName [d:ds] world 
	# (exists,world) = fileExists (d </> addExtension appName "css") world
	| exists 	= (True,world)
				= checkCustomCSS appName ds world
