implementation module iTasks._Framework.Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool, StdEnum
from StdFunc import o, seqList, ::St, const
from Data.Map import :: Map
from Data.Queue import :: Queue(..)
import qualified Data.Map as DM
import Data.List, Data.Error, Data.Func, Data.Tuple, Math.Random, Internet.HTTP, Text, Text.Encodings.MIME, Text.Encodings.UrlEncoding
import System.Time, System.CommandLine, System.Environment, System.OSError, System.File, System.FilePath, System.Directory
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil
import iTasks._Framework.IWorld, iTasks._Framework.WebService, iTasks._Framework.SDSService
import iTasks.API.Common.SDSCombinators
import qualified iTasks._Framework.SDS as SDS
import iTasks.UI.Layout, iTasks.UI.Layout.Auto
from iTasks.API.Core.TaskCombinators import class tune(..), instance tune ApplyLayout, instance tune AutoLayout

SESSION_TIMEOUT :== fromString "0000-00-00 00:10:00"
MAX_EVENTS 		:== 5

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
	# (appName,world)		= determineAppName world
	# (mbSDKPath,world)		= determineSDKPath SEARCH_PATHS world
	// Show server name
	# world					= show (infoline appName) world
  	//Check commandline options
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
	# iworld				= createIWorld appName mbSDKPath webDirPaths storeOpt saplOpt world
	# (res,iworld) 			= initJSCompilerState iworld
	| res =:(Error _)
		= show ["Fatal error: " +++ fromError res] (destroyIWorld iworld)
    //Reset connectedTo for all task instances
    # iworld                = clearConnections iworld
	// mark all instance as outdated initially
    # iworld                = queueAllPersistent iworld
    //Run task server
	# iworld				= serve port (httpServer port keepalive (engine publishable) allUIChanges)
		[BackgroundTask removeOutdatedSessions
		,BackgroundTask updateClocks, BackgroundTask (processEvents MAX_EVENTS)] timeout iworld
	= destroyIWorld iworld
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
	timeout iworld = case 'SDS'.read taskEvents iworld of //Check if there are events in the queue
		(Ok (Queue [] []),iworld)   = (Just 100,iworld) //Empty queue, don't waste CPU, but refresh
		(Ok _,iworld)               = (Just 0,iworld)   //There are still events, don't wait
		(Error _,iworld)            = (Just 500,iworld) //Keep retrying, but not too fast

    //Read the content of the master instance index on disk to the "ti" field in the iworld
    clearConnections :: !*IWorld -> *IWorld
    clearConnections iworld = snd (modify clear (sdsFocus filter filteredInstanceIndex) iworld)
    where
        //When the server starts we make sure all have a blank connectedTo field
        filter = {InstanceFilter|defaultValue & includeProgress = True}
        clear index = ((),[(n,c,Just {InstanceProgress|p & connectedTo = Nothing},a) \\(n,c,Just p,a) <-index])

queueAllPersistent :: !*IWorld -> *IWorld
queueAllPersistent iworld
    # (mbIndex,iworld) = read (sdsFocus {InstanceFilter|defaultValue & onlySession=Just False} filteredInstanceIndex) iworld
    = case mbIndex of
        Ok index    = queueRefresh [(instanceNo,"Persistent first refresh") \\ (instanceNo,_,_,_)<- index]  iworld
        _           = iworld

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
background iworld = (processEvents MAX_EVENTS o removeOutdatedSessions) iworld

// The iTasks engine consist of a set of HTTP request handlers
engine :: publish -> [(!String -> Bool
					  ,!Bool
					  ,!(HTTPRequest (Map InstanceNo (Queue UIChangeDef)) *IWorld -> (!HTTPResponse,!Maybe ConnectionType, !Maybe (Map InstanceNo (Queue UIChangeDef)), !*IWorld))
					  ,!(HTTPRequest (Map InstanceNo (Queue UIChangeDef)) (Maybe {#Char}) ConnectionType *IWorld -> (![{#Char}], !Bool, !ConnectionType, !Maybe (Map InstanceNo (Queue UIChangeDef)), !*IWorld))
					  ,!(HTTPRequest (Map InstanceNo (Queue UIChangeDef)) ConnectionType *IWorld -> (!Maybe (Map InstanceNo (Queue UIChangeDef)), !*IWorld))
					  )] | Publishable publish
engine publishable
	= taskHandlers (publishAll publishable) ++ defaultHandlers
where
	taskHandlers published
		= [let (matchF,reqF,dataF,disconnectF) = webService url task defaultFormat in (matchF,True,reqF,dataF,disconnectF)
		  \\ {url,task=TaskWrapper task,defaultFormat} <- published]	
	
	defaultHandlers = [sdsService, simpleHTTPResponse (const True, handleStaticResourceRequest)]

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
	(!(String -> Bool),!Bool,!(HTTPRequest r *IWorld -> (HTTPResponse, Maybe loc, Maybe w ,*IWorld))
							,!(HTTPRequest r (Maybe {#Char}) loc *IWorld -> (![{#Char}], !Bool, loc, Maybe w ,!*IWorld))
							,!(HTTPRequest r loc *IWorld -> (!Maybe w,!*IWorld)))
simpleHTTPResponse (pred,responseFun) = (pred,True,initFun,dataFun,lostFun)
where
	initFun req _ env
		# (rsp,env) = responseFun req env
		= (rsp,Nothing,Nothing,env)
		
	dataFun _ _ _ s env = ([],True,s,Nothing,env)
	lostFun _ _ s env = (Nothing,env)

publish :: String ServiceFormat (HTTPRequest -> Task a) -> PublishedTask | iTask a
publish url format task = {url = url, task = TaskWrapper (withFinalSessionLayout task), defaultFormat = format}

publishRaw :: String ServiceFormat (HTTPRequest -> Task a) -> PublishedTask | iTask a
publishRaw url format task = {url = url, task = TaskWrapper (withoutLayout task), defaultFormat = format}

withFinalSessionLayout :: (HTTPRequest -> Task a) -> (HTTPRequest -> Task a) | iTask a
withFinalSessionLayout taskf = \req -> tune (ApplyLayout autoLayoutSession) (taskf req)

withoutLayout :: (HTTPRequest -> Task a) -> (HTTPRequest -> Task a) | iTask a
withoutLayout taskf = \req -> tune WithoutAutoLayout (taskf req)

instance Publishable (Task a) | iTask a
where
	publishAll task = [publish "/" (WebApp []) (const task)]

instance Publishable (HTTPRequest -> Task a) | iTask a
where
	publishAll task = [publish "/" (WebApp []) task]
	
instance Publishable [PublishedTask]
where
	publishAll list = list

// Determines the server executables name
determineAppName :: !*World -> (!String,!*World)
determineAppName world 
	# (appPath, world) = determineAppPath world
	= ((dropExtension o dropDirectory) appPath, world)

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

