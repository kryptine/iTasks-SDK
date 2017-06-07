implementation module iTasks._Framework.Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool, StdEnum
from StdFunc import o, seqList, ::St, const
from Data.Map import :: Map
from Data.Queue import :: Queue(..)
import qualified Data.Map as DM
import Data.List, Data.Error, Data.Func, Data.Tuple, Math.Random, Text 
import System.Time, System.CommandLine, System.Environment, System.OSError, System.File, System.FilePath, System.Directory
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil
import iTasks._Framework.IWorld, iTasks._Framework.WebService, iTasks._Framework.SDSService
import iTasks.API.Common.SDSCombinators
import qualified iTasks._Framework.SDS as SDS
import iTasks.UI.Layout, iTasks.UI.Layout.Default
from iTasks.API.Core.TaskCombinators import class tune(..)
from iTasks.UI.Layout import instance tune ApplyLayout


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

show :: ![String] !*World -> *World
show lines world
	# (console,world)	= stdio world
	# console			= seqSt (\s c -> fwrites (s +++ "\n") c) lines console
	# (_,world)			= fclose console world
	= world

getServerOptions :: !*World -> (!Maybe ServerOptions,!*World)
getServerOptions world
	# (opts,world)			= getCommandLine world
	# (appName,world)		= determineAppName world
	# (appPath,world)		= determineAppPath world	
	//Check commandline options
	# port 					= fromMaybe DEFAULT_PORT (intOpt "-port" opts)
	# keepalive				= fromMaybe DEFAULT_KEEPALIVE_TIME (intOpt "-keepalive" opts)
	# help					= boolOpt "-help" opts
	# webOpt				= stringOpt "-webpublic" opts
	# storeOpt		    	= stringOpt "-store" opts
	# saplOpt		    	= stringOpt "-sapl" opts
	//If -help option is given show help and stop
	| help					= (Nothing, show instructions world)
	# options =	
		{ appName			= appName
		, appPath			= appPath
		, serverPort		= port
		, keepalive 		= keepalive
		, webDirPath 		= webOpt
		, storeDirPath      = storeOpt
		, saplDirPath 	    = saplOpt
		}
	= (Just options,world)
where
	instructions :: [String]
	instructions =
		["Available commandline options:"
		," -help             : Show this message and exit"
		," -webpublic <path> : Use <path> to point to the folder that contain the application's static web content"
	    ," -store <path> 	 : Use <path> as data store location"
	    ," -sapl <path> 	 : Use <path> to point to the folders that hold the sapl version of the application"
		," -port <port>      : Set port number (default " +++ toString DEFAULT_PORT +++ ")"
		," -keepalive <time> : Set connection keepalive time in seconds (default " +++ toString DEFAULT_KEEPALIVE_TIME +++ ")"
		,""
		]

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

startEngine :: a !*World -> *World | Publishable a
startEngine publishable world
	= case getServerOptions world of
		(Nothing,world)      = world
		(Just options,world) = startEngineWithOptions publishable options world

startEngineWithOptions :: a ServerOptions !*World -> *World | Publishable a
startEngineWithOptions publishable options=:{appName,appPath,serverPort,keepalive,webDirPath,storeDirPath,saplDirPath} world
	# world					= show (running appName serverPort) world
 	# iworld				= createIWorld appName appPath webDirPath storeDirPath saplDirPath world
 	# (res,iworld) 			= initJSCompilerState iworld
 	| res =:(Error _) 		= show ["Fatal error: " +++ fromError res] (destroyIWorld iworld)
    //Start task server
	# iworld				= serve [] [(serverPort,httpServer serverPort keepalive (engine publishable) allUIChanges)] backgroundTasks timeout iworld
	= destroyIWorld iworld
where
	running :: !String !Int -> [String]
	running app port = ["*** " +++ app +++ " HTTP server ***"
                       ,""
                       ,"Running at http://localhost" +++ (if (port == 80) "/" (":" +++ toString port +++ "/"))]

runTasks :: a !*World -> *World | Runnable a
runTasks tasks world
	= case getServerOptions world of
		(Nothing,world)      = world
		(Just options,world) = runTasksWithOptions tasks options world

runTasksWithOptions :: a ServerOptions !*World -> *World | Runnable a
runTasksWithOptions runnable options=:{appName,appPath,serverPort,keepalive,webDirPath,storeDirPath,saplDirPath} world
 	# iworld				= createIWorld appName appPath webDirPath storeDirPath saplDirPath world
 	# (res,iworld) 			= initJSCompilerState iworld
 	| res =:(Error _) 		= show ["Fatal error: " +++ fromError res] (destroyIWorld iworld)
	# iworld				= serve (toRunnable runnable) [] backgroundTasks timeout iworld
	= destroyIWorld iworld

backgroundTasks =
 	[BackgroundTask updateClocks
	,BackgroundTask (processEvents MAX_EVENTS)
	,BackgroundTask removeOutdatedSessions]

timeout :: !*IWorld -> (!Maybe Timeout,!*IWorld)
timeout iworld = case 'SDS'.read taskEvents iworld of //Check if there are events in the queue
	(Ok (Queue [] []),iworld)   = (Just 10,iworld) //Empty queue, don't waste CPU, but refresh
	(Ok _,iworld)               = (Just 0,iworld)   //There are still events, don't wait
	(Error _,iworld)            = (Just 500,iworld) //Keep retrying, but not too fast

updateClocks :: !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateClocks iworld=:{IWorld|clocks,world}
    //Determine current date and time
	# (timestamp,world) 	= time world
	# (local,world)	= currentLocalDateTimeWorld world
	# localDate = toDate local
	  localTime = toTime local 
	# (utc,world)	= currentUTCDateTimeWorld world
	# utcDate = toDate utc
	  utcTime = toTime utc 
    # iworld = {iworld & world = world}
    //Write SDS's if necessary
    # (mbe,iworld) = if (localDate == clocks.localDate) (Ok (),iworld) (write localDate iworldLocalDate iworld)
	| mbe =:(Error _) = (mbe,iworld)
    # (mbe,iworld) = if (localTime == clocks.localTime) (Ok (),iworld) (write localTime iworldLocalTime iworld)
	| mbe =:(Error _) = (mbe,iworld)
    # (mbe,iworld) = if (utcDate == clocks.utcDate) (Ok (),iworld) (write utcDate iworldUTCDate iworld)
	| mbe =:(Error _) = (mbe,iworld)
    # (mbe,iworld) = if (utcTime == clocks.utcTime) (Ok (),iworld) (write utcTime iworldUTCTime iworld)
	| mbe =:(Error _) = (mbe,iworld)
    # (mbe,iworld) = if (timestamp == clocks.timestamp) (Ok (),iworld) (write timestamp iworldTimestamp iworld)
	| mbe =:(Error _) = (mbe,iworld)
    = (Ok (),iworld)

removeOutdatedSessions :: !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
removeOutdatedSessions iworld
    # (mbIndex,iworld) = read (sdsFocus {InstanceFilter|defaultValue & onlySession=Just True} filteredInstanceIndex) iworld
    = case mbIndex of
        Ok index    = (Ok (), foldr removeIfOutdated iworld index)
        Error e     = (Error e, iworld)
where
    removeIfOutdated (instanceNo,_,_,_) iworld=:{clocks={localDate,localTime}}
		//Get lastIO time
		= case read (sdsFocus instanceNo taskInstanceIO) iworld of
			(Ok (Just (client,time)),iworld) //No IO for too long, clean up
				# (Timestamp tInstance) = datetimeToTimestamp time
				  (Timestamp tNow) = datetimeToTimestamp (toDateTime localDate localTime)
				| (tNow - tInstance) > SESSION_TIMEOUT
					# (_,iworld) = deleteTaskInstance instanceNo iworld
					# (_,iworld) = 'SDS'.write Nothing (sdsFocus instanceNo taskInstanceIO) iworld
					= iworld
				| otherwise
					= iworld
			(Ok Nothing,iworld) = iworld
			(Error e,iworld) = iworld


//HACK FOR RUNNING BACKGROUND TASKS ON A CLIENT
background :: !*IWorld -> *IWorld
background iworld
	# iworld = snd (processEvents MAX_EVENTS iworld)
	# iworld = snd (removeOutdatedSessions iworld)
	= iworld

// The iTasks engine consist of a set of HTTP WebService 
engine :: publish -> [(!String -> Bool
					  ,!Bool
					  ,!(HTTPRequest (Map InstanceNo (Queue UIChange)) *IWorld -> (!HTTPResponse,!Maybe ConnectionState, !Maybe (Map InstanceNo (Queue UIChange)), !*IWorld))
					  ,!(HTTPRequest (Map InstanceNo (Queue UIChange)) (Maybe {#Char}) ConnectionState *IWorld -> (![{#Char}], !Bool, !ConnectionState, !Maybe (Map InstanceNo (Queue UIChange)), !*IWorld))
					  ,!(HTTPRequest (Map InstanceNo (Queue UIChange)) ConnectionState *IWorld -> (!Maybe (Map InstanceNo (Queue UIChange)), !*IWorld))
					  )] | Publishable publish

engine publishable = [taskUIService published, documentService, sdsService, staticResourceService [url \\ {PublishedTask|url} <- published]]
where
	published = publishAll publishable 

publish :: String (HTTPRequest -> Task a) -> PublishedTask | iTask a
publish url task = {url = url, task = WebTaskWrapper (withFinalSessionLayout task)}

withFinalSessionLayout :: (HTTPRequest -> Task a) -> (HTTPRequest -> Task a) | iTask a
withFinalSessionLayout taskf = \req -> tune (ApplyLayout defaultSessionLayout) (taskf req)

publishWithoutLayout :: String (HTTPRequest -> Task a) -> PublishedTask | iTask a
publishWithoutLayout url task = {url = url, task = WebTaskWrapper task}

instance Publishable (Task a) | iTask a
where
	publishAll task = [publish "/" (const task)]

instance Publishable (HTTPRequest -> Task a) | iTask a
where
	publishAll task = [publish "/" task]
	
instance Publishable [PublishedTask]
where
	publishAll list = list

class Runnable a
where
	toRunnable :: !a -> [TaskWrapper] 

instance Runnable (Task a) | iTask a
where
	toRunnable task = [TaskWrapper task]

instance Runnable [TaskWrapper]
where
	toRunnable list = list


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
	
