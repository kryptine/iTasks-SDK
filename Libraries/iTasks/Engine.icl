implementation module iTasks.Engine

import Data.Func
import Data.Functor
import Data.List
import Data.Queue
import Internet.HTTP
import StdEnv
import System.CommandLine
import System.Directory
import System.File
import System.FilePath
import System.GetOpt
import System.OS
import Text
import iTasks.Internal.EngineTasks
import iTasks.Internal.IWorld
import iTasks.Internal.SDS
import iTasks.Internal.SDSService
import iTasks.Internal.TaskServer
import iTasks.Internal.TaskState
import iTasks.Internal.TaskIO
import iTasks.Internal.Util
import iTasks.SDS.Sources.System
import iTasks.WF.Combinators.Common
import iTasks.WF.Definition
import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.SDS
import iTasks.WF.Tasks.System
import iTasks.WF.Derives

import qualified Data.Map as DM
import Data.Map.GenJSON

from TCPIP import :: Timeout
from StdFunc import :: St, seqList

MAX_EVENTS 		        :== 5

derive class iTask EngineOptions

doTasks :: a !*World -> *World | Startable a
doTasks startable world = doTasksWithOptions (defaultEngineCLIOptions startable) world

doTasksWithOptions :: ([String] EngineOptions -> MaybeError [String] (a,EngineOptions)) !*World -> *World | Startable a
doTasksWithOptions initFun world
	# (cli,world)                = getCommandLine world
	# (options,world)            = defaultEngineOptions world
	# mbOptions                  = initFun cli options
	| mbOptions =:(Error _)      = showErr (fromError mbOptions) (setReturnCode 1 world)
	# (startable,options)        = fromOk mbOptions
	# mbIWorld                   = createIWorld options world
	| mbIWorld =: Left _
		# (Left (err, world)) = mbIWorld
		= showErr [err] (setReturnCode 1 world)
	# (Right iworld)             = mbIWorld
	| isNothing options.distributed && options.distributedChild
		= showErr ["Conflicting options, distributedChild and distributed"] (setReturnCode 1 (destroyIWorld iworld))
	# iworld = if (hasDup (requestPaths startable))
		(iShowErr ["Warning: duplicate paths in the web tasks: " +++ join ", " ["'" +++ p +++ "'"\\p<-requestPaths startable]] iworld)
		iworld
	# iworld                     = serve (startupTasks startable options) (tcpTasks startable options) (timeout options.timeout) iworld
	= destroyIWorld iworld
where
	requestPaths startable = [path\\{path}<-webTasks startable]
	webTasks startable = [t \\ WebTask t <- toStartable startable]
	startupTasks startable options
		| options.distributedChild
			= systemTasks $ distributedTasks []
		| otherwise
			= systemTasks $ distributedTasks $ backgroundTasks userTasks
	where
		//System tasks are always executed
		systemTasks c = [systemTask (startTask flushWritesWhenIdle):c]
		//Backgroundtasks are only needed when the server is not in child mode
		backgroundTasks c
			| (webTasks startable) =: []
				= [systemTask (startTask stopOnStable):c]
			| otherwise
				= [ startTask viewWebServerInstructions
				  , systemTask (startTask removeOutdatedSessions)
				  : c]
		//DistributedTasks support distributed operation, e.g., hosting the sds service
		distributedTasks c
			| isNothing options.distributed
				= c
			| otherwise
				= [ systemTask (startTask (asyncTaskListener))
				  , systemTask (startTask (sdsServiceTask (fromJust options.distributed)))
				  :c]
		//Tasks specified by the user
		userTasks
			= [t \\ StartupTask t <- toStartable startable]

	startTask t = {StartupTask|attributes=defaultValue,task=TaskWrapper t}
	systemTask t = {StartupTask|t&attributes='DM'.put "system" (JSONBool True) t.StartupTask.attributes}

	//Only run a webserver if there are tasks that are started through the web and we are not a child
	tcpTasks startable {distributedChild,serverPort,keepaliveTime}
		| distributedChild = []
		| (webTasks startable) =: [] = []
		= [(serverPort,httpServer serverPort keepaliveTime (engineWebService (webTasks startable)) taskOutput)]

	// The iTasks engine consist of a set of HTTP Web services
	engineWebService :: [WebTask] -> [WebService (Map InstanceNo TaskOutput) (Map InstanceNo TaskOutput)]
	engineWebService webtasks =
		[taskUIService webtasks
		,documentService
		,staticResourceService [path \\ {WebTask|path} <- webtasks]
		]

defaultEngineCLIOptions :: a [String] EngineOptions -> MaybeError [String] (a, EngineOptions)
defaultEngineCLIOptions tasks [argv0:argv] defaults
	# (settings, positionals, errs) = getOpt Permute opts argv
	| not (errs =: []) = Error errs
	| not (positionals =: []) = Error ["Positional arguments not allowed"]
	= case foldl (o) id settings (Just defaults) of
		Nothing = (Error [usageInfo ("Usage " +++ argv0 +++ "[OPTIONS]") opts])
		Just settings = Ok (tasks,settings)
where
	opts :: [OptDescr ((Maybe EngineOptions) -> Maybe EngineOptions)]
	opts =
		[ Option ['?'] ["help"] (NoArg (\_->Nothing))
			"Display this message"
		, Option ['p'] ["port"] (ReqArg (\p->fmap \o->{o & serverPort=toInt p}) "PORT")
			("Specify the HTTP port (default: " +++ toString defaults.serverPort +++ ")")
		, Option [] ["timeout"] (OptArg (\mp->fmap \o->{o & timeout=fmap toInt mp}) "MILLISECONDS")
			"Specify the timeout in ms (default: 500)\nIf not given, use an indefinite timeout."
		, Option [] ["allowed-hosts"] (ReqArg (\p->fmap \o->{o & allowedHosts = if (p == "") [] (split "," p)}) "IPADRESSES")
			("Specify a comma separated white list of hosts that are allowed to connected to this application\ndefault: "
			 +++ join "," defaults.allowedHosts)
		, Option [] ["keepalive"] (ReqArg (\p->fmap \o->{o & keepaliveTime={tv_sec=toInt p,tv_nsec=0}}) "SECONDS")
			"Specify the keepalive time in seconds (default: 300)"
		, Option [] ["maxevents"] (ReqArg (\p->fmap \o->{o & maxEvents=toInt p}) "NUM")
			"Specify the maximum number of events to process per loop (default: 5)"
		, Option [] ["sessiontime"] (ReqArg (\p->fmap \o->{o & sessionTime={tv_sec=toInt p,tv_nsec=0}}) "SECONDS")
			"Specify the expiry time for a session in seconds (default: 60)"
		, Option [] ["autolayout"] (NoArg (fmap \o->{o & autoLayout=True}))
			"Enable autolayouting (default)"
		, Option [] ["no-autolayout"] (NoArg (fmap \o->{o & autoLayout=False}))
			"Disable autolayouting"
		, Option [] ["persist-tasks"] (NoArg (fmap \o->{o & persistTasks=True}))
			"Enable the persistence of tasks"
		, Option [] ["no-persist-tasks"] (NoArg (fmap \o->{o & persistTasks=False}))
			"Disable the persistence of tasks (default)"
		, Option [] ["webdir"] (ReqArg (\p->fmap \o->{o & webDirPath=p}) "PATH")
			("Specify the folder containing static web content\ndefault: " +++ defaults.webDirPath)
		, Option [] ["storedir"] (ReqArg (\p->fmap \o->{o & storeDirPath=p}) "PATH")
			("Specify the folder containing the data stores\ndefault: " +++ defaults.storeDirPath)
		, Option [] ["tempdir"] (ReqArg (\p->fmap \o->{o & tempDirPath=p}) "PATH")
			("Specify the folder containing the temporary files\ndefault: " +++ defaults.tempDirPath)
		, Option [] ["bytecodepath"] (ReqArg (\p->fmap \o->{o & byteCodePath=p}) "PATH")
			("Specify the app's bytecode file\ndefault: " +++ defaults.byteCodePath)
		, Option [] ["distributed"] (ReqArg (\p->fmap \o->{o & distributed=Just (toInt p)}) "PORT")
			"Enable distributed mode (start the sds and task service)"
		, Option [] ["no-distributed"] (NoArg (fmap \o->{o & distributed=Nothing}))
			"Disable distributed mode (start the sds and task service)"
		, Option [] ["distributedChild"] (ReqArg (\p->fmap \o->{o & distributedChild=True, distributed=Just (toInt p)}) "PORT")
			"Enable distributed child mode (sds and task service)"
		, Option [] ["no-distributedChild"] (NoArg (fmap \o->{o & distributedChild=False}))
			"Disable distributed child mode (sds and task service)"
		, Option ['q'] ["quiet"] (NoArg (fmap \o->{o & verboseOperation=False}))
			"Don't show diagnostic information about (browser instructions, sds server)"
		, Option ['v'] ["verbose"] (NoArg (fmap \o->{o & verboseOperation=True}))
			"Show diagnostic information about (browser instructions, sds server)"
		]

onStartup :: (Task a) -> StartableTask | iTask a
onStartup task = StartupTask {StartupTask|attributes = defaultValue, task = TaskWrapper task}

onRequest :: String (Task a) -> StartableTask | iTask a
onRequest path task = WebTask {WebTask|path = path, task = WebTaskWrapper (const task)}

onStartupWithAttributes :: (Task a) TaskAttributes -> StartableTask | iTask a
onStartupWithAttributes task attributes = StartupTask {StartupTask|attributes = attributes, task = TaskWrapper task}

onRequestFromRequest :: String (HTTPRequest -> Task a) -> StartableTask | iTask a
onRequestFromRequest path task = WebTask {WebTask|path = path, task = WebTaskWrapper task}

class Startable a
where
	toStartable :: !a -> [StartableTask]

instance Startable (Task a) | iTask a //Default as web task
where
	toStartable task = [onRequest "/" task]

instance Startable (HTTPRequest -> Task a) | iTask a //As web task
where
	toStartable task = [onRequestFromRequest "/" task]

instance Startable StartableTask
where
	toStartable task = [task]

instance Startable [StartableTask]
where
	toStartable list = list

instance Startable (a,b) | Startable a & Startable b
where
	toStartable (x,y) = toStartable x ++ toStartable y

viewWebServerInstructions :: Task ()
viewWebServerInstructions
	=   get applicationOptions
	>>- \{EngineOptions|appName,serverPort,verboseOperation}
		| verboseOperation ->
			traceValue (join OS_NEWLINE
				["*** " +++ appName +++ " HTTP server ***"
				,""
				,"Running at http://localhost" +++
					if (serverPort == 80)
						"/"
						(":" +++ toString serverPort +++ "/")
				]) @! ()
		| otherwise -> treturn ()

defaultEngineOptions :: !*World -> (!EngineOptions,!*World)
defaultEngineOptions world
	# (appPath,world)    = determineAppPath world
	# (appVersion,world) = determineAppVersion appPath world
	# appDir             = takeDirectory appPath
	# appName            = (if (takeExtension appPath == "exe") dropExtension id o dropDirectory) appPath
	# options =
		{ appName          = appName
		, appPath          = appPath
		, appVersion       = appVersion
		, serverPort       = IF_POSIX_OR_WINDOWS 8080 80
		, serverUrl        = "http://localhost/"
		, allowedHosts     = ["127.0.0.1"]
		, keepaliveTime    = {tv_sec=300,tv_nsec=0} // 5 minutes
		, sessionTime      = {tv_sec=60,tv_nsec=0}  // 1 minute, (the client pings every 10 seconds by default)
		, persistTasks     = False
		, autoLayout       = True
		, maxEvents        = 5
		, distributed      = Nothing
		, distributedChild = False
		, timeout          = Nothing//Just 500
		, webDirPath       = appDir </> appName +++ "-www"
		, storeDirPath     = appDir </> appName +++ "-data" </> "stores"
		, tempDirPath      = appDir </> appName +++ "-data" </> "tmp"
		, byteCodePath     = appDir </> appName +++ ".bc"
		, verboseOperation = True
		}
	= (options,world)

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
					(_,Ok {FileInfo | lastModifiedTime = y}) = x > y

//By default, we use the modification time of the application executable as version id
determineAppVersion :: !FilePath!*World -> (!String,!*World)
determineAppVersion appPath world
	# (res,world)      = getFileInfo appPath world
	| res =: (Error _) = ("unknown",world)
	# ts               = timespecToStamp (fromOk res).lastModifiedTime
	# (tm, world)      = toLocalTime ts world
	# version           = strfTime "%Y%m%d-%H%M%S" tm
	= (version,world)

timeout :: !(Maybe Timeout) !*IWorld -> (!Maybe Timeout,!*IWorld)
timeout mt iworld = case read taskEvents EmptyContext iworld of
	//No events
	(Ok (ReadingDone (Queue [] [])),iworld=:{sdsNotifyRequests,world})
		# (ts, world) = nsTime world
		= ( minListBy lesser [mt:flatten (map (getTimeoutFromClock ts) ('DM'.elems sdsNotifyRequests))]
		  , {iworld & world = world})
	(Ok (ReadingDone (Queue _ _)), iworld)               = (Just 0,iworld)   //There are still events, don't wait
	(Error _,iworld)            = (Just 500,iworld) //Keep retrying, but not too fast
where
	lesser (Just x) (Just y) = x < y
	lesser (Just _) Nothing  = True
	lesser _        _        = False

	getTimeoutFromClock :: Timespec (Map SDSNotifyRequest Timespec) -> [Maybe Timeout]
	getTimeoutFromClock now requests = map getTimeoutFromClock` ('DM'.toList requests)
	where
		getTimeoutFromClock` :: (!SDSNotifyRequest, !Timespec) -> Maybe Timeout
		getTimeoutFromClock` (snr, reqTimespec)
			| dependsOnShareWithName "IWorld:timespec" snr.reqSDSId = case snr.cmpParam of
				(ts :: ClockParameter Timespec) | ts.interval <> zero
					# fire = iworldTimespecNextFire now reqTimespec ts
					= Just (max 0 (toMs fire - toMs now))
				_
					= mt
			| otherwise
				= mt

	toMs x = x.tv_sec * 1000 + x.tv_nsec / 1000000
