implementation module iTasks.Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool, StdEnum
import iTasks.WF.Combinators.Common
import iTasks.WF.Tasks.System
from StdFunc import o, seqList, ::St, const, id
from Data.Map import :: Map
from Data.Queue import :: Queue(..)
import qualified Data.Map as DM
import Data.List, Data.Error, Data.Func, Data.Tuple, Math.Random, Text 
import System.Time, System.CommandLine, System.Environment, System.OSError, System.File, System.FilePath, System.Directory
import iTasks.Internal.Util, iTasks.Internal.HtmlUtil
import iTasks.Internal.IWorld, iTasks.Internal.WebService, iTasks.Internal.SDSService
import qualified iTasks.Internal.SDS as SDS
import iTasks.UI.Layout, iTasks.UI.Layout.Default

from iTasks.WF.Combinators.Tune import class tune(..), instance tune ApplyLayout Task, :: ApplyLayout(..)
from iTasks.SDS.Combinators.Common import sdsFocus

import StdInt, StdChar, StdString
import tcp
import Internet.HTTP, System.Time, System.CommandLine, Data.Func

import iTasks.Internal.IWorld, iTasks.Internal.TaskEval, iTasks.Internal.TaskStore
import iTasks.Internal.Util
import iTasks.Internal.TaskServer
import iTasks.Internal.EngineTasks

from iTasks.Extensions.DateTime import toDate, toTime, instance == Date, instance == Time

from Data.Set import :: Set, newSet
from Sapl.Linker.LazyLinker import generateLoaderState, :: LoaderStateExt
from Sapl.Linker.SaplLinkerShared import :: SkipSet
from Sapl.Target.Flavour import :: Flavour, toFlavour

from System.OS import IF_POSIX_OR_WINDOWS
import System.GetOpt
import Data.Functor

MAX_EVENTS 		        :== 5

defaultEngineOptions :: !*World -> (!EngineOptions,!*World)
defaultEngineOptions world
	# (appPath,world)    = determineAppPath world	
	# (appVersion,world) = determineAppVersion appPath world
	# appDir             = takeDirectory appPath
	# appName            = (dropExtension o dropDirectory) appPath
	# options =	
		{ appName			= appName
		, appPath			= appPath
        , appVersion        = appVersion
		, serverPort		= IF_POSIX_OR_WINDOWS 8080 80
        , serverUrl         = "http://localhost/"
		, keepaliveTime     = {tv_sec=300,tv_nsec=0} // 5 minutes
		, sessionTime       = {tv_sec=60,tv_nsec=0}  // 1 minute, (the client pings every 10 seconds by default)
        , persistTasks      = False
		, autoLayout        = True
		, timeout			= Just 500
		, webDirPath 		= appDir </> appName +++ "-www"
		, storeDirPath      = appDir </> appName +++ "-data" </> "stores"
		, tempDirPath       = appDir </> appName +++ "-data" </> "tmp"
		, saplDirPath 	    = appDir </> appName +++ "-sapl"
		}
	= (options,world)

defaultEngineCLIOptions :: [String] EngineOptions -> (!Maybe EngineOptions,![String])
defaultEngineCLIOptions [argv0:argv] defaults
	# (settings, positionals, errs) = getOpt Permute opts argv
	| not (errs =: []) = (Nothing, errs)
	| not (positionals =: []) = (Nothing, ["Positional arguments not allowed"])
	= case foldl (o) id settings (Just defaults) of
		Nothing = (Nothing, [usageInfo ("Usage " +++ argv0 +++ "[OPTIONS]") opts])
		Just settings = (Just settings,
			["*** " +++ settings.appName +++ " HTTP server ***"
			,""
			,"Running at http://localhost" +++ if (settings.serverPort == 80) "/" (":" +++ toString settings.serverPort +++ "/")])
where
	opts :: [OptDescr ((Maybe EngineOptions) -> Maybe EngineOptions)]
	opts =
		[ Option ['?'] ["help"] (NoArg $ const Nothing)
			"Display this message"
		, Option ['p'] ["port"] (ReqArg (\p->fmap \o->{o & serverPort=toInt p}) "PORT")
			("Specify the HTTP port (default: " +++ toString defaults.serverPort)
		, Option [] ["timeout"] (OptArg (\mp->fmap \o->{o & timeout=fmap toInt mp}) "MILLISECONDS")
			"Specify the timeout in ms (default: 500)\nIf not given, use an indefinite timeout."
		, Option [] ["keepalive"] (ReqArg (\p->fmap \o->{o & keepaliveTime={tv_sec=toInt p,tv_nsec=0}}) "SECONDS")
			"Specify the keepalive time in seconds (default: 300)"
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
		, Option [] ["sapldir"] (ReqArg (\p->fmap \o->{o & saplDirPath=p}) "PATH")
			("Specify the folder containing the sapl files\ndefault: " +++ defaults.saplDirPath)
		]

doTasks :: a !*World -> *World | Startable a
doTasks startable world = doTasksWithOptions defaultEngineCLIOptions startable world

doTasksWithOptions :: ([String] EngineOptions -> (!Maybe EngineOptions,![String])) a !*World -> *World | Startable a
doTasksWithOptions initFun startable world
	# (cli,world)			= getCommandLine world
	# (options,world)       = defaultEngineOptions world
	# (mbOptions,msg)       = initFun cli options
	# world                 = show msg world
	| mbOptions =: Nothing  = world
	# (Just options)		= mbOptions
	# iworld				= createIWorld options world
	# (res,iworld) 			= initJSCompilerState iworld
	| res =:(Error _) 		= show ["Fatal error: " +++ fromError res] (destroyIWorld iworld)
	# iworld				= serve startupTasks (tcpTasks options.serverPort options.keepaliveTime)
	                                engineTasks (timeout options.timeout) iworld
	= destroyIWorld iworld
where
    webTasks = [t \\ WebTask t <- toStartable startable]
	startupTasks = [t \\ StartupTask t <- toStartable startable]
	hasWebTasks = not (webTasks =: [])

	//Only run a webserver if there are tasks that are started through the web
	tcpTasks serverPort keepaliveTime
		| webTasks =: [] = []
		| otherwise
			= [(serverPort,httpServer serverPort keepaliveTime (engineWebService webTasks) taskOutput)]
	
	engineTasks =
 		[BackgroundTask updateClock
		,BackgroundTask (processEvents MAX_EVENTS)
		:if (webTasks =: [])
			[BackgroundTask removeOutdatedSessions
		 	,BackgroundTask flushWritesWhenIdle
			]
			[BackgroundTask stopOnStable]
		]

// The iTasks engine consist of a set of HTTP Web services
engineWebService :: [WebTask] -> [WebService (Map InstanceNo TaskOutput) (Map InstanceNo TaskOutput)] 
engineWebService webtasks =
	[taskUIService webtasks
	,documentService
	,sdsService
	,staticResourceService [url \\ {WebTask|url} <- webtasks]
	]

show :: ![String] !*World -> *World
show lines world
	# (console,world)	= stdio world
	# console			= seqSt (\s c -> fwrites (s +++ "\n") c) lines console
	# (_,world)			= fclose console world
	= world

atRequest :: String (HTTPRequest -> Task a) -> StartableTask | iTask a
atRequest url task = WebTask {WebTask|url = url, task = WebTaskWrapper task}

atStartup :: TaskAttributes (Task a) -> StartableTask | iTask a
atStartup attributes task = StartupTask {StartupTask|attributes = attributes, task = TaskWrapper task}

class Startable a
where
	toStartable :: !a -> [StartableTask]

instance Startable (Task a) | iTask a //Default as web task
where
	toStartable task = [atRequest "/" (const task)]

instance Startable (HTTPRequest -> Task a) | iTask a //As web task
where
	toStartable task = [atRequest "/" task]

instance Startable StartableTask
where
	toStartable task = [task]

instance Startable [StartableTask]
where
	toStartable list = list

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
					(_,Ok {FileInfo | lastModifiedTime = y}) = timeGm x > timeGm y

//By default, we use the modification time of the applaction executable as version id
determineAppVersion :: !FilePath!*World -> (!String,!*World)	
determineAppVersion appPath world
	# (res,world)       = getFileInfo appPath world
	| res =: (Error _)  = ("unknown",world) 
	# tm				= (fromOk res).lastModifiedTime
	# version           = strfTime "%Y%m%d-%H%M%S" tm
	= (version,world)

