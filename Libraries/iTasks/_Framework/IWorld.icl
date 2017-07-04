implementation module iTasks._Framework.IWorld

from System.FilePath				import :: FilePath
from Data.Map						import :: Map
from Data.Maybe						import :: Maybe
from Data.Error 					import :: MaybeError(..), :: MaybeErrorString(..)
from System.Time					import :: Timestamp, time
from Text.JSON						import :: JSONNode
from iTasks.WF.Definition           import :: TaskId, :: InstanceNo, :: TaskNo 
from iTasks.WF.Combinators.Core     import :: TaskListItem, :: ParallelTaskType
from iTasks.API.Extensions.DateTime import :: Time, :: Date, :: DateTime, toTime, toDate
from iTasks._Framework.TaskEval     import :: TaskTime
from iTasks._Framework.IWorld       import :: Config
from System.Process                 import :: ProcessHandle, :: ProcessIO

from StdFile import class FileSystem(..)
from StdFile import instance FileSystem World
from StdFunc import const, o, seqList, :: St
from StdMisc import abort
from StdOrdList import sortBy

from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

import System.Time, StdList, Text.Encodings.Base64, _SystemArray, StdBool, StdTuple, Text.JSON, Data.Error, Math.Random
import iTasks._Framework.TaskStore, iTasks._Framework.Util
import iTasks._Framework.Serialization
import iTasks._Framework.SDS
import qualified Data.Map as DM
import Data.Func, Data.Tuple, Data.List


import System.Time, System.CommandLine, System.Environment, System.OSError, System.File, System.FilePath, System.Directory

from Data.Set import :: Set, newSet
from Sapl.Linker.LazyLinker import generateLoaderState, :: LoaderStateExt, :: LoaderState, :: FuncTypeMap, :: LineType
from Sapl.Linker.SaplLinkerShared import :: SkipSet
from Sapl.Target.Flavour import :: Flavour, toFlavour
from Sapl.Target.CleanFlavour import cleanFlavour
from Sapl.SaplParser import :: ParserState

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

createIWorld :: !String FilePath !(Maybe FilePath) !(Maybe FilePath) !(Maybe FilePath) !*World -> *IWorld
createIWorld appName appPath mbWebdirPath mbStorePath mbSaplPath world
	# appDir					= takeDirectory appPath
	# dataDir					= fromMaybe (appDir </> appName +++ "-data") mbStorePath
	# webDir                    = fromMaybe (appDir </> appName +++ "-www") mbWebdirPath 
	# saplDir                   = fromMaybe (appDir </> appName +++ "-sapl") mbSaplPath 
	# (saplDir,world)           = fallBackSaplDir appDir saplDir world
	# (res,world)				= getFileInfo appPath world
	| isError res				= abort "Cannot get executable info."
	# tm						= (fromOk res).lastModifiedTime
	# build						= strfTime "%Y%m%d-%H%M%S" tm
	# (local,world)             = currentLocalDateTimeWorld world
	# (utc,world)	            = currentUTCDateTimeWorld world
	# (_,world)					= ensureDir "data" dataDir world
	# tmpDir					= dataDir </> "tmp"
	# (_,world)					= ensureDir "tmp" tmpDir world
	# storeDir					= dataDir </> "stores"
	# (exists,world)			= ensureDir "stores" storeDir world
	# (timestamp=:(Timestamp seed), world)	= time world
	= {IWorld
	  |server =
        {serverName = appName
	    ,serverURL	= "//127.0.0.1:80"
	    ,buildID	= build
        ,paths      =
            {appDirectory   = appDir
	        ,dataDirectory  = dataDir
            ,webDirectory   = webDir
			,saplDirectory  = saplDir
            }
        }
	  ,config				= initialConfig
      ,clocks =
        {SystemClocks
		|timestamp=timestamp
        ,localDate=toDate local 
        ,localTime=toTime local
        ,utcDate=toDate utc
        ,utcTime=toTime utc
        }
      ,current =
	    {TaskEvalState
        |taskTime				= 0
	    ,taskInstance		    = 0
        ,sessionInstance        = Nothing
        ,attachmentChain        = []
	    ,nextTaskNo			    = 0
        }
      ,sdsNotifyRequests    = []
      ,memoryShares         = 'DM'.newMap
      ,cachedShares         = 'DM'.newMap
	  ,exposedShares		= 'DM'.newMap
	  ,jsCompilerState		= Nothing
	  ,shutdown				= Nothing
      ,ioTasks              = {done = [], todo = []}
      ,ioStates             = 'DM'.newMap
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

	//Temporary fallback to use "sapl" instead of "<Application name>-sapl".
    //Once everybody uses an upgraded sapl-collector-linker that creates the proper
    //directory name it can be removed
	fallBackSaplDir appDir saplDir world
		# (exists, world) = fileExists saplDir world
		| exists = (saplDir,world)
				 = (appDir </> "sapl",world)
		

initJSCompilerState :: *IWorld -> *(!MaybeErrorString (), !*IWorld)
initJSCompilerState iworld=:{IWorld|world,server={paths={appDirectory,saplDirectory}}}
	# ((lst, ftmap, _), world)  = generateLoaderState [saplDirectory] [] JS_COMPILER_EXCLUDES world
    # jsCompilerState = { loaderState = lst, functionMap = ftmap, flavour = cleanFlavour, parserState = Nothing, skipMap = 'DM'.newMap}
    = (Ok (), {iworld & jsCompilerState = Just jsCompilerState, world = world})

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

destroyIWorld :: !*IWorld -> *World
destroyIWorld iworld=:{IWorld|world} = world

iworldLocalDate :: Shared Date
iworldLocalDate = createReadWriteSDS "IWorld" "localDate" read write
where
    read _ iworld=:{IWorld|clocks={localDate}} = (Ok localDate,iworld)
    write _ localDate iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & localDate=localDate}})

iworldLocalTime :: Shared Time
iworldLocalTime = createReadWriteSDS "IWorld" "localTime" read write
where
    read _ iworld=:{IWorld|clocks={localTime}} = (Ok localTime,iworld)
    write _ localTime iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & localTime=localTime}})

iworldUTCDate :: Shared Date
iworldUTCDate = createReadWriteSDS "IWorld" "utcDate" read write
where
    read _ iworld=:{IWorld|clocks={utcDate}} = (Ok utcDate,iworld)
    write _ utcDate iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & utcDate=utcDate}})

iworldUTCTime :: Shared Time
iworldUTCTime = createReadWriteSDS "IWorld" "utcTime" read write
where
    read _ iworld=:{IWorld|clocks={utcTime}} = (Ok utcTime,iworld)
    write _ utcTime iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & utcTime=utcTime}})

iworldTimestamp :: Shared Timestamp
iworldTimestamp = createReadWriteSDS "IWorld" "timestamp" read write
where
    read _ iworld=:{IWorld|clocks={timestamp}} = (Ok timestamp,iworld)
    write _ timestamp iworld=:{IWorld|clocks} = (Ok (const True), {iworld & clocks = {clocks & timestamp=timestamp}})

//Wrapper instance for file access
instance FileSystem IWorld
where
	fopen filename mode iworld=:{IWorld|world}
		# (ok,file,world) = fopen filename mode world
		= (ok,file,{IWorld|iworld & world = world})
	fclose file iworld=:{IWorld|world}
		# (ok,world) = fclose file world
		= (ok,{IWorld|iworld & world = world})
	stdio iworld=:{IWorld|world}
		# (io,world) = stdio world
		= (io,{IWorld|iworld & world = world})
	sfopen filename mode iworld=:{IWorld|world}
		# (ok,file,world) = sfopen filename mode world
		= (ok,file,{IWorld|iworld & world = world})
