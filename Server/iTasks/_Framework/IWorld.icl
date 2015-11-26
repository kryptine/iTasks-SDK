implementation module iTasks._Framework.IWorld

from System.FilePath				import :: FilePath
from Data.Map						import :: Map
from Data.Maybe						import :: Maybe
from Data.Error 					import :: MaybeError(..), :: MaybeErrorString(..)
from System.Time					import :: Timestamp, time
from Text.JSON						import :: JSONNode
from iTasks.API.Core.Types	        import :: DateTime, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime

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

createIWorld :: !String !(Maybe FilePath) !(Maybe [FilePath]) !(Maybe FilePath) !(Maybe FilePath) !*World -> *IWorld
createIWorld appName mbSDKPath mbWebdirPaths mbStorePath mbSaplPath world
	# (appPath,world)			= determineAppPath world
	# appDir					= takeDirectory appPath
	# dataDir					= case mbStorePath of
		Just path 				= path	
		Nothing 				= appDir </> appName +++ "-data"
	# saplDir = case mbSaplPath of
		Just path 	= path
		Nothing 	= appDir </>"sapl"
	# flavourPath = case mbSDKPath of
		Just sdkPath 	= sdkPath </> "Dependencies" </> "clean-sapl" </> "src" </>"clean.f"
		Nothing 		= saplDir </> "clean.f"
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
			,saplDirectory 			= saplDir
			,saplFlavourFile 		= flavourPath
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
        ,eventRoute			    = 'DM'.newMap
        }
      ,sdsNotifyRequests    = []
      ,memoryShares         = 'DM'.newMap
      ,cachedShares         = 'DM'.newMap
	  ,exposedShares		= 'DM'.newMap
	  ,jsCompilerState		= Nothing
	  ,shutdown				= False
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

initJSCompilerState :: *IWorld -> *(!MaybeErrorString (), !*IWorld)
initJSCompilerState iworld=:{IWorld|world,server={paths={appDirectory,saplDirectory,saplFlavourFile}}}
	# ((lst, ftmap, _), world)  = generateLoaderState [saplDirectory] [] JS_COMPILER_EXCLUDES world
	= case readFlavour saplFlavourFile world of
		(Ok flavour,world)
 			# jsCompilerState =
				{ loaderState = lst
				, functionMap = ftmap
				, flavour 		= flavour
				, parserState = Nothing
				, skipMap = 'DM'.newMap
				}
			= (Ok (),{iworld & jsCompilerState = Just jsCompilerState, world = world})
		(Error e,world)
			= (Error e,{iworld & world = world})
where
    readFlavour :: !String !*World -> *(!MaybeErrorString Flavour, !*World)
    readFlavour flavourPath world
	    # (flavRes, world) 	= readFile flavourPath world
		= case readFile flavourPath world of
			(Error e,world) = (Error ("JavaScript Flavour file could not be read: " +++ toString e),world)
			(Ok flavFile,world)
				= case toFlavour flavFile of
					Nothing      = (Error "Error in JavaScript flavour file",world)
					Just flavour = (Ok flavour,world)

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

updateClocks :: !*IWorld -> *IWorld
updateClocks iworld=:{IWorld|clocks,world}
    //Determine current date and time
	# (DateTime localDate localTime,world)		= currentLocalDateTimeWorld world
	# (DateTime utcDate utcTime,world)			= currentUTCDateTimeWorld world
    # iworld = {iworld & world = world}
    //Write SDS's if necessary
    # iworld = if (localDate == clocks.localDate) iworld (snd (write localDate iworldLocalDate iworld))
    # iworld = if (localTime == clocks.localTime) iworld (snd (write localTime iworldLocalTime iworld))
    # iworld = if (utcDate == clocks.utcDate) iworld (snd (write utcDate iworldUTCDate iworld))
    # iworld = if (utcTime == clocks.utcTime) iworld (snd (write utcTime iworldUTCTime iworld))
    = iworld

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
