implementation module iTasks.Internal.IWorld

import StdEnv
from StdFunc import seqList, :: St

from ABC.Interpreter import prepare_prelinked_interpretation, :: PrelinkedInterpretationEnvironment
from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

import Data.Func
from Data.Map import :: Map
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe
import Math.Random
import System.CommandLine
import System.Directory
import System.File
import System.FilePath
import System.Signal

import iTasks.Engine
import iTasks.Extensions.DateTime
import iTasks.Internal.Task
import iTasks.Internal.TaskEval
import iTasks.Internal.Util
import iTasks.SDS.Combinators.Common
import iTasks.SDS.Combinators.Core
import iTasks.WF.Definition
import iTasks.WF.Derives

createIWorld :: !EngineOptions !*World -> Either (!String, !*World) *IWorld
createIWorld options world
	# (ts=:{tv_nsec=seed}, world) = nsTime world
	# (mbAbcEnv,           world) = prepare_prelinked_interpretation options.appPath options.byteCodePath world
	= case mbAbcEnv of
		Just abcEnv = Right
			{IWorld
			|options = options
			,clock = ts
			,nextTick = Nothing
			,current =
				{TaskEvalState
				|taskTime				= 0
				,taskInstance		    = 0
				,sessionInstance        = Nothing
				,attachmentChain        = []
				,nextTaskNo			    = 0
				}
			,sdsNotifyRequests    = 'DM'.newMap
			,sdsNotifyReqsByTask  = 'DM'.newMap
			,memoryShares         = 'DM'.newMap
			,readCache            = 'DM'.newMap
			,writeCache           = 'DM'.newMap
			,abcInterpreterEnv    = abcEnv
			,shutdown             = Nothing
			,ioTasks              = {done = [], todo = []}
			,ioStates             = 'DM'.newMap
			,world                = world
			,signalHandlers       = []
			,resources            = []
			,random               = genRandInt seed
			,onClient             = False
			}
		Nothing =
			Left ("Failed to parse bytecode, is ByteCode set in the project file?", world)

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

destroyIWorld :: !*IWorld -> *World
destroyIWorld iworld=:{IWorld|world} = world

//Ad hoc share to hook into the readRegister call (only used for the clock)
:: SDSRegistered p r w
	= SDSRegistered !(p *IWorld -> *(MaybeError TaskException (), *IWorld)) !(SDSSource p r w)

instance Identifiable SDSRegistered
where
	sdsIdentity (SDSRegistered _ sds) = sdsIdentity sds
instance Readable SDSRegistered
where
	readSDS (SDSRegistered _ sds) p c iworld = readSDS sds p c iworld
instance Writeable SDSRegistered
where
	writeSDS (SDSRegistered _ sds) p c w iworld = writeSDS sds p c w iworld
instance Modifiable SDSRegistered
where
	modifySDS f (SDSRegistered _ sds) p c iworld = modifySDS f sds p c iworld
instance Registrable SDSRegistered
where
	readRegisterSDS (SDSRegistered rfun sds) p c t r iworld
		= case rfun p iworld of
			(Ok _, iworld) = readRegisterSDS sds p c t r iworld
			(Error e, iworld) = (ReadException e, iworld)

iworldTimespec :: SDSRegistered (ClockParameter Timespec) Timespec Timespec
iworldTimespec =: SDSRegistered register
	$ createReadWriteSDS "IWorld" "timespec"
		(\_ iworld=:{IWorld|clock}->(Ok clock, iworld))
		(\_ ts iworld->(Ok (pred ts), {iworld & clock=ts}))
where
	//Watchers are notified when the current time exceeded the nexttick time
	pred :: !Timespec !Timespec !(ClockParameter Timespec) -> Bool
	pred ts reg p = ts >= nexttick reg p

	read :: !(ClockParameter Timespec) !*IWorld -> (!MaybeError TaskException Timespec, !*IWorld)
	read p iworld=:{IWorld|clock}
		= (Ok clock, iworld)

	write :: !(ClockParameter Timespec) !Timespec !*IWorld -> (!MaybeError TaskException (SDSNotifyPred (ClockParameter Timespec)), !*IWorld)
	write p ts iworld
		= (Ok pred, {iworld & clock=ts})

	register :: !(ClockParameter Timespec) !*IWorld -> (!MaybeError TaskException (), !*IWorld)
	register p iworld=:{IWorld|clock,nextTick}
		# nt = nexttick clock p
		= (Ok (), {iworld & nextTick=maybe (Just nt) (\ot->Just (if (nt < ot) nt ot)) nextTick})

	//The next tick is either interval+reg (when start is zero) or start+interval
	nexttick :: !Timespec !(ClockParameter Timespec) -> Timespec
	nexttick reg {start,interval} = (if (start == zero) reg start) + interval

iworldTimestamp :: SDSLens (ClockParameter Timestamp) Timestamp Timestamp
iworldTimestamp =: mapReadWrite (timespecToStamp, \w r. Just (timestampToSpec w)) (Just \_ s. Ok (timespecToStamp s)) 
	$ sdsTranslate "iworldTimestamp translation" (\{start,interval}->{start=timestampToSpec start,interval=timestampToSpec interval}) iworldTimespec

iworldLocalDateTime :: SDSParallel () DateTime ()
iworldLocalDateTime =: sdsParallel "iworldLocalDateTime"
	// ignore value, but use notifications for 'iworldTimestamp'
	(\p -> (p,p)) fst
	(SDSWriteConst \_ _ -> Ok Nothing) (SDSWriteConst \_ _ -> Ok Nothing)
	(createReadOnlySDS \_ -> iworldLocalDateTime`)
	(sdsFocus {start=Timestamp 0,interval=Timestamp 1} iworldTimestamp)

iworldLocalDateTime` :: !*IWorld -> (!DateTime, !*IWorld)
iworldLocalDateTime` iworld=:{clock={tv_sec}, world}
	# (tm, world) = toLocalTime (Timestamp tv_sec) world
	= (tmToDateTime tm, {iworld & world = world})

iworldResource :: (*Resource -> (Bool, *Resource)) *IWorld -> (*[*Resource], *IWorld)
iworldResource f iworld=:{IWorld|resources}
# (matches, resources) = splitWithUnique f resources
= (matches, {iworld & resources=resources})
where
	splitWithUnique f [] = ([], [])
	splitWithUnique f [r:rs]
	# (ok, r) = f r
	| ok = let (ms, xs) = splitWithUnique f rs in ([r:ms], xs)
	= let (ms, xs) = splitWithUnique f rs in (ms, [r:xs])

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

instance FileEnv IWorld
where
	accFiles accfun iworld=:{IWorld|world}
		# (x, world) = accFiles accfun world
		= (x, {IWorld | iworld & world=world})
	appFiles appfun iworld=:{IWorld|world}
		# world = appFiles appfun world
		= {IWorld | iworld & world=world}
