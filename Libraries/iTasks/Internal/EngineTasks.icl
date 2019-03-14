implementation module iTasks.Internal.EngineTasks

import Data.Functor, Data.Func
import Data.List
import qualified Data.Map as DM
import Data.Queue
import qualified Data.Set as DS
import StdBool, StdOverloaded, StdList, StdOrdList
import System.Time
import Text
import Text.GenJSON
import iTasks.Engine
import iTasks.Extensions.DateTime
import iTasks.Internal.IWorld
import iTasks.Internal.SDS
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskServer
import iTasks.Internal.TaskStore
import iTasks.Internal.Util
import iTasks.SDS.Combinators.Common
import iTasks.SDS.Definition
import iTasks.WF.Definition

from TCPIP import :: Timeout

timeout :: !(Maybe Timeout) !*IWorld -> (!Maybe Timeout,!*IWorld)
timeout mt iworld = case read taskEvents EmptyContext iworld of
	//No events
	(Ok (ReadingDone (Queue [] [])),iworld=:{sdsNotifyRequests,world})
		# (ts, world) = nsTime world
		= ( minListBy lesser [mt:flatten $ map (getTimeoutFromClock ts) $ 'DM'.elems sdsNotifyRequests]
		  , {iworld & world = world})
	(Ok (ReadingDone (Queue _ _)), iworld)               = (Just 0,iworld)   //There are still events, don't wait
	(Error _,iworld)            = (Just 500,iworld) //Keep retrying, but not too fast
where
	lesser (Just x) (Just y) = x < y
	lesser (Just _) Nothing = True
	lesser Nothing Nothing = False

	getTimeoutFromClock :: Timespec (Map SDSNotifyRequest Timespec) -> [Maybe Timeout]
	getTimeoutFromClock now requests = getTimeoutFromClock` <$> 'DM'.toList requests
	where
		getTimeoutFromClock` :: (!SDSNotifyRequest, !Timespec) -> Maybe Timeout
		getTimeoutFromClock` (snr=:{cmpParam=(ts :: ClockParameter Timespec)}, reqTimespec)
			| startsWith "$IWorld:timespec$" snr.reqSDSId && ts.interval <> zero
				# fire = iworldTimespecNextFire now reqTimespec ts
				= Just (max 0 (toMs fire - toMs now))
			= mt
		getTimeoutFromClock` _ = mt

	toMs x = x.tv_sec * 1000 + x.tv_nsec / 1000000

updateClock :: !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateClock iworld=:{IWorld|clock,world}
	//Determine current date and time
	# (timespec,world) = nsTime world
	# iworld & world   = world
	//Write SDS if necessary
	# (mbe,iworld)     = write timespec (sdsFocus {start=zero,interval=zero} iworldTimespec) EmptyContext iworld
	= (() <$ mbe, iworld)

everyTick :: (*IWorld -> *(!MaybeError TaskException (), !*IWorld)) -> Task ()
everyTick f = Task eval
where
	eval event evalOpts tree=:(TCInit taskId ts) iworld
		# (merr, iworld) = f iworld
		| isError merr = (ExceptionResult (fromError merr), iworld)
		# (merr, iworld) = readRegister taskId tick iworld
		| isError merr = (ExceptionResult (fromError merr), iworld)
		= (ValueResult
				NoValue
				{TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
				NoChange
				(TCInit taskId ts)
			, iworld)
	
//When we run the built-in HTTP server we need to do active garbage collection of instances that were created for sessions
removeOutdatedSessions :: Task ()
removeOutdatedSessions = everyTick \iworld=:{IWorld|options}->
	case read (sdsFocus {InstanceFilter|defaultValue & onlySession=Just True} filteredInstanceIndex) EmptyContext iworld of
		(Ok (ReadingDone index), iworld) = checkAll (removeIfOutdated options) index iworld
		(Error e, iworld)                = (Error e, iworld)
where
	checkAll f [] iworld = (Ok (),iworld)
	checkAll f [x:xs] iworld = case f x iworld of
		(Ok (),iworld) = checkAll f xs iworld
		(Error e,iworld) = (Error e,iworld)

    removeIfOutdated options (instanceNo,_,_,_) iworld=:{options={appVersion},clock=tNow}
		# (remove,iworld) = case read (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
			//If there is I/O information, we check that age first
			(Ok (ReadingDone (Just (client,tInstance))),iworld) //No IO for too long, clean up
				= (Ok ((tNow - tInstance) > options.EngineOptions.sessionTime),iworld)
			//If there is no I/O information, get meta-data and check builtId and creation date
			(Ok (ReadingDone Nothing),iworld)
				= case read (sdsFocus instanceNo taskInstanceConstants) EmptyContext iworld of
					(Ok (ReadingDone {InstanceConstants|build,issuedAt=tInstance}),iworld)
						| build <> appVersion = (Ok True,iworld)
						| (tNow - tInstance) > options.EngineOptions.sessionTime = (Ok True,iworld)
						= (Ok False,iworld)
					(Error e,iworld)
						= (Error e,iworld)
			(Error e,iworld)
				= (Error e,iworld)
		= case remove of
			(Ok True)
				# (e,iworld) = deleteTaskInstance instanceNo iworld
				| e=:(Error _) = (e,iworld)
				= case write Nothing (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
					(Error e, iworld) = (Error e, iworld)
					(Ok WritingDone, iworld) = (Ok (), iworld)
			(Ok False)
				= (Ok (), iworld)
			(Error e)
				= (Error e,iworld)

//When the event queue is empty, write deferred SDS's
flushWritesWhenIdle:: Task ()
flushWritesWhenIdle = everyTick \iworld->case read taskEvents EmptyContext iworld of
		(Error e,iworld)          = (Error e,iworld)
		(Ok (ReadingDone (Queue [] [])),iworld) = flushDeferredSDSWrites iworld
		(Ok _,iworld)             = (Ok (),iworld)

//When we don't run the built-in HTTP server we don't want to loop forever so we stop the loop
//once all tasks are stable
stopOnStable :: Task ()
stopOnStable = everyTick \iworld=:{IWorld|shutdown}->case read (sdsFocus {InstanceFilter|defaultValue & includeProgress=True} filteredInstanceIndex) EmptyContext iworld of
		(Ok (ReadingDone index), iworld)
			# shutdown = case shutdown of
				Nothing = if (allStable index) (Just (if (exceptionOccurred index) 1 0)) Nothing
				_       = shutdown
			= (Ok (), {IWorld|iworld & shutdown = shutdown})
		(Error e, iworld)  = (Error e, iworld)
where
	allStable instances = all (\v -> v =: Stable || v =: (Exception _)) (values instances)
	exceptionOccurred instances = any (\v -> v =: (Exception _)) (values instances)
	values instances = [value \\ (_,_,Just {InstanceProgress|value},_) <- instances]
