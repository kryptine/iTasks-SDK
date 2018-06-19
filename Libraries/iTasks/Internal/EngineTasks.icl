implementation module iTasks.Internal.EngineTasks

import StdBool, StdOverloaded, StdList, StdOrdList
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Functor, Data.Func
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.WF.Definition
import iTasks.Internal.Util
import iTasks.Internal.SDS
import iTasks.Internal.TaskStore
import iTasks.SDS.Definition
import iTasks.SDS.Combinators.Common
import iTasks

from iTasks.Extensions.DateTime import toDate, toTime, instance == Date, instance == Time
from System.Time import time

from TCPIP import :: Timeout

import Data.Queue
import Text

timeout :: !(Maybe Timeout) !*IWorld -> (!Maybe Timeout,!*IWorld)
timeout mt iworld = case read taskEvents iworld of
	//No events
	(Ok (Queue [] []),iworld=:{sdsNotifyRequests,world})
		# (ts, world) = nsTime world
		= ( minListBy lesser [mt:flatten $ map (getTimeoutFromClock ts) $ 'DM'.elems sdsNotifyRequests]
		  , {iworld & world = world})
	(Ok _,iworld)               = (Just 0,iworld)   //There are still events, don't wait
	(Error _,iworld)            = (Just 500,iworld) //Keep retrying, but not too fast
where
	lesser (Just x) (Just y) = x < y
	lesser (Just _) Nothing = True
	lesser Nothing Nothing = False
	
	getTimeoutFromClock :: Timespec (Set SDSNotifyRequest) -> [Maybe Int]
	getTimeoutFromClock now requests = getTimeoutFromClock` <$> 'DS'.toList requests
	where
		getTimeoutFromClock` :: SDSNotifyRequest -> Maybe Int
		getTimeoutFromClock` snr=:{cmpParam=(ts :: ClockParameter Timespec)}
			| startsWith "$IWorld:timespec$" snr.reqSDSId && ts.interval <> zero
				# fire = iworldTimespecNextFire now snr.reqTimespec ts
				= Just (max 0 (toMs fire - toMs now))
			= mt
		getTimeoutFromClock` _ = mt

	toMs x = x.tv_sec * 1000 + x.tv_nsec / 1000000

//When we run the built-in HTTP server we need to do active garbage collection of instances that were created for sessions
removeOutdatedSessions :: Task ()
removeOutdatedSessions = whileUnchanged (sdsFocus {start=Timestamp 0,interval=Timestamp 1} iworldTimestamp)
	\_->get (sdsFocus {InstanceFilter|defaultValue & onlySession=Just True} filteredInstanceIndex)
	>>- mkInstantTask o const o checkAll removeIfOutdated
where
	checkAll f [] iworld = (Ok (),iworld)
	checkAll f [x:xs] iworld = case f x iworld of
		(Ok (),iworld) = checkAll f xs iworld
		(Error e,iworld) = (Error e,iworld)

    removeIfOutdated (instanceNo,_,_,_) iworld=:{options={appVersion,sessionTime},clock=tNow}
		# (remove,iworld) = case read (sdsFocus instanceNo taskInstanceIO) iworld of
			//If there is I/O information, we check that age first
			(Ok (Just (client,tInstance)),iworld) //No IO for too long, clean up
				= (Ok ((tNow - tInstance) > sessionTime),iworld)
			//If there is no I/O information, get meta-data and check builtId and creation date
			(Ok Nothing,iworld)
				= case read (sdsFocus instanceNo taskInstanceConstants) iworld of
					(Ok {InstanceConstants|build,issuedAt=tInstance},iworld)
						| build <> appVersion = (Ok True,iworld)
						| (tNow - tInstance) > sessionTime = (Ok True,iworld)
						= (Ok False,iworld)
					(Error e,iworld)
						= (Error e,iworld)
			(Error e,iworld) 
				= (Error e,iworld)
		= case remove of
			(Ok True)
				# (e,iworld) = deleteTaskInstance instanceNo iworld
				| e=:(Error _) = (e,iworld)
				# (e,iworld) = write Nothing (sdsFocus instanceNo taskInstanceIO) iworld
				| e=:(Error _) = (e,iworld)
				= (Ok (),iworld)
			(Ok False)
				= (Ok (), iworld)
			(Error e)
				= (Error e,iworld)

//When the event queue is empty, write deferred SDS's
flushWritesWhenIdle:: !*IWorld -> (!MaybeError TaskException (), !*IWorld)
flushWritesWhenIdle iworld = case read taskEvents iworld of
		(Error e,iworld)          = (Error e,iworld)
		(Ok (Queue [] []),iworld) = flushDeferredSDSWrites iworld
		(Ok _,iworld)             = (Ok (),iworld)

//When we don't run the built-in HTTP server we don't want to loop forever so we stop the loop
//once all tasks are stable
stopOnStable :: Task ()
stopOnStable = get (sdsFocus {InstanceFilter|defaultValue & includeProgress=True} filteredInstanceIndex)
	>>- \index->mkInstantTask \tid iworld=:{shutdown}->case shutdown of
			Just _ = (Ok (), iworld)
			_ = (Ok (), {iworld & shutdown=
				if (allStable index)
					(Just (if (exceptionOccurred index) 1 0))
					Nothing})
where
	allStable instances = all (\v -> v =: Stable || v =: (Exception _)) (values instances) 
	exceptionOccurred instances = any (\v -> v =: (Exception _)) (values instances)
	values instances = [value \\ (_,_,Just {InstanceProgress|value},_) <- instances]
