implementation module iTasks.Internal.TaskIO

import StdEnv
from Control.Applicative import class Alternative(<|>)
import Data.Func
import Data.Error
import Data.Maybe
import Data.Functor
import Data.List
import Data.Func
import qualified Data.Map as DM
import Data.Map.GenJSON
import qualified Data.Queue as DQ
import qualified Data.Set as DS
import Data.Set.GenJSON
import iTasks.WF.Definition
import iTasks.Internal.SDSService
import Text

from Data.Queue import :: Queue(..)

import iTasks.Internal.SDS
import qualified iTasks.Internal.SDS as SDS
from iTasks.SDS.Definition import :: SDSLensRead(..), :: SDSLensWrite(..), :: SDSLensNotify(..)
import iTasks.SDS.Combinators.Core, iTasks.SDS.Combinators.Common
import iTasks.SDS.Sources.Store
import iTasks.WF.Derives

derive JSONEncode TaskOutputMessage, Queue, Event
derive JSONDecode TaskOutputMessage, Queue, Event

rawInstanceEvents    = storeShare NS_TASK_INSTANCES False InMemory (Just 'DQ'.newQueue)
rawInstanceOutput    = storeShare NS_TASK_INSTANCES False InMemory (Just 'DM'.newMap)

//Event queues of task instances
taskEvents :: SimpleSDSLens TaskInput
taskEvents = sdsFocus "events" rawInstanceEvents

taskOutput :: SimpleSDSLens (Map InstanceNo TaskOutput)
taskOutput = sdsFocus "taskOutput" rawInstanceOutput

taskInstanceOutput :: SDSLens InstanceNo TaskOutput TaskOutput
taskInstanceOutput = sdsLens "taskInstanceOutput" (const ()) (SDSRead read) (SDSWrite write) (SDSNotifyConst notify) (Just reducer) taskOutput
where
	read instanceNo outputs = Ok (fromMaybe 'DQ'.newQueue ('DM'.get instanceNo outputs))
	write instanceNo outputs output = Ok (Just ('DM'.put instanceNo output outputs))
	notify instanceNo _ = const ((==) instanceNo)
	reducer p ws = Ok (fromMaybe 'DQ'.newQueue ('DM'.get p ws))

queueEvent :: !InstanceNo !Event !*IWorld -> *IWorld
queueEvent ino event iworld = snd (write (ino, event) queueEventShare EmptyContext iworld)

queueEventShare :: SDSLens () () (InstanceNo, Event)
queueEventShare = mapReadWrite (const (), writer) Nothing taskEvents
where
	writer :: (InstanceNo, Event) TaskInput -> Maybe TaskInput
	writer (instanceNo, event) q = Just (fromMaybe ('DQ'.enqueue (instanceNo,event) q) (queueWithMergedRefreshEvent q))
	where
		// merge multiple refresh events for same instance
		queueWithMergedRefreshEvent :: !(Queue (!InstanceNo, !Event)) -> Maybe (Queue (!InstanceNo, !Event))
		queueWithMergedRefreshEvent ('DQ'.Queue front back) = case event of
			RefreshEvent refreshTasks reason =
				((\front` -> ('DQ'.Queue front` back))  <$> queueWithMergedRefreshEventList front) <|>
				((\back`  -> ('DQ'.Queue front  back`)) <$> queueWithMergedRefreshEventList back)
			where
				queueWithMergedRefreshEventList :: [(InstanceNo, Event)] -> Maybe [(InstanceNo, Event)]
				queueWithMergedRefreshEventList [] = Nothing
				queueWithMergedRefreshEventList [hd=:(instanceNo`, event`) : tl] = case event` of
					RefreshEvent refreshTasks` reason` | instanceNo` == instanceNo =
						Just [(instanceNo, RefreshEvent ('DS'.union refreshTasks refreshTasks`) (mergeReason reason reason`)) : tl]
					_ =
						(\tl` -> [hd : tl`]) <$> queueWithMergedRefreshEventList tl
	
				mergeReason :: !String !String -> String
				mergeReason x y = concat [x , "; " , y]
			_ = Nothing

queueRefresh :: ![(TaskId, String)] !*IWorld -> *IWorld
queueRefresh [] iworld = iworld
queueRefresh tasks iworld
	//Clear the instance's share change registrations, we are going to evaluate anyway
	# iworld	= 'SDS'.clearTaskSDSRegistrations ('DS'.fromList (map fst tasks)) iworld
	# iworld 	= foldl (\w (t,r) -> queueEvent (toInstanceNo t) (RefreshEvent ('DS'.singleton t) r) w) iworld tasks
	= iworld

dequeueEvent :: !*IWorld -> (!MaybeError TaskException (Maybe (InstanceNo,Event)),!*IWorld)
dequeueEvent iworld
  = case 'SDS'.read taskEvents 'SDS'.EmptyContext iworld of
	(Error e, iworld)               = (Error e, iworld)
	(Ok ('SDS'.ReadingDone queue), iworld)
	# (val, queue) = 'DQ'.dequeue queue
	= case 'SDS'.write queue taskEvents 'SDS'.EmptyContext iworld of
	  (Error e, iworld) = (Error e, iworld)
	  (Ok 'SDS'.WritingDone, iworld) = (Ok val, iworld)

clearEvents :: !InstanceNo !*IWorld -> *IWorld
clearEvents instanceNo iworld
	# (_,iworld) = 'SDS'.modify clear taskEvents 'SDS'.EmptyContext iworld
	= iworld
where
	clear (Queue fs bs) = Queue [f \\ f=:(i,_) <- fs | i <> instanceNo] [b \\ b=:(i,_) <- bs | i <> instanceNo]

queueOutput :: !InstanceNo ![TaskOutputMessage] !*IWorld -> *IWorld
queueOutput instanceNo messages iworld
	# (_,iworld) = 'SDS'.modify (enqueueAll messages) (sdsFocus instanceNo taskInstanceOutput) 'SDS'.EmptyContext iworld
	= iworld
where
	enqueueAll [] q = q
	enqueueAll [x:xs] q = enqueueAll xs ('DQ'.enqueue x q)

queueUIChange :: !InstanceNo !UIChange !*IWorld -> *IWorld
queueUIChange instanceNo change iworld = queueOutput instanceNo [TOUIChange change] iworld

queueUIChanges :: !InstanceNo ![UIChange] !*IWorld -> *IWorld
queueUIChanges instanceNo changes iworld = queueOutput instanceNo (map TOUIChange changes) iworld

queueException :: !InstanceNo !String !*IWorld -> *IWorld
queueException instanceNo description iworld = queueOutput instanceNo [TOException description] iworld

attachViewport :: !InstanceNo !*IWorld -> *IWorld
attachViewport instanceNo iworld
	# iworld = clearEvents instanceNo iworld
	# iworld = queueEvent instanceNo ResetEvent iworld
	= iworld

detachViewport :: !InstanceNo !*IWorld -> *IWorld
detachViewport instanceNo iworld
	# iworld = clearEvents instanceNo iworld
	= iworld

