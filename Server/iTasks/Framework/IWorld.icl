implementation module iTasks.Framework.IWorld

from System.FilePath				import :: FilePath
from Data.Map						import :: Map
from Data.Maybe						import :: Maybe
from System.Time					import :: Timestamp, time
from Text.JSON						import :: JSONNode
from iTasks.API.Core.SystemTypes	import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime
from iTasks.Framework.TaskState		import :: TaskListEntry
from iTasks.Framework.UIDiff		import :: UIUpdate

from StdFile import class FileSystem(..)
from StdFile import instance FileSystem World
//from iTasks.Framework.TaskServer import class HttpServerEnv(..)

from Data.List import splitWith
from Data.SharedDataSource	import class registerSDSDependency, class registerSDSChangeDetection, class reportSDSChange, :: CheckRes(..), :: BasicShareId, :: Hash

import System.Time, StdList, Text.Encodings.Base64, _SystemArray, StdBool, StdTuple, Text.JSON, Data.Error, Data.Map
import iTasks.Framework.TaskStore, iTasks.Framework.Util
import iTasks.Framework.SerializationGraphCopy 

updateCurrentDateTime :: !*IWorld -> *IWorld
updateCurrentDateTime iworld=:{IWorld|world}
	# (localDt,world)		= currentLocalDateTimeWorld world
	# (utcDt,world)			= currentUTCDateTimeWorld world
	# (timestamp,world)		= time world
	= {IWorld|iworld  & currentLocalDateTime = localDt, currentUTCDateTime = utcDt, timestamp = timestamp, world = world}

queueWork :: !(!Work, !Maybe Timestamp) !*IWorld -> *IWorld
queueWork newWork iworld=:{workQueue}
	= {iworld & workQueue = queue newWork workQueue}
where
	queue newWork [] = [newWork]
	queue newWorkTs=:(newWork,mbNewTimestamp) [workTs=:(work,mbTimestamp):qs]
		| newWork == work	= [(work,minTs mbNewTimestamp mbTimestamp):qs]
		| otherwise			= [workTs:queue newWorkTs qs]
	
	minTs Nothing _			= Nothing
	minTs _ Nothing			= Nothing
	minTs (Just x) (Just y)	= Just (min x y)
	
instance == Work
where
	(==) (Evaluate instanceNoX)			(Evaluate instanceNoY)			= instanceNoX == instanceNoY
	(==) (EvaluateUrgent instanceNoX)	(EvaluateUrgent instanceNoY)	= instanceNoX == instanceNoY
	(==) (TriggerSDSChange sdsIdX)		(TriggerSDSChange sdsIdY)		= sdsIdX == sdsIdY
	(==) (CheckSDS sdsIdX hashX _)		(CheckSDS sdsIdY hashY _)		= sdsIdX == sdsIdY && hashX == hashY
	(==) _							_							= False

queueUrgentEvaluate	:: !InstanceNo !*IWorld -> *IWorld
queueUrgentEvaluate instanceNo iworld=:{workQueue}
	= {iworld & workQueue = queue instanceNo workQueue}
where
	queue newInstanceNo []			= [(EvaluateUrgent instanceNo,Nothing)]
	queue newInstanceNo [(EvaluateUrgent no,ts):qs]
		| newInstanceNo == no		= [(EvaluateUrgent no,ts):qs]
									= [(EvaluateUrgent no,ts):queue newInstanceNo qs]
	queue newInstanceNo [(Evaluate no,ts):qs]		
		| newInstanceNo == no		= [(EvaluateUrgent no,Nothing):qs]	
									= [(Evaluate no,ts):queue newInstanceNo qs]
	queue newInstanceNo [q:qs]		= [q:queue newInstanceNo qs]
	
dequeueWork	:: !*IWorld -> (!DequeueResult, !*IWorld)
dequeueWork iworld=:{workQueue}
	| isEmpty workQueue	= (Empty, iworld)
	# (curTime, iworld)	= currentTimestamp iworld
	# (res, workQueue)	= getFirst curTime Nothing workQueue
	= (res, {iworld & workQueue = workQueue})
where
	getFirst _ mbMin [] = (maybe Empty WorkAt mbMin,[])
	getFirst curTime mbMin [(w,mbTime):ws] = case mbTime of
		Nothing
			= (Work w,ws)
		Just time | curTime >= time
			= (Work w,ws)
		Just time
			# (mbWork,ws) = getFirst curTime (Just (maybe time (min time) mbMin)) ws
			= (mbWork,[(w,mbTime):ws])
			
dequeueWorkFilter :: !(Work -> Bool) !*IWorld -> (![Work], !*IWorld)
dequeueWorkFilter filter iworld=:{workQueue}
	# (curTime, iworld)		= currentTimestamp iworld
	# (result,workQueue)	= splitWith (filter` curTime) workQueue
	= (map fst result, {iworld & workQueue = workQueue})
where
	filter` _		(work,Nothing)		= filter work
	filter` curTime	(work,Just time)	= curTime >= time && filter work

//Determine the expiration of request, thereby determining the poll interval of
//polling clients
REGULAR_EXPIRY		:== 10000
FAST_EXPIRY			:== 100
IMMEDIATE_EXPIRY	:== 0

getResponseExpiry :: !InstanceNo !*IWorld -> (!Maybe Int, !*IWorld) 
getResponseExpiry instanceNo iworld=:{workQueue}
	= (Just (expiry instanceNo workQueue), iworld)
where
	expiry _ [] = REGULAR_EXPIRY	
	expiry instanceNo [(Evaluate _,Just (Timestamp 0)):ws]		//HACK...
								= IMMEDIATE_EXPIRY
	expiry instanceNo [(Evaluate evalNo,_):ws]
		| evalNo == instanceNo	= FAST_EXPIRY
								= expiry instanceNo ws
	expiry instanceNo [_:ws]	= expiry instanceNo ws


addUIMessage :: !InstanceNo !UIMessage !*IWorld -> *IWorld
addUIMessage instanceNo message iworld=:{uiMessages}
	= {iworld & uiMessages = put instanceNo (maybe [message] (\m -> m ++ [message]) (get instanceNo uiMessages)) uiMessages}

getUIMessages :: !InstanceNo !*IWorld -> (![UIMessage],!*IWorld)
getUIMessages instanceNo iworld=:{uiMessages}
	= (fromMaybe [] (get instanceNo uiMessages),{iworld & uiMessages = del instanceNo uiMessages})

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

instance registerSDSDependency InstanceNo IWorld
where
	registerSDSDependency sdsId instanceNo iworld
		= addShareRegistration sdsId instanceNo iworld

instance registerSDSChangeDetection IWorld
where
	registerSDSPredictableChange timestamp sdsId iworld
		= queueWork (TriggerSDSChange sdsId, Just timestamp) iworld
	registerSDSCheckForChange timestamp hash checkF sdsId iworld
		= queueWork (CheckSDS sdsId hash checkF, Just timestamp) iworld
		
instance reportSDSChange InstanceNo IWorld
where
	reportSDSChange sdsId filterFun iworld
		= addOutdatedOnShareChange sdsId filterFun iworld

instance reportSDSChange Void IWorld
where
	reportSDSChange sdsId _ iworld
		= addOutdatedOnShareChange sdsId (\_ -> True) iworld

// serialise Work as dynamic since it contains functions on unique states
JSONEncode{|Work|} work  = [JSONArray [JSONString "_FUNCTION_", JSONString (base64URLEncode (serialize work))]]
JSONDecode{|Work|} [JSONArray [JSONString "_FUNCTION_",JSONString string]:c] = (Just (fromOk(deserialize {s` \\ s` <-: base64URLDecode string})) ,c)

WORKQUEUE_INDEX :== "workqueue-index"
	
saveWorkQueue :: !*IWorld -> *IWorld
saveWorkQueue iworld=:{workQueue} = storeValue NS_TASK_INSTANCES WORKQUEUE_INDEX workQueue iworld

restoreWorkQueue :: !*IWorld -> *IWorld
restoreWorkQueue iworld
	# (mbWorkQueue,iworld) = loadValue NS_TASK_INSTANCES WORKQUEUE_INDEX iworld
	= {iworld & workQueue = fromMaybe [] mbWorkQueue}
