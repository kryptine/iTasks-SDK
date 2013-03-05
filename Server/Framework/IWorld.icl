implementation module IWorld

from FilePath			import :: FilePath
from Map				import :: Map
from Maybe				import :: Maybe
from SystemTypes		import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime
from Time				import :: Timestamp
from TaskState			import :: TaskListEntry
from JSON_NG			import :: JSONNode

from StdFile import class FileSystem(..)
from StdFile import instance FileSystem World
from List_NG import splitWith
from SharedDataSource	import class registerSDSDependency, class registerSDSChangeDetection, class reportSDSChange, :: CheckRes(..), :: BasicShareId, :: Hash
import TaskStore, Time, Util, StdList, Base64, _SystemArray, StdBool, StdTuple
import SerializationGraphCopy //TODO: Make switchable from within iTasks module

updateCurrentDateTime :: !*IWorld -> *IWorld
updateCurrentDateTime iworld=:{IWorld|world}
	# (dt,world)			= currentDateTimeWorld world
	# (timestamp,world)		= time world
	= {IWorld|iworld  & currentDateTime = dt, timestamp = timestamp, world = world}

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
	(==) (Evaluate instanceNoX)		(Evaluate instanceNoY)		= instanceNoX == instanceNoY
	(==) (TriggerSDSChange sdsIdX)	(TriggerSDSChange sdsIdY)	= sdsIdX == sdsIdY
	(==) (CheckSDS sdsIdX hashX _)	(CheckSDS sdsIdY hashY _)	= sdsIdX == sdsIdY && hashX == hashY
	(==) _							_							= False

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
	reportSDSChange shareId filterFun iworld
		= addOutdatedOnShareChange shareId filterFun iworld

instance reportSDSChange Void IWorld
where
	reportSDSChange shareId _ iworld
		= addOutdatedOnShareChange shareId (\_ -> True) iworld


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