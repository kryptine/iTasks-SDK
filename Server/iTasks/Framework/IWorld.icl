implementation module iTasks.Framework.IWorld

from System.FilePath				import :: FilePath
from Data.Map						import :: Map
from Data.Maybe						import :: Maybe
from System.Time					import :: Timestamp, time
from Text.JSON						import :: JSONNode
from iTasks.API.Core.Types	        import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime
from iTasks.Framework.TaskState		import :: TaskListEntry
from iTasks.Framework.UIDiff		import :: UIUpdate

from StdFile import class FileSystem(..)
from StdFile import instance FileSystem World

from Data.List import splitWith

import System.Time, StdList, Text.Encodings.Base64, _SystemArray, StdBool, StdTuple, Text.JSON, Data.Error, Data.Map
import iTasks.Framework.TaskStore, iTasks.Framework.Util
import iTasks.Framework.SerializationGraphCopy 

updateCurrentDateTime :: !*IWorld -> *IWorld
updateCurrentDateTime iworld=:{IWorld|current,world}
	# (timestamp,world)		= time world
	# (localDt,world)		= currentLocalDateTimeWorld world
	# (utcDt,world)			= currentUTCDateTimeWorld world
	= {IWorld|iworld  & current = {current & timestamp = timestamp, utcDateTime = utcDt, localDateTime = localDt}, world = world}


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


addUIUpdates :: !InstanceNo ![UIUpdate] !*IWorld -> *IWorld
addUIUpdates instanceNo [] iworld = iworld
addUIUpdates instanceNo updates iworld=:{uiUpdates}
	= {iworld & uiUpdates = put instanceNo (maybe updates (\u -> u ++ updates) (get instanceNo uiUpdates)) uiUpdates}

popUIUpdates :: ![InstanceNo] !*IWorld -> (![(!InstanceNo,![UIUpdate])],!*IWorld)
popUIUpdates instances iworld=:{uiUpdates}
    # uiUpdates     = toList uiUpdates
    # outUpdates    = [m \\ m=:(instanceNo,updates) <- uiUpdates | isMember instanceNo instances]
    # uiUpdates     = [m \\ m=:(instanceNo,_) <- uiUpdates | not (isMember instanceNo instances)]
	= (outUpdates, {iworld & uiUpdates = fromList uiUpdates})

clearUIUpdates :: !InstanceNo !*IWorld -> *IWorld
clearUIUpdates instanceNo iworld=:{uiUpdates}
    = {iworld & uiUpdates = del instanceNo uiUpdates}

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
