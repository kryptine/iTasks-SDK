implementation module IWorld

from FilePath			import :: FilePath
from Map				import :: Map
from Maybe				import :: Maybe
from SystemTypes		import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime
from Time				import :: Timestamp
from TaskState			import :: TaskListEntry
from JSON_NG			import :: JSONNode
from SharedDataSource	import class registerSDSMsg, class reportSDSChange, class reportSDSChangeFilter
import TaskStore

instance registerSDSMsg InstanceNo IWorld
where
	registerSDSMsg shareId instanceNo iworld = addShareRegistration shareId instanceNo iworld
			
instance reportSDSChange IWorld
where
	reportSDSChange shareId iworld = addOutdatedOnShareChange shareId iworld
		
instance reportSDSChangeFilter InstanceNo IWorld
where
	reportSDSChangeFilter shareId filterFun iworld //TODO
		= addOutdatedOnShareChange shareId iworld