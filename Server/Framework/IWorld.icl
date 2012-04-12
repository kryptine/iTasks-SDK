implementation module IWorld

from FilePath		import :: FilePath
from Map			import :: Map
from Maybe			import :: Maybe
from SystemTypes	import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime
from Time			import :: Timestamp
from TaskState		import :: TaskListEntry
from JSON_NG		import :: JSONNode
from SharedDataSource	import class registerSDSMsg, class reportSDSChange, class reportSDSChangeFilter

instance registerSDSMsg ITaskMsg IWorld
where
	registerSDSMsg _ _ iworld = iworld
	
instance reportSDSChange IWorld
where
	reportSDSChange _ iworld = iworld

instance reportSDSChangeFilter ITaskMsg IWorld
where
	reportSDSChangeFilter _ _ iworld = iworld