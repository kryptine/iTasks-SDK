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

from SharedDataSource	import class registerSDSMsg, class reportSDSChange
import TaskStore, Time, Util

updateCurrentDateTime :: !*IWorld -> *IWorld
updateCurrentDateTime iworld=:{IWorld|world}
	# (dt,world)			= currentDateTimeWorld world
	# (timestamp,world)		= time world
	= {IWorld|iworld  & currentDateTime = dt, timestamp = timestamp, world = world}

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

instance registerSDSMsg InstanceNo IWorld
where
	registerDependency shareId instanceNo iworld = addShareRegistration shareId instanceNo iworld
	registerTimedMsg timestamp instanceNo iworld = addOutdatedInstances [(instanceNo, Just timestamp)] iworld
		
instance reportSDSChange InstanceNo IWorld
where
	reportSDSChange shareId filterFun iworld
		= addOutdatedOnShareChange shareId filterFun iworld