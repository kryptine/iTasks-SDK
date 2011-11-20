implementation module IWorld

from FilePath		import :: FilePath
from Map			import :: Map
from Maybe			import :: Maybe
from SystemTypes	import :: DateTime, :: User, :: ProcessId, :: Config
from Time			import :: Timestamp
from SharedMemory	import class MemoryEnv
import SharedDataSource, JSON

instance MemoryEnv IWorld
where
	accMemory accFunc env
		# (a, mem) = accFunc 0
		= (a,env)