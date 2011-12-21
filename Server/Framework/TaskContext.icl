implementation module TaskContext

import SystemTypes
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: EditEvent
from GenUpdate	import :: UpdateMask
import JSON

derive JSONEncode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta, UpdateMask
derive JSONDecode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta, UpdateMask
