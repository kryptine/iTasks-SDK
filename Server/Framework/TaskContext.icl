implementation module TaskContext

import SystemTypes
from iTasks		import JSONEncode, JSONDecode
from Task		import :: Event, :: EditEvent
from GenUpdate	import :: UpdateMask
import JSON, Map

derive JSONEncode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta, UpdateMask
derive JSONDecode TaskContext, ProcessState, TaskContextTree, SubTaskContext, ParallelMeta, UpdateMask

getLocalVar :: !String !TaskContextTree -> Maybe a | JSONDecode{|*|} a
getLocalVar key (TCBasic vars)
	= case get key vars of
		Just json	= (fromJSON json)
		Nothing		= Nothing
getLocalVar _ _ = Nothing

setLocalVar :: !String !a !TaskContextTree -> TaskContextTree | JSONEncode{|*|} a
setLocalVar key val (TCBasic vars) = TCBasic (put key (toJSON val) vars)
setLocalVar key val context = context

delLocalVar :: !String !TaskContextTree -> TaskContextTree
delLocalVar key (TCBasic vars) = TCBasic (del key vars)
delLocalVar key context = context