implementation module TaskContext

import Types
from ProcessDB import ::Process(..)
import JSON, Map

derive JSONEncode TaskContext, TopTaskContext, SubTaskContext, ParallelMeta
derive JSONDecode TaskContext, TopTaskContext, SubTaskContext, ParallelMeta
derive bimap (,),Maybe

getLocalVar :: !String !TaskContext -> Maybe a | JSONDecode{|*|} a
getLocalVar key (TCBasic vars)
	= case get key vars of
		Just json	= (fromJSON json)
		Nothing		= Nothing
getLocalVar _ _ = Nothing

setLocalVar :: !String !a !TaskContext -> TaskContext | JSONEncode{|*|} a
setLocalVar key val (TCBasic vars) = TCBasic (put key (toJSON val) vars)
setLocalVar key val context = context

delLocalVar :: !String !TaskContext -> TaskContext
delLocalVar key (TCBasic vars) = TCBasic (del key vars)
delLocalVar key context = context