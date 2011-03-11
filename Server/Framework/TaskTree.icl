implementation module TaskTree

import StdTuple, Util, Maybe, Either, HTML, Time, Types, StdFunc, GenMap, GenMapSt
from JSON 			import :: JSONNode
from TUIDefinition	import :: TUIDef, :: TUIUpdate

derive gMap		TaskTree, TaskInfo, Maybe
derive gMapLSt	TaskTree, TaskInfo, Maybe
derive bimap (,)

toSpineTree	:: !NonNormalizedTree -> SpineTree
toSpineTree tree		= gMap{|*->*->*->*|} (const Void) (const Void) (const Void) tree

toUITree	:: !NonNormalizedTree !*IWorld -> (!UITree,!*IWorld)
toUITree tree iworld	= gMapLSt{|*->*->*->*|} (app o fst) (\(h,_) w -> (h,w)) app tree iworld

toJSONTree	:: !NonNormalizedTree !*IWorld -> (!JSONTree,!*IWorld)
toJSONTree tree iworld	= gMapLSt{|*->*->*->*|} (app o snd) (\(_,j) w -> (j,w)) app tree iworld
