implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, SystemTypes, GenRecord, HTTP, Map, Util
import GenVisualize, iTaskClass, IWorld
from TaskState			import :: TaskState(..), :: ParallelMeta, :: ParallelItem
from LayoutCombinators	import :: Layout
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

mkTask :: !TaskInitFun !(TaskEvalFun a) -> Task a 
mkTask initFun evalFun =
	{ Task
	| initFun			= initFun
	, evalFun			= evalFun
	, layout			= Nothing
	}
	
mkInstantTask :: (TaskId *IWorld -> (!TaskResult a,!*IWorld)) -> Task a |  iTask a
mkInstantTask iworldfun =
	{ Task
	| initFun			= \taskId iworld=:{taskTime}	-> (TCEmpty taskId taskTime,iworld)
	, evalFun			= evalOnce iworldfun
	, layout			= Nothing
	}
where
	evalOnce f _ _ _ context=:(TCEmpty taskId _) iworld = case f taskId iworld of
		(ValueResult (Value a Stable) ts _ _, iworld)	= (ValueResult (Value a Stable) ts NoRep (TCStable taskId (toJSON a) ts), iworld)
		(ExceptionResult e s, iworld)					= (ExceptionResult e s, iworld)
		(_,iworld)										= (exception "Instant task did not complete instantly", iworld)

	evalOnce f _ _ _ context=:(TCStable taskId enc ts) iworld = case fromJSON enc of
		(Just a)	= (ValueResult (Value a Stable) ts NoRep context, iworld)
		Nothing		= (exception "Corrupt task result", iworld)

derive gGetRecordFields	TaskValue, Stability
derive gPutRecordFields	TaskValue, Stability

JSONEncode{|Task|} _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Task|} _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Task|} _ c = (Nothing,c)

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a =	{ Task
					| initFun	= \_ -> abort funerror
					, evalFun	= \_ -> abort funerror
					, layout	= Nothing
					}
	funerror = "Creating default task functions is impossible"
	
gUpdate{|Task|} _ (UDSearch t) ust = basicSearch t (\Void t -> t) ust

gDefaultMask{|Task|} _ _ = [Touched []]

gVerify{|Task|} _ _ vst = alwaysValid vst

gVisualizeText{|Task|} _ _ _ = ["<Task>"]
gVisualizeEditor{|Task|} _ _ _ _ _ vst = (NormalEditor [stringDisplay "<Task>"],vst)

gHeaders{|Task|} _ = (undef, ["Task"])
gGridRows{|Task|} _ _ _ _	= Nothing	
gEq{|Task|} _ _ _			= True // tasks are always equal??

gGetRecordFields{|Task|} _ _ _ fields = fields
gPutRecordFields{|Task|} _ t _ fields = (t,fields)

exception :: !e -> TaskResult a | TC, toString e
exception e = ExceptionResult (dynamic e) (toString e)

instance Functor TaskValue
where
	fmap f (NoValue)		= NoValue
	fmap f (Value v s)		= Value (f v) s
