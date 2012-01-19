implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, SystemTypes, GenRecord, HTTP, Map, Util
import GenVisualize, iTaskClass
from TaskContext		import :: TaskState(..), :: ParallelMeta, :: ParallelContext, :: ParallelItem
from LayoutCombinators	import :: Layout
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

mkTask :: !TaskInitFun !TaskEditFun !(TaskEvalFun a) -> Task a 
mkTask initFun editFun evalFun =
	{ Task
	| initFun			= initFun
	, editFun			= editFun
	, evalFun			= evalFun
	, layout			= Nothing
	}
	
mkInstantTask :: (TaskId *IWorld -> (!TaskResult a,!*IWorld)) -> Task a |  iTask a
mkInstantTask iworldfun =
	{ Task
	| initFun			= \taskId iworld		-> (TCEmpty taskId,iworld)
	, editFun			= \_ context iworld		-> (context,iworld)
	, evalFun			= evalOnce iworldfun
	, layout			= Nothing
	}
where
	evalOnce f _ _ _ context=:(TCEmpty taskId) iworld = case f taskId iworld of
		(TaskStable res _ _, iworld)	= (TaskStable res NoRep (TCBasic taskId (toJSON res) True), iworld)
		(TaskException e s, iworld)		= (TaskException e s, iworld)
		(_,iworld)						= (taskException "Instant task did not complete instantly", iworld)

	evalOnce f _ _ _ context=:(TCBasic taskId enc True) iworld = case fromJSON enc of
		(Just res)	= (TaskStable res NoRep context, iworld)
		Nothing		= (taskException "Corrupt task result", iworld)
	
JSONEncode{|Task|} _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Task|} _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Task|} _ c = (Nothing,c)

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a =	{ Task
					| initFun	= \_ -> abort funerror
					, editFun	= \_ -> abort funerror
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

taskException :: !e -> TaskResult a | TC, toString e
taskException e = TaskException (dynamic e) (toString e)