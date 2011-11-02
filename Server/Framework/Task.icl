implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, SystemTypes, GenRecord, HTTP, Map, Util
import GenVisualize, iTaskClass
from TaskContext	import :: TaskContextTree(..), :: SubTaskContext, :: ParallelMeta
from iTasks			import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

mkTask :: !d !TaskInitFun !TaskEditFun !(TaskEvalFun a) -> Task a | descr d
mkTask description initFun editFun evalFun =
	{ Task
	| meta				= initTaskMeta description
	, def = NormalTask
		{ initFun		= initFun
		, editFun		= editFun
		, evalFun		= evalFun
		}
	, layout				= Nothing
	}
	
mkInstantTask :: !d (TaskNr *IWorld -> (!TaskResult a,!*IWorld)) -> Task a | descr d
mkInstantTask description iworldfun =
	{ Task
	| meta				= initTaskMeta description
	, def = NormalTask
		{ initFun		= \_ iworld -> (TCEmpty,iworld)
		, editFun		= \_ _  context iworld -> (context,iworld)
		, evalFun		= \taskNr _ _ _ _ _ iworld -> iworldfun taskNr iworld
		}
	, layout				= Nothing
	}
	
mkActionTask :: !d !(A.b: (TermFunc a b) -> TaskFuncs b | iTask b) -> Task a | descr d
mkActionTask description actionTaskFun =
	{ Task
	| meta			= initTaskMeta description
	, def			= ActionTask actionTaskFun
	, layout		= Nothing
	}

mapActionTask :: !((InformationState a) -> (InformationState b)) !(Task a) -> Task b
mapActionTask f task=:{Task|def} = case def of
	ActionTask actionF	= {Task | task & def = ActionTask (\termF -> actionF (termF o f))}
	_					= abort "mapActionTask: no action task"
	
mapActionTaskModelValue	:: !(a -> b) !(Task a) -> Task b
mapActionTaskModelValue f task = mapActionTask (\st=:{modelValue} -> {st & modelValue = f modelValue}) task
	
taskTitle :: !(Task a) -> String
taskTitle task = task.Task.meta.TaskMeta.title

taskMeta :: !(Task a) -> TaskMeta
taskMeta {Task|meta} = meta

instance iTaskId TaskNr
where
	iTaskId :: !TaskNr !String -> String
	iTaskId tasknr postfix = iTaskId (taskNrToString tasknr) postfix

instance iTaskId TaskId
where
	iTaskId :: !TaskId !String -> String
	iTaskId taskid postfix
		# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }		// throw away characters not allowed in a file name
		| postfix == ""		= "iTask_" +++ taskid 
		| otherwise			= "iTask_" +++ taskid +++ "-" +++ postfix
		
taskNrToString :: !TaskNr -> String
taskNrToString [] 		= ""
taskNrToString [i] 		= toString i
taskNrToString [i:is] 	= taskNrToString is +++ "." +++ toString i 

taskNrFromString :: !String -> TaskNr
taskNrFromString "" 		= []
taskNrFromString string	= reverse (parseTaskNr` (fromString string))
where
	parseTaskNr` :: ![Char] -> TaskNr
	parseTaskNr` [] = []
	parseTaskNr` list 
	# (front,end)	= span (\c -> c <> '.') list
	=  [toInt (toString  front) : parseTaskNr` (stl end)]

	toString :: [Char] -> String
	toString list = {c \\ c <- list}

	stl :: [Char] -> [Char]
	stl [] = []
	stl xs = tl xs

stepEvent :: !Int !(Maybe (Event e)) -> Maybe (Event e)
stepEvent i (Just (TaskEvent [s:ss] e))
	| s == i	= Just (TaskEvent ss e)
				= Nothing
stepEvent i (Just (ProcessEvent s e))	= Just (ProcessEvent s e)
stepEvent _ _ 							= Nothing

stepTarget :: !Int !ReversedTaskNr -> ReversedTaskNr
stepTarget i []		= []
stepTarget i [t:ts]	
	| i == t			= ts
	| otherwise			= []

JSONEncode{|Task|} _ tt = dynamicJSONEncode tt			
JSONDecode{|Task|} _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Task|} _ c = (Nothing,c)

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a =	{ Task
					| meta			= initTaskMeta Void
					, def = NormalTask
						{ initFun	= abort funerror
						, editFun	= abort funerror
						, evalFun	= abort funerror
						}
					, layout = Nothing
					}
	funerror = "Creating default task functions is impossible"
	
gUpdate{|Task|} _ (UDSearch t) ust = basicSearch t (\Void t -> t) ust

gDefaultMask{|Task|} _ _ = [Touched []]

gVerify{|Task|} _ _ vst = alwaysValid vst

gVisualizeText{|Task|} _ _ {Task|meta} = [meta.TaskMeta.title]
gVisualizeEditor{|Task|} _ _ _ _ mbVal vst
	# vis = case mbVal of
		Just {Task|meta}	= [stringDisplay meta.TaskMeta.title]
		Nothing				= []
	= (vis,vst)
gHeaders{|Task|} _ = (undef, ["Task"])
gGridRows{|Task|} _ _ _ _ = Nothing	
gEq{|Task|} _ _ _ = True // tasks are always equal

gGetRecordFields{|Task|} _ _ _ fields = fields
gPutRecordFields{|Task|} _ t _ fields = (t,fields)

taskFuncs :: !(Task a) -> TaskFuncs a | iTask a
taskFuncs {Task|def} = case def of
	NormalTask funcs	= funcs
	ActionTask actionF	= actionF (\{modelValue,localValid} -> UserActions [(ActionOk,if localValid (Just modelValue) Nothing)])

taskLayouters :: !(Task a) -> (InteractionLayouter, ParallelLayouter)
taskLayouters {Task|layout} = case layout of
	Nothing				= (defaultInteractionLayout	, defaultParallelLayout)
	Just (Left ilayout)	= (ilayout					, defaultParallelLayout)
	Just (Right playout)= (defaultInteractionLayout	, playout)
	

taskException :: !e -> TaskResult a | TC, toString e
taskException e = TaskException (dynamic e) (toString e)