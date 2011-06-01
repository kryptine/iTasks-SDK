implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, Types, GenRecord, HTTP, Map, Util
import GenVisualize, iTaskClass
from TaskContext	import :: TaskContext(..), :: TopTaskContext, :: SubTaskContext, :: ParallelMeta
from ProcessDB		import :: Process
from iTasks			import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

mkTask :: !d !TaskInitFun !TaskEditEventFun !(TaskEvalFun a) -> Task a | descr d
mkTask description initFun editEventFun evalTaskFun =
	{ properties			= {TaskProperties|initTaskProperties & taskDescription = toDescr description}
	, mbTaskNr				= Nothing
	, type = NormalTask
		{ initFun			= initFun
		, editEventFun		= editEventFun
		, evalTaskFun		= evalTaskFun
		}
	}
	
mkInstantTask :: !d (TaskNr *IWorld -> (!TaskResult a,!*IWorld)) -> Task a | descr d
mkInstantTask description iworldfun =
	{ properties			= {TaskProperties|initTaskProperties & taskDescription = toDescr description}
	, mbTaskNr				= Nothing
	, type = NormalTask
		{ initFun			= \_ iworld -> (TCBasic newMap,iworld)
		, editEventFun		= \_ _ context iworld -> (context,iworld)
		, evalTaskFun		= \taskNr _ _ _ _ _ iworld -> iworldfun taskNr iworld
		}
	}
	
mkActionTask :: !d !(A.b: (TermFunc a b) -> TaskFuncs b | iTask b) -> Task a | descr d
mkActionTask description actionTaskFun =
	{ properties	= {TaskProperties|initTaskProperties & taskDescription = toDescr description}
	, mbTaskNr		= Nothing
	, type			= ActionTask actionTaskFun
	}

mapActionTask :: !((InformationState a) -> (InformationState b)) !(Task a) -> Task b
mapActionTask f task=:{Task|type} = case type of
	ActionTask actionF	= {Task | task & type = ActionTask (\termF -> actionF (termF o f))}
	_					= abort "mapActionTask: no action task"
	
taskTitle :: !(Task a) -> String
taskTitle task = task.Task.properties.taskDescription.TaskDescription.title

taskDescription	:: !(Task a) -> String
taskDescription task = task.Task.properties.taskDescription.TaskDescription.description

taskProperties :: !(Task a) -> TaskProperties
taskProperties {Task|properties} = properties
	
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

stepCommitEvent :: !Int !(Maybe CommitEvent) -> Maybe CommitEvent 
stepCommitEvent i (Just ([s:ss],action))
	| s == i	= Just (ss,action)
				= Nothing
stepCommitEvent _ _ = Nothing

stepTUITaskNr :: !Int !ReversedTaskNr -> ReversedTaskNr
stepTUITaskNr i []		= []
stepTUITaskNr i [t:ts]	
	| i == t			= ts
	| otherwise			= []

derive JSONEncode Task
derive JSONDecode Task
derive bimap Maybe, (,)

// Generic functions for menus not needed because only functions generating menus (no actual menu structures) are serialised
JSONEncode{|Menu|} _			= abort "not implemented"
JSONEncode{|MenuItem|} _		= abort "not implemented"
JSONDecode{|Menu|} _			= abort "not implemented"
JSONDecode{|MenuItem|} _		= abort "not implemented"
gUpdate{|Menu|} _ _				= abort "not implemented"
gUpdate{|MenuItem|} _ _			= abort "not implemented"
gDefaultMask{|Menu|} _			= abort "not implemented"
gDefaultMask{|MenuItem|} _		= abort "not implemented"
gVerify{|Menu|} _ _				= abort "not implemented"
gVerify{|MenuItem|} _ _			= abort "not implemented"
gVisualize{|Menu|} _ _			= abort "not implemented"
gVisualize{|MenuItem|} _ _		= abort "not implemented"
gEq{|Menu|} _ _					= abort "not implemented"
gEq{|MenuItem|} _ _				= abort "not implemented"
JSONEncode{|TUIInteraction|} _	= abort "not implemented"
JSONDecode{|TUIInteraction|} _	= abort "not implemented"
JSONEncode{|TUIParallel|} _		= abort "not implemented"
JSONDecode{|TUIParallel|} _		= abort "not implemented"
JSONEncode{|TUIResult|} _		= abort "not implemented"
JSONDecode{|TUIResult|} _		= abort "not implemented"
JSONEncode{|TUIDef|} _			= abort "not implemented"
JSONDecode{|TUIDef|} _			= abort "not implemented"

JSONEncode{|TaskType|} _ tt = dynamicJSONEncode tt
					
JSONDecode{|TaskType|} _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|TaskType|} _ c = (Nothing,c)

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a =	{ properties		= {initTaskProperties & taskDescription = toDescr "return"}
					, mbTaskNr			= Nothing
					, type = NormalTask
						{ initFun		= undef
						, editEventFun	= undef
						, evalTaskFun	= undef
						}
					}
gUpdate{|Task|} _ (UDSearch t) ust = basicSearch t (\Void t -> t) ust

gDefaultMask{|Task|} _ _ = [Touched []]

gVerify{|Task|} _ _ vst = alwaysValid vst

gVisualize{|Task|} _ mbVal vst=:{VSt|currentPath,verifyMask}
	# vis = case mbVal of
		Just {Task|properties}	= [TextFragment properties.TaskProperties.taskDescription.TaskDescription.title]
		Nothing					= []
	= (vis,vst)
	
gEq{|Task|} _ _ _ = False // tasks are never equal

gGetRecordFields{|Task|} _ _ _ fields = fields
gPutRecordFields{|Task|} _ t _ fields = (t,fields)

toTaskFuncs :: !(Task a) -> TaskFuncs a | iTask a
toTaskFuncs {Task|type} = case type of
	NormalTask funcs	= funcs
	ActionTask actionF	= actionF (\{modelValue,localValid} -> UserActions [(ActionOk,if localValid (Just modelValue) Nothing)])

taskException :: !e -> TaskResult a | TC, toString e
taskException e = TaskException (dynamic e) (toString e)