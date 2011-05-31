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
	, initFun				= initFun
	, editEventFun			= editEventFun
	, evalTaskFun			= evalTaskFun
	}
	
mkInstantTask :: !d (TaskNr *IWorld -> (!TaskResult a,!*IWorld)) -> Task a | descr d
mkInstantTask description iworldfun =
	{ properties			= {TaskProperties|initTaskProperties & taskDescription = toDescr description}
	, mbTaskNr				= Nothing
	, initFun				= \_ iworld -> (TCBasic newMap,iworld)
	, editEventFun			= \_ _ context iworld -> (context,iworld)
	, evalTaskFun			= \taskNr _ _ _ _ _ iworld -> iworldfun taskNr iworld
	}
	
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

JSONEncode{|Task|} _ {properties,mbTaskNr,initFun,editEventFun,evalTaskFun}
	= [JSONArray	[  JSONString "Task"
					:  JSONEncode{|*|} properties
					++ JSONEncode{|*|} mbTaskNr
					++ dynamicJSONEncode initFun
					++ dynamicJSONEncode editEventFun
					++ dynamicJSONEncode evalTaskFun]]
					
JSONDecode{|Task|} _ [JSONArray [JSONString "Task",properties,mbTaskNr,initFun,editEventFun,evalTaskFun]:c]
	# mbTaskProperties		= fromJSON properties
	# mbMbTaskNr			= fromJSON mbTaskNr
	# mbInitFun				= dynamicJSONDecode initFun
	# mbEditEventFun		= dynamicJSONDecode editEventFun
	# mbEvalTaskFun			= dynamicJSONDecode evalTaskFun
	|  isJust mbTaskProperties
	&& isJust mbMbTaskNr
	&& isJust mbInitFun
	&& isJust mbEditEventFun
	&& isJust mbEvalTaskFun
		= (Just	{ properties			= fromJust mbTaskProperties
				, mbTaskNr				= fromJust mbMbTaskNr
				, initFun				= fromJust mbInitFun
				, editEventFun			= fromJust mbEditEventFun
				, evalTaskFun			= fromJust mbEvalTaskFun
				},c)
	| otherwise
		= (Nothing,c)
JSONDecode{|Task|} _ c = (Nothing,c)

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a =	{ properties		= {initTaskProperties & taskDescription = toDescr "return"}
					, mbTaskNr				= Nothing
					, initFun				= undef
					, editEventFun			= undef
					, evalTaskFun			= undef 
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

taskException :: !e -> TaskResult a | TC, toString e
taskException e = TaskException (dynamic e) (toString e)
