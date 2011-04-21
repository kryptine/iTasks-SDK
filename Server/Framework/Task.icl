implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, Types, GenRecord, HTTP, Util
import GenVisualize
from TSt import :: TSt
from iTasks import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

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
	
//Increase the task nr
incTaskNr :: !TaskNr -> TaskNr
incTaskNr [] = [0]
incTaskNr [i:is] = [i+1:is]
	
//Applies given function to the result if task is finished
mapTaskResult :: !(a -> b) !(TaskResult a) -> TaskResult b
mapTaskResult f (TaskFinished x)		= TaskFinished (f x) 
mapTaskResult f (TaskBusy)				= TaskBusy
mapTaskResult f (TaskException e str)	= TaskException e str

mapTask :: !(a -> b) !(Task a) -> Task b
mapTask f t=:{taskFuncCommit} = {t & taskFuncCommit = appFst (mapTaskResult f) o taskFuncCommit}

derive gUpdate		ManagerProperties, TaskPriority, RunningTaskStatus
derive gDefaultMask	ManagerProperties, TaskPriority, RunningTaskStatus
derive gVerify		ManagerProperties, TaskPriority, RunningTaskStatus
derive gVisualize	ManagerProperties, TaskPriority, RunningTaskStatus
derive JSONEncode 	TaskResult
derive JSONDecode 	TaskResult
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
JSONEncode{|TUIInteractive|} _	= abort "not implemented"
JSONDecode{|TUIInteractive|} _	= abort "not implemented"
JSONEncode{|TUIParallel|} _		= abort "not implemented"
JSONDecode{|TUIParallel|} _		= abort "not implemented"
JSONEncode{|TUIResult|} _		= abort "not implemented"
JSONDecode{|TUIResult|} _		= abort "not implemented"
JSONEncode{|TUIDef|} _			= abort "not implemented"
JSONDecode{|TUIDef|} _			= abort "not implemented"

JSONEncode{|Task|} _ {properties,mbTaskNr,taskFuncEdit,taskFuncCommit,mbInteractiveLayout,mbParallelLayout,mbResultLayout}
	= [JSONArray	[  JSONString "Task"
					:  JSONEncode{|*|} properties
					++ JSONEncode{|*|} mbTaskNr
					++ dynamicJSONEncode taskFuncEdit
					++ dynamicJSONEncode taskFuncCommit
					++ JSONEncode{|*|} mbInteractiveLayout
					++ JSONEncode{|*|} mbParallelLayout
					++ JSONEncode{|*|} mbResultLayout]]
					
JSONDecode{|Task|} _ [JSONArray [JSONString "Task",properties,mbTaskNr,taskFuncEdit,taskFuncCommit,mbInteractiveLayout,mbParallelLayout,mbResultLayout]:c]
	# mbTaskProperties		= fromJSON properties
	# mbMbTaskNr			= fromJSON mbTaskNr
	# mbTaskFuncEdit		= dynamicJSONDecode taskFuncEdit
	# mbTaskFuncCommit		= dynamicJSONDecode taskFuncCommit
	# mbMbInteractiveLayout	= fromJSON mbInteractiveLayout
	# mbMbParallelLayout	= fromJSON mbParallelLayout
	# mbMbResultLayout		= fromJSON mbResultLayout
	|  isJust mbTaskProperties
	&& isJust mbMbTaskNr
	&& isJust mbTaskFuncEdit
	&& isJust mbTaskFuncCommit
	&& isJust mbMbInteractiveLayout
	&& isJust mbMbParallelLayout
	&& isJust mbMbResultLayout
		= (Just	{ properties			= fromJust mbTaskProperties
				, mbTaskNr				= fromJust mbMbTaskNr
				, taskFuncEdit			= fromJust mbTaskFuncEdit
				, taskFuncCommit		= fromJust mbTaskFuncCommit
				, mbInteractiveLayout	= fromJust mbMbInteractiveLayout
				, mbParallelLayout		= fromJust mbMbParallelLayout
				, mbResultLayout		= fromJust mbMbResultLayout
				},c)
	| otherwise
		= (Nothing,c)
JSONDecode{|Task|} _ c = (Nothing,c)

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a =	{ properties		= {initTaskProperties & taskDescription = toDescr "return"}
					, mbTaskNr			= Nothing
					, taskFuncEdit		= id
					, taskFuncCommit	= \tst -> (TaskFinished a,tst)
					, mbInteractiveLayout	= Nothing
					, mbParallelLayout	= Nothing
					, mbResultLayout		= Nothing
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
