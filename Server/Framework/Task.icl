implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, Types, dynamic_string, Base64, HTTP, Util
import GenVisualize
from TSt 		import :: TSt

taskTitle :: !(Task a) -> String
taskTitle task = task.Task.properties.taskDescription.TaskDescription.title

taskDescription	:: !(Task a) -> String
taskDescription task = task.Task.properties.taskDescription.TaskDescription.description

taskUser :: !(Task a) -> User
taskUser {Task|containerType} = case containerType of
	DetachedTask {worker} _	= worker
	_						= AnyUser

taskProperties :: !(Task a) -> TaskProperties
taskProperties {Task|properties} = properties

managerProperties :: !(Task a) -> ManagerProperties
managerProperties {Task|containerType} = case containerType of
	DetachedTask props _	= props
	_						= initManagerProperties
	
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
taskNrFromString string	= reverse (parseTaskNr` [char \\ char <-: string])
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

derive JSONEncode TaskResult, TaskContainerType
derive JSONDecode TaskResult, TaskContainerType
derive bimap Maybe, (,)

// JSON (de)serialisation of menus not needed because only functions generating menus (no actual menu structures) are serialised
JSONEncode{|Menu|} _		= abort "not implemented"
JSONEncode{|MenuItem|} _	= abort "not implemented"
JSONDecode{|Menu|} _		= abort "not implemented"
JSONDecode{|MenuItem|} _	= abort "not implemented"

JSONEncode{|Task|} _ {properties,containerType,mbTaskNr,taskFuncEdit,taskFuncCommit}
	= [JSONArray	[  JSONString "Task"
					:  JSONEncode{|*|} properties
					++ JSONEncode{|*|} containerType
					++ JSONEncode{|*|} mbTaskNr
					++ encodeFunc taskFuncEdit
					++ encodeFunc taskFuncCommit]]
					
JSONDecode{|Task|} _ [JSONArray [JSONString "Task",properties,containerType,mbTaskNr,taskFuncEdit,taskFuncCommit]:c]
	# mbTaskProperties		= fromJSON properties
	# mbContainerType		= fromJSON containerType
	# mbMbTaskNr			= fromJSON mbTaskNr
	# mbTaskFuncEdit		= decodeFunc taskFuncEdit
	# mbTaskFuncCommit		= decodeFunc taskFuncCommit
	|  isJust mbTaskProperties
	&& isJust mbContainerType
	&& isJust mbMbTaskNr
	&& isJust mbTaskFuncEdit
	&& isJust mbTaskFuncCommit
		= (Just	{ properties	= fromJust mbTaskProperties
				, containerType		= fromJust mbContainerType
				, formWidth			= Nothing
				, mbTaskNr			= fromJust mbMbTaskNr
				, taskFuncEdit		= fromJust mbTaskFuncEdit
				, taskFuncCommit	= fromJust mbTaskFuncCommit
				},c)
	| otherwise
		= (Nothing,c)
JSONDecode{|Task|} _ c = (Nothing,c)

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a =	{ properties		= {initTaskProperties & taskDescription = toDescr "return"}
					, containerType		= InBodyTask
					, formWidth			= Nothing
					, mbTaskNr			= Nothing
					, taskFuncEdit		= id
					, taskFuncCommit	= \tst -> (TaskFinished a,tst)
					}
gUpdate{|Task|} _ (UDSearch t) ust = basicSearch t (\_ t -> t) ust

gDefaultMask{|Task|} _ _ = [Touched []]

gVerify{|Task|} _ _ vst = alwaysValid vst

gVisualize{|Task|} _ mbVal vst=:{VSt|currentPath,verifyMask}
	# vis = case mbVal of
		Just {Task|properties}	= [TextFragment properties.TaskProperties.taskDescription.TaskDescription.title]
		Nothing						= []
	= (vis,vst)
	
gEq{|Task|} _ _ _ = False // tasks are never equal

taskException :: !e -> TaskResult a | TC, toString e
taskException e = TaskException (dynamic e) (toString e)
