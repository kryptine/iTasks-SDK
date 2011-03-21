implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, Types, dynamic_string, Base64, HTTP, Util
import GenVisualize
from TSt import :: TSt

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

mapTaskContainer :: !(a -> b) !(TaskContainer a) -> TaskContainer b
mapTaskContainer f container = case container of
	DetachedTask p m t	= DetachedTask p m (mapTask f t)
	WindowTask w m t	= WindowTask w m (mapTask f t)
	DialogTask w t		= DialogTask w (mapTask f t)
	InBodyTask t		= InBodyTask (mapTask f t)
	HiddenTask t		= HiddenTask (mapTask f t)
	
fromContainerToTask	:: !(TaskContainer a) -> (!Task a,TaskContainerType)
fromContainerToTask container = case container of
	DetachedTask p m t	= (t,CTDetached p m)
	WindowTask w m t	= (t,CTWindow w m)
	DialogTask w t		= (t,CTDialog w)
	InBodyTask t		= (t,CTInBody)
	HiddenTask t		= (t,CTHidden)
	
fromContainerToTaskParam :: !(ParamTaskContainer a b) -> (!a -> Task b,TaskContainerType)
fromContainerToTaskParam container = case container of
	DetachedPTask p m t	= (t,CTDetached p m)
	WindowPTask w m t	= (t,CTWindow w m)
	DialogPTask w t		= (t,CTDialog w)
	InBodyPTask t		= (t,CTInBody)
	HiddenPTask t		= (t,CTHidden)
	
applyParam :: !a !(ParamTaskContainer a b) -> TaskContainer b
applyParam a container = case container of
	DetachedPTask p m	ct = DetachedTask p m	(ct a)
	WindowPTask w m		ct = WindowTask w m		(ct a)
	DialogPTask w		ct = DialogTask w		(ct a)
	InBodyPTask			ct = InBodyTask			(ct a)
	HiddenPTask			ct = HiddenTask			(ct a)
	
changeTask :: !((Task a) -> Task a) !(TaskContainer a) -> TaskContainer a
changeTask f container = case container of
	DetachedTask p m t	= DetachedTask p m	(f t)
	WindowTask w m t	= WindowTask w m	(f t)
	DialogTask w t		= DialogTask w		(f t)
	InBodyTask t		= InBodyTask		(f t)
	HiddenTask t		= HiddenTask		(f t)

derive JSONEncode	TaskContainer
derive JSONDecode	TaskContainer
derive gUpdate		TaskContainer, ManagerProperties, TaskPriority, RunningTaskStatus
derive gDefaultMask	TaskContainer, ManagerProperties, TaskPriority, RunningTaskStatus
derive gVerify		TaskContainer, ManagerProperties, TaskPriority, RunningTaskStatus
derive gVisualize	TaskContainer, ManagerProperties, TaskPriority, RunningTaskStatus
derive gEq			TaskContainer
derive JSONEncode 	TaskResult, TaskContainerType
derive JSONDecode 	TaskResult, TaskContainerType
derive bimap Maybe, (,)

// Generic functions for menus not needed because only functions generating menus (no actual menu structures) are serialised
JSONEncode{|Menu|} _		= abort "not implemented"
JSONEncode{|MenuItem|} _	= abort "not implemented"
JSONDecode{|Menu|} _		= abort "not implemented"
JSONDecode{|MenuItem|} _	= abort "not implemented"
gUpdate{|Menu|} _ _			= abort "not implemented"
gUpdate{|MenuItem|} _ _		= abort "not implemented"
gDefaultMask{|Menu|} _		= abort "not implemented"
gDefaultMask{|MenuItem|} _	= abort "not implemented"
gVerify{|Menu|} _ _			= abort "not implemented"
gVerify{|MenuItem|} _ _		= abort "not implemented"
gVisualize{|Menu|} _ _		= abort "not implemented"
gVisualize{|MenuItem|} _ _	= abort "not implemented"
gEq{|Menu|} _ _				= abort "not implemented"
gEq{|MenuItem|} _ _			= abort "not implemented"

JSONEncode{|Task|} _ {properties,mbTaskNr,taskFuncEdit,taskFuncCommit}
	= [JSONArray	[  JSONString "Task"
					:  JSONEncode{|*|} properties
					++ JSONEncode{|*|} mbTaskNr
					++ encodeFunc taskFuncEdit
					++ encodeFunc taskFuncCommit]]
					
JSONDecode{|Task|} _ [JSONArray [JSONString "Task",properties,mbTaskNr,taskFuncEdit,taskFuncCommit]:c]
	# mbTaskProperties		= fromJSON properties
	# mbMbTaskNr			= fromJSON mbTaskNr
	# mbTaskFuncEdit		= decodeFunc taskFuncEdit
	# mbTaskFuncCommit		= decodeFunc taskFuncCommit
	|  isJust mbTaskProperties
	&& isJust mbMbTaskNr
	&& isJust mbTaskFuncEdit
	&& isJust mbTaskFuncCommit
		= (Just	{ properties		= fromJust mbTaskProperties
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
