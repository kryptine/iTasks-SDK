implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, Types, dynamic_string, Base64, HTTP, Util
import GenVisualize
from TSt import :: TSt

taskTitle :: !(Task a) -> String
taskTitle task = task.taskProperties.taskDescription.TaskDescription.title

taskDescription	:: !(Task a) -> String
taskDescription task = task.taskProperties.taskDescription.TaskDescription.description

taskUser :: !(Task a) -> User
taskUser task = task.taskProperties.worker

taskProperties :: !(Task a) -> ManagerProperties
taskProperties task = task.taskProperties
	
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
mapTaskResult f (TaskFinished x)	= TaskFinished (f x) 
mapTaskResult f (TaskBusy)			= TaskBusy
mapTaskResult f (TaskException e)	= TaskException e

mapTask :: !(a -> b) !(Task a) -> Task b
mapTask f t=:{taskFuncCommit} = {t & taskFuncCommit = appFst (mapTaskResult f) o taskFuncCommit}

derive JSONEncode TaskResult, TaskContainerType
derive JSONDecode TaskResult, TaskContainerType
derive bimap Maybe, (,)

// JSON (de)serialisation of menus not needed because only function generating menus are serialised
JSONEncode{|Menu|} _		= abort "not implemented"
JSONEncode{|MenuItem|} _	= abort "not implemented"
JSONDecode{|Menu|} _		= abort "not implemented"
JSONDecode{|MenuItem|} _	= abort "not implemented"

JSONEncode{|Task|} _ {taskProperties,containerType,mbTaskNr,mbMenuGenFunc,taskFuncEdit,taskFuncCommit}
	= [JSONArray	[  JSONString "Task"
					:  JSONEncode{|*|} taskProperties
					++ JSONEncode{|*|} containerType
					++ JSONEncode{|*|} mbTaskNr
					++ encodeFunc mbMenuGenFunc
					++ encodeFunc taskFuncEdit
					++ encodeFunc taskFuncCommit]]
					
encodeFunc f = [JSONString (base64Encode (copy_to_string f))]

JSONDecode{|Task|} _ [JSONArray [JSONString "Task",taskProperties,containerType,mbTaskNr,mbMenuGenFunc,taskFuncEdit,taskFuncCommit]:c]
	# mbTaskProperties		= fromJSON taskProperties
	# mbContainerType		= fromJSON containerType
	# mbMbTaskNr			= fromJSON mbTaskNr
	# mbMbMenuGenFunc		= decodeFunc mbMenuGenFunc
	# mbTaskFuncEdit		= decodeFunc taskFuncEdit
	# mbTaskFuncCommit		= decodeFunc taskFuncCommit
	|  isJust mbTaskProperties
	&& isJust mbContainerType
	&& isJust mbMbTaskNr
	&& isJust mbMbMenuGenFunc
	&& isJust mbTaskFuncEdit
	&& isJust mbTaskFuncCommit
		= (Just	{ taskProperties	= fromJust mbTaskProperties
				, containerType		= fromJust mbContainerType
				, formWidth			= Nothing
				, mbTaskNr			= fromJust mbMbTaskNr
				, mbMenuGenFunc		= fromJust mbMbMenuGenFunc
				, taskFuncEdit		= fromJust mbTaskFuncEdit
				, taskFuncCommit	= fromJust mbTaskFuncCommit
				},c)
	| otherwise
		= (Nothing,c)
JSONDecode{|Task|} _ c = (Nothing,c)

decodeFunc (JSONString str)	= Just (fst(copy_from_string {s` \\ s` <-: base64Decode str}))
decodeFunc _				= Nothing

gUpdate{|Task|} fx UDCreate ust
	# (a,ust) = fx UDCreate ust
	= basicCreate (defaultTask a) ust
where
	defaultTask a =	{ taskProperties	= {ManagerProperties | initManagerProperties & taskDescription = toDescr "return"}
					, containerType		= InParallelBody
					, formWidth			= Nothing
					, mbTaskNr			= Nothing
					, mbMenuGenFunc		= Nothing
					, taskFuncEdit		= id
					, taskFuncCommit	= \tst -> (TaskFinished a,tst)
					}
gUpdate{|Task|} _ (UDSearch t) ust = basicSearch t (\_ t -> t) ust

gDefaultMask{|Task|} _ _ = [Touched []]

gVerify{|Task|} _ _ vst = alwaysValid vst

gVisualize{|Task|} _ mbVal vst=:{VSt|currentPath,verifyMask}
	# vis = case mbVal of
		Just {taskProperties}	= [TextFragment taskProperties.ManagerProperties.taskDescription.TaskDescription.title]
		Nothing					= []
	= (vis,vst)
	
gEq{|Task|} _ _ _ = False // tasks are never equal

noMenu :: ActionMenu
noMenu = const []

staticMenu	:: !MenuDefinition -> ActionMenu
staticMenu def = const def
