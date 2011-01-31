implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdMisc, Html, Types, dynamic_string, Base64, Http
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
	iTaskId tasknr postfix 
		# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }		// throw away characters not allowed in a file name
		| postfix == ""		= "iTask_" +++ (taskNrToString tasknr) 
		| otherwise			= "iTask_" +++ (taskNrToString tasknr) +++ "-" +++ postfix

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

derive JSONEncode TaskResult
derive JSONDecode TaskResult
derive bimap Maybe, (,)

JSONEncode{|Task|} _ t						= [JSONString (base64Encode (copy_to_string t))]
JSONDecode{|Task|} _ [JSONString string:c]	= (Just (fst(copy_from_string {s` \\ s` <-: base64Decode string})) ,c) 
JSONDecode{|Task|} _ c						= (Nothing,c)

gUpdate{|Task|} fx _ ust=:{mode=UDCreate}
	# (a,ust) = fx (abort "Task create with undef") ust
	=	(	{ taskProperties	= {ManagerProperties | initManagerProperties & taskDescription = toDescr "return"}
			, groupedProperties	= initGroupedProperties
			, mbTaskNr			= Nothing
			, mbMenuGenFunc		= Nothing
			, taskFunc			= \tst -> (TaskFinished a,tst)
			}
		, ust)
gUpdate{|Task|} _ x ust = (x,ust)
gVerify{|Task|} _ _ vst = vst

gVisualize{|Task|} _ mbVal vst=:{VSt|currentPath, updateMask, verifyMask}
	# vis = case mbVal of
		Just {taskProperties}	= [TextFragment taskProperties.ManagerProperties.taskDescription.TaskDescription.title]
		Nothing					= []
	= (vis,vst)
	
gEq{|Task|} _ _ _ = False // tasks are never equal
