implementation module Task

import StdClass, StdArray, StdTuple, StdInt, StdList, StdFunc, StdBool, StdMisc, HTML, SystemTypes, GenRecord, HTTP, Map, Util
import GenVisualize, iTaskClass
from TaskContext		import :: TaskContextTree(..), :: SubTaskContext, :: SubTaskId, :: SubTaskOrder, :: ParallelMeta
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
	
mkInstantTask :: (TaskNr *IWorld -> (!TaskResult a,!*IWorld)) -> Task a |  iTask a
mkInstantTask iworldfun =
	{ Task
	| initFun			= \_ iworld -> (TCEmpty,iworld)
	, editFun			= \_ _ context iworld -> (context,iworld)
	, evalFun			= evalOnce iworldfun
	, layout			= Nothing
	}
where
	evalOnce f taskNo _ _ _ _ context=:(TCBasic enc True) iworld = case fromJSON enc of
		(Just res)	= (TaskStable res NoRep context, iworld)
		Nothing		= (taskException "Corrupt task result", iworld)
	evalOnce f taskNo _ _ _ _ _ iworld = case f taskNo iworld of
		(TaskStable res _ _, iworld)	= (TaskStable res NoRep (TCBasic (toJSON res) True), iworld)
		(TaskException e s, iworld)		= (TaskException e s, iworld)
		(_,iworld)						= (taskException "Instant task did not complete instantly", iworld)

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
stepEvent _ (Just (LuckyEvent e)) = Just (LuckyEvent e)
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
gGridRows{|Task|} _ _ _ _ = Nothing	
gEq{|Task|} _ _ _ = True // tasks are always equal

gGetRecordFields{|Task|} _ _ _ fields = fields
gPutRecordFields{|Task|} _ t _ fields = (t,fields)

taskException :: !e -> TaskResult a | TC, toString e
taskException e = TaskException (dynamic e) (toString e)