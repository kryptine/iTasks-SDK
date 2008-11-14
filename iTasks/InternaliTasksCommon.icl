implementation module InternaliTasksCommon

// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdArray, StdFunc, StdTuple
import iDataForms, iDataTrivial
import iTasksSettings
import Time

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import DrupBasic

:: TCl a 			= 	TCl !.(Task a)				// task closure, container for a task used for higher order tasks (task which deliver a task)			

showTaskNr :: !TaskNr -> String
showTaskNr [] 		= ""
showTaskNr [i] 		= toString i
showTaskNr [i:is] 	= showTaskNr is <+++ "." <+++ toString i 

iTaskId :: !Int !TaskNr !String -> String
iTaskId userid tasknr postfix 
# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }			// throw away characters not allowed in a file name
| postfix == ""
	| userid < 0	= "iLog_"  <+++ (showTaskNr tasknr) 
	| otherwise		= "iTask_" <+++ (showTaskNr tasknr) 
| userid < 0		= "iLog_"  <+++ (showTaskNr tasknr) <+++ "-" <+++ postfix
| otherwise			= "iTask_" <+++ (showTaskNr tasknr) <+++ "-" <+++ postfix //  MJP:info removed to allow dynamic realloc of users:    <+++ "+"  <+++ userid

deleteAllSubTasks :: ![TaskNr] TSt -> TSt
deleteAllSubTasks [] tst = tst
deleteAllSubTasks [tx:txs] tst=:{hst,userId} 
# hst	= deleteIData  (iTaskId userId (tl tx) "") hst
= deleteAllSubTasks txs {tst & hst = hst}

// ******************************************************************************************************
// Task creation and printing
// ******************************************************************************************************

incTaskNr tst 		= {tst & tasknr = incNr tst.tasknr}

incNr :: !TaskNr -> TaskNr
incNr [] = [0]
incNr [i:is] = [i+1:is]

// mkTask is an important wrapper function which should be wrapped around any task
// It takes care of
//		- deciding whether the task should be called (activated) or not
//		- adding of trace information
//		- generating task numbers in a systematic way
// It is very important that the numbering of the tasks is done systematically
// Every task should have a unique number
// Every sequential task should increase the task number
// If a task j is a subtask of task i, than it will get number i.j in reverse order

mkTask :: !String !(Task a) -> (Task a) | iCreateAndPrint a
mkTask taskname mytask = mkTaskNoInc taskname mytask o incTaskNr

mkTaskNoInc :: !String !(Task a) -> (Task a) | iCreateAndPrint a			// common second part of task wrappers
mkTaskNoInc taskname mytask = mkTaskNoInc`
where
	mkTaskNoInc` tst=:{activated,tasknr,userId,options}		
	| not activated							= (createDefault,tst)	// not active, don't call task, return default value
	# (val,tst=:{activated,trace})			= mytask tst			// active, so perform task and get its result
	# tst	= {tst & tasknr = tasknr, options = options, userId = userId}
	| isNothing trace || taskname == ""		= (val,tst)				// no trace, just return value
	= (val,{tst & trace = Just (InsertTrace activated tasknr userId options taskname (printToString val%(0,60)) (fromJust trace))}) // adjust trace, don't print to long values

mkParSubTask :: !String !Int (Task a) -> (Task a)  | iCreateAndPrint a					// two shifts are needed
mkParSubTask name i task = mkParSubTask`
where
	mkParSubTask` tst=:{tasknr, options}
	# (v,tst) = mkTaskNoInc (name <+++ "." <+++ i) mysubtask {tst & tasknr = [i:tasknr],activated = True} // shift task
	= (v,{tst & tasknr = tasknr, options = options})
	where
		mysubtask tst=:{tasknr} = task {tst & tasknr = [-1:tasknr], activated = True}	// shift once again!

// ******************************************************************************************************
// Trace Insertion ...
// ******************************************************************************************************

InsertTrace :: !Bool !TaskNr !Int !Options String !String ![Trace] -> [Trace]
InsertTrace finished idx who options taskname val trace = InsertTrace` ridx who val trace
where
	InsertTrace` :: !TaskNr !Int !String ![Trace] -> [Trace]
	InsertTrace` [i] 	who str traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr idx <+++ "," <+++ who <+++ "," <+++ taskname)
	# (Trace _ itraces)		= select i traces
	= updateAt` i (Trace (Just (finished,(who,show,options,taskname,str))) itraces)  traces
	InsertTrace` [i:is] who str traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr idx <+++ "," <+++ who <+++ "," <+++ taskname)
	# (Trace ni itraces)	= select i traces
	# nistraces				= InsertTrace` is who str itraces
	= updateAt` i (Trace ni nistraces) traces

	select :: !Int ![Trace] -> Trace
	select i list
	| i < length list = list!!i 
	=  Trace Nothing []

	show 	= idx //showTaskNr idx
	ridx	= reverse idx

	updateAt`:: !Int !Trace ![Trace] -> [Trace]
	updateAt` n x list
	| n < 0		= abort "negative numbers not allowed"
	= updateAt` n x list
	where
		updateAt`:: !Int !Trace ![Trace] -> [Trace]
		updateAt` 0 x []		= [x]
		updateAt` 0 x [y:ys]	= [x:ys]
		updateAt` n x []		= [Trace Nothing []	: updateAt` (n-1) x []]
		updateAt` n x [y:ys]	= [y      			: updateAt` (n-1) x ys]




// ******************************************************************************************************
// iTask Storage Utilities
// ******************************************************************************************************

cFormId :: !Options !String !a -> FormId a
cFormId  {tasklife,taskstorage,taskmode} s d = {sFormId  s d & lifespan = tasklife, storage = taskstorage, mode = taskmode} 

sessionFormId :: !Options !String !a -> FormId a
sessionFormId options s d = cFormId options s d <@ if (options.tasklife == Client) Client Session

pageFormId :: !Options !String !a -> FormId a
pageFormId options s d = cFormId options s d <@ if (options.tasklife == Client) Client Page

storageFormId :: !Options !String !a -> FormId a
storageFormId  options s d = cFormId options s d <@ NoForm

// ******************************************************************************************************

instance == GarbageCollect
where
	(==) Collect   Collect 		= True
	(==) NoCollect NoCollect 	= True
	(==) _ _ 					= False

// ******************************************************************************************************
// TCl specialization
// ******************************************************************************************************

write{|TCl|} write_a (TCl task) wst
	= write{|*|} (copy_to_string task) wst

read {|TCl|} read_a  wst 
	# (Read str i file) = read{|*|} wst
	= Read (TCl  (deserialize str)) i file
where
	deserialize :: .String -> .(Task .a)
	deserialize str = fst (copy_from_string {c \\ c <-: str })

gPrint{|TCl|} ga (TCl task) ps = ps <<- copy_to_string task

gParse{|TCl|} ga expr
# mbstring = parseString expr
| isNothing mbstring = Nothing
= Just (TCl (fst(copy_from_string {s` \\ s` <-: fromJust mbstring})))
where
	parseString :: Expr -> Maybe String
	parseString expr = gParse{|*|} expr

gUpd{|TCl|} gc (UpdSearch _ 0)	  	 c		= (UpdDone, c)								
gUpd{|TCl|} gc (UpdSearch val cnt)  c		= (UpdSearch val (cnt - 2),c)						
gUpd{|TCl|} gc (UpdCreate l)        _		
# (mode,default)	= gc (UpdCreate l) undef
= (UpdCreate l, TCl (\tst -> (default,tst)))			
gUpd{|TCl|} gc mode                 b		= (mode, b)										

gForm{|TCl|} gfa (init,formid) hst
= ({value=formid.ival,changed=False,form=[], inputs = []},hst)


