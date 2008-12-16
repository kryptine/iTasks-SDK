implementation module InternaliTasksCommon

// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdArray, StdFunc, StdTuple
import iDataForms, iDataTrivial
import iTasksSettings
import Time
import TSt

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import DrupBasic

iTaskId :: !Int !TaskNr !String -> String
iTaskId userid tasknr postfix 
# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }			// throw away characters not allowed in a file name
| postfix == ""
	| userid < 0	= "iLog_"  <+++ (taskNrToString tasknr) 
	| otherwise		= "iTask_" <+++ (taskNrToString tasknr) 
| userid < 0		= "iLog_"  <+++ (taskNrToString tasknr) <+++ "-" <+++ postfix
| otherwise			= "iTask_" <+++ (taskNrToString tasknr) <+++ "-" <+++ postfix //  MJP:info removed to allow dynamic realloc of users:    <+++ "+"  <+++ userid

// ******************************************************************************************************
// Task creation and printing
// ******************************************************************************************************

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
mkTask taskname mytask = Task (appTaskTSt (mkTaskNoInc taskname mytask) o incTStTaskNr)
where
	incTStTaskNr tst 		= {tst & tasknr = incTaskNr tst.tasknr}

mkTaskNoInc :: !String !(Task a) -> (Task a) | iCreateAndPrint a			// common second part of task wrappers
mkTaskNoInc taskname mytask = Task mkTaskNoInc`
where
	mkTaskNoInc` tst=:{activated,tasknr,userId,options,trace}		
	| not activated						= (createDefault,tst)				// not active, don't call task, return default value
	# (val,tst=:{activated,html})		= appTaskTSt mytask tst				// active, so perform task and get its result
	# tst	= {tst & tasknr = tasknr, options = options, userId = userId}
	| trace || taskname == ""	= (val,tst)									// no trace, just return value
	# tst = {tst & html = TaskTrace {trTaskNr = taskNrToString tasknr, trTaskName = taskname, trActivated = activated, trUserId = userId, trValue = printToString val, trOptions = options} html}
	= (val,tst) 

mkParSubTask :: !String !Int (Task a) -> (Task a)  | iCreateAndPrint a					// two shifts are needed
mkParSubTask name i task = Task mkParSubTask`
where
	mkParSubTask` tst=:{tasknr, options}
	# (v,tst) = appTaskTSt (mkTaskNoInc (name <+++ "." <+++ i) (Task mysubtask)) {tst & tasknr = [i:tasknr],activated = True} // shift task
	= (v,{tst & tasknr = tasknr, options = options})
	where
		mysubtask tst=:{tasknr} = appTaskTSt task {tst & tasknr = [-1:tasknr], activated = True}	// shift once again!


// ******************************************************************************************************
// iTask Storage Utilities
// ******************************************************************************************************

cFormId :: !Options !String !a -> FormId a
cFormId  {tasklife,taskstorage,taskmode} s d = {sFormId  s d & lifespan = tasklife, storage = taskstorage, mode = taskmode} 

sessionFormId :: !Options !String !a -> FormId a
sessionFormId options s d = cFormId options s d <@ if (options.tasklife == LSClient) LSClient LSSession

pageFormId :: !Options !String !a -> FormId a
pageFormId options s d = cFormId options s d <@ if (options.tasklife == LSClient) LSClient LSPage

storageFormId :: !Options !String !a -> FormId a
storageFormId  options s d = cFormId options s d <@ NoForm

// ******************************************************************************************************

instance == GarbageCollect
where
	(==) Collect   Collect 		= True
	(==) NoCollect NoCollect 	= True
	(==) _ _ 					= False

// ******************************************************************************************************
// Task specialization
// ******************************************************************************************************

write{|Task|} write_a task wst
	= write{|*|} (copy_to_string task) wst

read {|Task|} read_a  wst 
	# (Read str i file) = read{|*|} wst
	= Read (deserialize str) i file
where
	deserialize :: .String -> .(Task .a)
	deserialize str = fst (copy_from_string {c \\ c <-: str })

gPrint{|Task|} ga task ps = ps <<- copy_to_string task

gParse{|Task|} ga expr
# mbstring = parseString expr
| isNothing mbstring = Nothing
= Just (fst(copy_from_string {s` \\ s` <-: fromJust mbstring}))
where
	parseString :: Expr -> Maybe String
	parseString expr = gParse{|*|} expr

gUpd{|Task|} gc (UpdSearch 0 _)	  	 c		= (UpdDone, c)								
gUpd{|Task|} gc (UpdSearch cntr val)  c		= (UpdSearch (cntr - 1) val,c)						
gUpd{|Task|} gc (UpdCreate l)        _		
# (mode,default)	= gc (UpdCreate l) undef
= (UpdCreate l, Task (\tst -> (default,tst)))			
gUpd{|Task|} gc mode                 b		= (mode, b)										

gForm{|Task|} gfa (init,formid) hst
= ({value=formid.ival,changed=False,form=[], inputs = []},hst)


