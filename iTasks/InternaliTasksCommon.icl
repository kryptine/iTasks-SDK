implementation module InternaliTasksCommon

// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdEnv
import iDataForms, iDataTrivial
import Time
import TSt

import dynamic_string, graph_to_string_with_descriptors, graph_to_sapl_string
import DrupBasic

iTaskId :: !TaskNr !String -> String
iTaskId tasknr postfix 
	# postfix	=	{ c \\ c <-: postfix | not (isMember c ['\\\"/:*?<>|"']) }		// throw away characters not allowed in a file name
	| postfix == ""		= "iTask_" +++ (taskNrToString tasknr) 
	| otherwise			= "iTask_" +++ (taskNrToString tasknr) +++ "-" +++ postfix


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


