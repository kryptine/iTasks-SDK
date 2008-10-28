implementation module TaskTreeFilters

// *********************************************************************************************************************************
// This module contains filters for filtering information from a TaskTree
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************

import StdEnv
import iDataFormlib
import InternaliTasksCommon, iTasksHtmlSupport

collectTaskList :: !(TaskDescription -> Bool) !HtmlTree -> [TaskDescription] 	// returns who created the task, the tasknr, and taskname
collectTaskList pred (taskdescr @@: tree) 	
# collected				= collectTaskList pred tree									
| pred taskdescr		= [taskdescr:collected]
= collected
collectTaskList pred (ntaskuser -@: tree)
= collectTaskList pred tree
collectTaskList pred (tree1 +|+ tree2)
# collection1	= collectTaskList pred tree1
# collection2	= collectTaskList pred tree2
= collection1 ++ collection2
collectTaskList pred  (tree1 +-+ tree2)
# collection1	= collectTaskList pred tree1
# collection2	= collectTaskList pred tree2
= collection1 ++ collection2
collectTaskList pred  (BT bdtg)
= []
collectTaskList pred (DivCode id tree)
= collectTaskList pred tree

initialOptions ::  !UserId !Lifespan  -> !Options 
initialOptions thisUser location 
	=	{ tasklife 		= if (thisUser >= 0) location Session 
		, taskstorage 	= PlainString
		, taskmode 		= Edit 
		, gc			= Collect
		}

noFilter :: !HtmlTree -> HtmlCode
noFilter (BT body) 			= body
noFilter (_ @@: html) 		= noFilter html
noFilter (_ -@: html) 		= noFilter html
noFilter (htmlL +-+ htmlR) 	= [noFilter htmlL  <=>  noFilter htmlR]
noFilter (htmlL +|+ htmlR) 	= noFilter htmlL <|.|> noFilter htmlR
noFilter (DivCode str html) = noFilter html

initialTaskDescription
	=	{ delegatorId	= 0								// id of the work delegator
		, taskWorkerId	= 0								// id of worker on the task
		, taskNrId		= "0"							// tasknr as string
		, processNr		= 0								// entry in process table
		, worflowLabel	= defaultWorkflowName			// name of the workflow
		, taskLabel		= "main"						// name of the task
		}							

Filter :: !Bool !UserId !UserId !HtmlTree !*HSt -> *(![BodyTag],![BodyTag],![BodyTag],![BodyTag],![BodyTag],!*HSt)
Filter wholepage thisUser thrOwner tree hst
# startuser			= if wholepage defaultUser thrOwner
# (threadcode,accu) = collect thisUser startuser [](initialTaskDescription @@: tree)  // KLOPT DIT WEL ??
| isEmpty accu		= (threadcode,[],[],[],[],hst)
# accu				= sortBy (\(i,_,_,_) (j,_,_,_) -> i < j) accu
# (workflownames,subtasks) 						= unziptasks accu
# ((mainSelected,mainButtons,chosenMain),hst) 	= mkTaskButtons True ("User " <+++ thisUser) thisUser [] 
														(initialOptions thisUser Session) workflownames hst 
# (subtasksnames,tcode)							= unzipsubtasks (subtasks!!mainSelected)
# ((taskSelected,subButtons,chosenTask),hst) 	= mkTaskButtons False ("User " <+++ thisUser <+++ "subtask" <+++ mainSelected) thisUser [] 
														(initialOptions thisUser Session) subtasksnames hst 
# subButtons		= if (length subtasksnames > 1) subButtons []
= (threadcode,[showMainLabel chosenMain, showTrace " / ", showLabel chosenTask],mainButtons,subButtons,tcode!!taskSelected,hst)
where
	unziptasks :: ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])] -> (![WorkflowLabel],![[(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])]])
	unziptasks [] 			= ([],[])
	unziptasks all=:[(pid,wlabel,tlabel,tcode):tasks] 
	# (wsubtask,other) 		= span (\(mpid,_,_,_) ->  mpid == pid) all 
	# (wlabels,wsubtasks)	= unziptasks other
	= ([wlabel:wlabels],[wsubtask:wsubtasks])

	unzipsubtasks :: ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])] -> (![TaskLabel],![[BodyTag]])
	unzipsubtasks []		= ([],[])
	unzipsubtasks [(pid,wlabel,tlabel,tcode):subtasks]		
	# (labels,codes)		= unzipsubtasks subtasks
	= ([tlabel:labels],[tcode:codes])

collect :: !UserId !UserId ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])] !HtmlTree -> (![BodyTag],![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])])
collect thisuser taskuser accu (description @@: tree) 	// collect returns the wanted code, and the remaining code
# (myhtml,accu)	= collect thisuser description.taskWorkerId accu tree									// collect all code of this user belonging to this task
| thisuser == description.taskWorkerId && not (isEmpty myhtml)
						= ([],[(description.processNr,description.worflowLabel,description.taskLabel,myhtml):accu])
| otherwise				= ([],accu)
collect thisuser taskuser accu (nuser -@: tree)
| thisuser == nuser 	= ([],accu)
| otherwise				= collect thisuser taskuser accu tree
collect thisuser taskuser accu (tree1 +|+ tree2)
# (lhtml,accu)	= collect thisuser taskuser accu tree1
# (rhtml,accu)	= collect thisuser taskuser accu tree2
= (lhtml <|.|> rhtml,accu)
collect thisuser taskuser accu (tree1 +-+ tree2)
# (lhtml,accu)	= collect thisuser taskuser accu tree1
# (rhtml,accu)	= collect thisuser taskuser accu tree2
= ([lhtml <=> rhtml],accu)
collect thisuser taskuser accu (BT bdtg)
| thisuser == taskuser	= (bdtg,accu)
| otherwise				= ([],accu)
collect thisuser taskuser accu (DivCode id tree)
# (html,accu)			= collect thisuser taskuser accu tree
| thisuser == taskuser 	= (mkDiv True id html,accu)
= ([],accu)

mkDiv :: !Bool !String !HtmlCode -> HtmlCode
mkDiv False id bodytag = bodytag
mkDiv True id bodytag = [Div [`Div_Std [Std_Id id, Std_Class "thread"]] bodytag]

