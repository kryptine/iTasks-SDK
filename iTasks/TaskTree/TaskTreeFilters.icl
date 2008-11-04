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
collectTaskList pred  (BT bdtg inputs)
= []
collectTaskList pred (DivCode id tree)
= collectTaskList pred tree


determineTaskForTab :: !UserId !TaskNrId !HtmlTree !*HSt -> (![HtmlTag],![InputId],!*HSt)
determineTaskForTab thisuser thistaskid tree hst
	# mytree = determineTaskTree thisuser thistaskid tree
	| isNothing mytree = ([Text "Html code belonging to indicated task could not be found\n"],[],hst)
	# (threadcode,taskname,mainbuts,subbuts,seltask,inputs,hst)	= Filter True thisuser thisuser (fromJust mytree) hst
	| isEmpty threadcode	= (seltask, inputs, hst)
							= (threadcode, inputs, hst)

determineTaskTree :: !UserId !TaskNrId !HtmlTree -> Maybe HtmlTree
determineTaskTree thisuser thistaskid (taskdescr @@: tree) 	
| taskdescr.taskNrId == thistaskid = Just tree
= determineTaskTree thisuser thistaskid tree									
determineTaskTree thisuser thistaskid (ntaskuser -@: tree)
| thisuser == ntaskuser = Nothing
= determineTaskTree thisuser thistaskid tree
determineTaskTree thisuser thistaskid (tree1 +|+ tree2)
# ntree1		= determineTaskTree thisuser thistaskid tree1
| isJust ntree1 = ntree1
= determineTaskTree thisuser thistaskid tree2
determineTaskTree  thisuser thistaskid  (tree1 +-+ tree2)
# ntree1		= determineTaskTree thisuser thistaskid tree1
| isJust ntree1 = ntree1
= determineTaskTree thisuser thistaskid tree2
determineTaskTree thisuser thistaskid  (BT bdtg inputs)
= Nothing
determineTaskTree thisuser thistaskid (DivCode id tree)
= determineTaskTree thisuser thistaskid tree

noFilter :: !HtmlTree -> [HtmlTag]
noFilter (BT body inputs) 	= body
noFilter (_ @@: html) 		= noFilter html
noFilter (_ -@: html) 		= noFilter html
noFilter (htmlL +-+ htmlR) 	= [noFilter htmlL  <=>  noFilter htmlR]
noFilter (htmlL +|+ htmlR) 	= noFilter htmlL <|.|> noFilter htmlR
noFilter (DivCode str html) = noFilter html


Filter :: !Bool !UserId !UserId !HtmlTree !*HSt -> *(![HtmlTag],![HtmlTag],![HtmlTag],![HtmlTag],![HtmlTag],![InputId],!*HSt)
Filter wholepage thisUser thrOwner tree hst
# startuser			= if wholepage defaultUser thrOwner
# (threadcode,threadinputs,accu) = collect thisUser startuser [](initialTaskDescription @@: tree)  // KLOPT DIT WEL ??
| isEmpty accu		= (threadcode,[],[],[],[],threadinputs,hst)
# accu				= sortBy (\(i,_,_,_,_) (j,_,_,_,_) -> i < j) accu
# (workflownames,subtasks) 						= unziptasks accu
# ((mainSelected,mainButtons,chosenMain),hst) 	= mkTaskButtons True ("User " <+++ thisUser) thisUser [] 
														(initialOptions thisUser Session) workflownames hst 
# (subtasksnames,tcode)							= unzipsubtasks (subtasks !! mainSelected)
# ((taskSelected,subButtons,chosenTask),hst) 	= mkTaskButtons False ("User " <+++ thisUser <+++ "subtask" <+++ mainSelected) thisUser [] 
														(initialOptions thisUser Session) subtasksnames hst 
# subButtons		= if (length subtasksnames > 1) subButtons []
# (selcode, selinputs)							= tcode!!taskSelected
= (threadcode,[showMainLabel chosenMain, showTrace " / ", showLabel chosenTask], mainButtons, subButtons, selcode, selinputs, hst)
where
	unziptasks :: ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![HtmlTag],![InputId])] -> (![WorkflowLabel],![[(!ProcessNr,!WorkflowLabel,!TaskLabel,![HtmlTag],![InputId])]])
	unziptasks [] 			= ([],[])
	unziptasks all=:[(pid,wlabel,tlabel,tcode,tinputs):tasks] 
	# (wsubtask,other) 		= span (\(mpid,_,_,_,_) ->  mpid == pid) all 
	# (wlabels,wsubtasks)	= unziptasks other
	= ([wlabel:wlabels],[wsubtask:wsubtasks])

	unzipsubtasks :: ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![HtmlTag],![InputId])] -> (![TaskLabel],![(![HtmlTag],![InputId])])
	unzipsubtasks []		= ([],[])
	unzipsubtasks [(pid,wlabel,tlabel,tcode,tinputs):subtasks]		
	# (labels,codes)		= unzipsubtasks subtasks
	= ([tlabel:labels],[(tcode,tinputs):codes])

	initialOptions ::  !UserId !Lifespan  -> Options 
	initialOptions thisUser location 
		=	{ tasklife 		= if (thisUser >= 0) location Session 
			, taskstorage 	= PlainString
			, taskmode 		= Edit 
			, gc			= Collect
			}

	initialTaskDescription
		=	{ delegatorId	= 0								// id of the work delegator
			, taskWorkerId	= 0								// id of worker on the task
			, taskNrId		= "0"							// tasknr as string
			, processNr		= 0								// entry in process table
			, worflowLabel	= defaultWorkflowName			// name of the workflow
			, taskLabel		= "main"						// name of the task
			, taskPriority	= NormalPriority
			, timeCreated	= Time 0
			}							

collect :: !UserId !UserId ![(!ProcessNr,!WorkflowLabel,!TaskLabel,![HtmlTag],![InputId])] !HtmlTree -> (![HtmlTag],![InputId],![(!ProcessNr,!WorkflowLabel,!TaskLabel,![HtmlTag],![InputId])])
collect thisuser taskuser accu (description @@: tree) 								// collect returns the wanted code, and the remaining code
	# (myhtml,myinputs,accu)= collect thisuser description.taskWorkerId accu tree	// collect all code of this user belonging to this task
	| thisuser == description.taskWorkerId && not (isEmpty myhtml)
							= ([],[],[(description.processNr,description.worflowLabel,description.taskLabel,myhtml,myinputs):accu]) //TODO: Add inputs to accu
	| otherwise				= ([],[],accu)
collect thisuser taskuser accu (nuser -@: tree)
	| thisuser == nuser 	= ([],[],accu)
	| otherwise				= collect thisuser taskuser accu tree
collect thisuser taskuser accu (tree1 +|+ tree2)
	# (lhtml,linputs,accu)	= collect thisuser taskuser accu tree1
	# (rhtml,rinputs,accu)	= collect thisuser taskuser accu tree2
	= (lhtml <|.|> rhtml,linputs ++ rinputs, accu)
collect thisuser taskuser accu (tree1 +-+ tree2)
	# (lhtml,linputs,accu)	= collect thisuser taskuser accu tree1
	# (rhtml,rinputs,accu)	= collect thisuser taskuser accu tree2
	= ([lhtml <=> rhtml],linputs ++ rinputs, accu)
collect thisuser taskuser accu (BT bdtg inputs)
	| thisuser == taskuser	= (bdtg,inputs,accu)
	| otherwise				= ([],[], accu)
collect thisuser taskuser accu (DivCode id tree)
	# (html,inputs,accu)	= collect thisuser taskuser accu tree
	| thisuser == taskuser 	= ([DivTag [IdAttr id, ClassAttr "itasks-thread"] html],inputs,accu)
	= ([],[],accu)

