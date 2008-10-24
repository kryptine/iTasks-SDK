implementation module TaskTreeFilters

// *********************************************************************************************************************************
// This module contains filters for filtering information from a TaskTree
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************

import StdEnv
import iDataFormlib
import InternaliTasksCommon, iTasksHtmlSupport

/*
ncollect :: !UserId !UserId !HtmlTree -> (![BodyTag],![(!ProcessNr,!WorkflowLabel,!TaskLabel,![BodyTag])])
ncollect thisuser taskuser htmltree = ncollect` thisuser taskuser [] ((taskuser,0,defaultWorkflowName,"main") @@: htmltree)
where
	ncollect` thisuser taskuser accu ((nuserid,processnr,workflowLabel,taskname) @@: tree) 	// collect returns the wanted code, and the remaining code
	# (myhtml,accu)	= ncollect` thisuser nuserid accu tree									// collect all code of this user belonging to this task
	| thisuser == nuserid && not (isEmpty myhtml)
							= ([],[(processnr,workflowLabel,taskname,myhtml):accu])
	| otherwise				= ([],accu)
	ncollect` thisuser taskuser accu (nuser -@: tree)
	| thisuser == nuser 	= ([],accu)
	| otherwise				= ncollect` thisuser taskuser accu tree
	ncollect` thisuser taskuser accu (tree1 +|+ tree2)
	# (lhtml,accu)	= ncollect` thisuser taskuser accu tree1
	# (rhtml,accu)	= ncollect` thisuser taskuser accu tree2
	= (lhtml <|.|> rhtml,accu)
	ncollect` thisuser taskuser accu (tree1 +-+ tree2)
	# (lhtml,accu)	= ncollect` thisuser taskuser accu tree1
	# (rhtml,accu)	= ncollect` thisuser taskuser accu tree2
	= ([lhtml <=> rhtml],accu)
	ncollect` thisuser taskuser accu (BT bdtg)
	| thisuser == taskuser	= (bdtg,accu)
	| otherwise				= ([],accu)
	ncollect` thisuser taskuser accu (DivCode id tree)
	# (html,accu)			= ncollect` thisuser taskuser accu tree
	| thisuser == taskuser 	= (mkDiv True id html,accu)
	= ([],accu)

*/
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


Filter :: !Bool !UserId !UserId !HtmlTree !*HSt -> *(![BodyTag],![BodyTag],![BodyTag],![BodyTag],![BodyTag],!*HSt)
Filter wholepage thisUser thrOwner tree hst
# startuser			= if wholepage defaultUser thrOwner
# (threadcode,accu) = collect thisUser startuser []((startuser,0,defaultWorkflowName,"main") @@: tree)  // KLOPT DIT WEL ??
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
collect thisuser taskuser accu ((nuserid,processnr,workflowLabel,taskname) @@: tree) 	// collect returns the wanted code, and the remaining code
# (myhtml,accu)	= collect thisuser nuserid accu tree									// collect all code of this user belonging to this task
| thisuser == nuserid && not (isEmpty myhtml)
						= ([],[(processnr,workflowLabel,taskname,myhtml):accu])
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

