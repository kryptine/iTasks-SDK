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

determineTaskForTab :: !UserId !TaskNrId !HtmlTree -> (!Bool,![HtmlTag],![InputId])
determineTaskForTab thisuser thistaskid tree
	# mytree = determineTaskTree thisuser thistaskid tree
	| isNothing mytree = (True,[],[])
	# (threadcode,seltask,inputs)	= mkFilteredTaskTree True thisuser thisuser (fromJust mytree)
	| isEmpty threadcode	= (False, seltask, inputs)
							= (False, threadcode, inputs)

determineTaskTree :: !UserId !TaskNrId !HtmlTree -> Maybe HtmlTree
determineTaskTree thisuser thistaskid (taskdescr @@: tree) 	
	| taskdescr.taskNrId == thistaskid	= Just tree
										= determineTaskTree thisuser thistaskid tree									
determineTaskTree thisuser thistaskid (ntaskuser -@: tree)
	| thisuser == ntaskuser				= Nothing
										= determineTaskTree thisuser thistaskid tree
determineTaskTree thisuser thistaskid (tree1 +|+ tree2)
	# ntree1		= determineTaskTree thisuser thistaskid tree1
	| isJust ntree1						= ntree1
										= determineTaskTree thisuser thistaskid tree2
determineTaskTree  thisuser thistaskid  (tree1 +-+ tree2)
	# ntree1		= determineTaskTree thisuser thistaskid tree1
	| isJust ntree1						= ntree1
										= determineTaskTree thisuser thistaskid tree2
determineTaskTree thisuser thistaskid  (BT bdtg inputs)
	= Nothing
determineTaskTree thisuser thistaskid (DivCode id tree)
	= determineTaskTree thisuser thistaskid tree


mkFilteredTaskTree :: !Bool !UserId !UserId !HtmlTree -> (![HtmlTag],![HtmlTag],![InputId])
mkFilteredTaskTree wholepage thisUser thrOwner tree
# startuser			= if wholepage defaultUser thrOwner
# (threadcode,threadinputs,accu) = collect thisUser startuser [] (initialTaskDescription @@: tree)  // KLOPT DIT WEL ??
| isEmpty accu		= (threadcode,[],threadinputs)
# accu				= sortBy (\(i,_,_,_,_) (j,_,_,_,_) -> i < j) accu
# (workflownames,subtasks) 						= unziptasks accu
# (subtasksnames,tcode)							= unzipsubtasks (hd subtasks)
# (selcode, selinputs)							= hd tcode
= (threadcode, selcode, selinputs)
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
		=	{ tasklife 		= if (thisUser >= 0) location LSSession 
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

mkUnfilteredTaskTree :: !HtmlTree -> ([HtmlTag],[InputId])
mkUnfilteredTaskTree (BT body inputs) 	= (body, inputs)
mkUnfilteredTaskTree (_ @@: html) 		= mkUnfilteredTaskTree html
mkUnfilteredTaskTree (_ -@: html) 		= mkUnfilteredTaskTree html
mkUnfilteredTaskTree (DivCode str html) = mkUnfilteredTaskTree html
mkUnfilteredTaskTree (nodeL +-+ nodeR) 	= ([htmlL <=> htmlR],inpL ++ inpR)
where
	(htmlL,inpL) = mkUnfilteredTaskTree nodeL
	(htmlR,inpR) = mkUnfilteredTaskTree nodeR
mkUnfilteredTaskTree (nodeL +|+ nodeR) 	= (htmlL <|.|> htmlR, inpL ++ inpR)
where
	(htmlL,inpL) = mkUnfilteredTaskTree nodeL
	(htmlR,inpR) = mkUnfilteredTaskTree nodeR


// ******************************************************************************************************
// Trace Printing ...
// ******************************************************************************************************

showTaskTreeOfTask	:: !TaskNrId !(Maybe [Trace]) -> HtmlTag					// This can be done much more efficiently, taken the ordening of tasknrs into account
showTaskTreeOfTask tasknr Nothing 		= Text ("Tracing enabled, cannot determine task tree of task " +++ tasknr)
showTaskTreeOfTask tasknr (Just []) 	= Text ("Cannot find task tree of task " +++ tasknr)
showTaskTreeOfTask tasknr (Just trace) 	= showTaskTree (snd (findTaskInTrace tasknr trace))

findTaskInTrace :: !TaskNrId ![Trace] -> (!Bool,!Maybe [Trace]) 
findTaskInTrace tasknr []
= (False, Just [])
findTaskInTrace tasknr mytrace=:[Trace Nothing traces:mtraces]
# (found,tags) = findTaskInTrace tasknr traces
| found = (found,tags)
= findTaskInTrace tasknr mtraces
findTaskInTrace tasknr mytrace=:[Trace (Just (dtask,(w,i,op,tn,s))) traces:mtraces]
| showTaskNr (repair i) == tasknr = (True,  Just mytrace)
# (found,tags) = findTaskInTrace tasknr traces
| found = (found,tags)
= findTaskInTrace tasknr mtraces
where
	repair [0:tnrs] = [-1:tnrs]		// The taks numbers obtained from client are one to low: this has to be made global consistent, very ughly
	repair other = other

showTaskTree :: !(Maybe [Trace]) -> HtmlTag
showTaskTree Nothing	= Text "No task tree trace " // SpanTag [] []
showTaskTree (Just a)	= DivTag [] [showLabel "Task Tree Forest:", BrTag [] , STable emptyBackground (print False a),HrTag []]
where
	print _ []		= []
	print b trace	= [pr b x ++ [STable emptyBackground (print (isDone x||b) xs)]\\ (Trace x xs) <- trace] 

	pr _ Nothing 			= []
	pr dprev (Just (dtask,(w,i,op,tn,s)))	
	| dprev && (not dtask)					= pr False Nothing	// subtask not important anymore (assume no milestone tasks)
	| not dtask	&& tn%(0,4) == "Ajax "		= showTask cellattr1b White Navy Aqua  Silver  (w,i,op,tn,s)
	| not dtask	&& tn%(0,6) == "Server "	= showTask cellattr1b White Navy Aqua  Silver  (w,i,op,tn,s)
	| not dtask	&& tn%(0,6) == "Client "	= showTask cellattr1b White Navy Aqua  Silver  (w,i,op,tn,s)
	| not dtask								= showTask cellattr1b White Navy Maroon Silver (w,i,op,tn,s)
	= showTask cellattr1a White Yellow Red White (w,i,op,tn,s)
	
	showTask2 attr1 c1 c2 c3 c4 (w,i,op,tn,s)
	= [TableTag doneBackground 	[ TrTag [] [TdTag attr1 [font c1 (toString (last (reverse i)))],	TdTag cellattr2 [font c2 tn]]
								, TrTag [] [TdTag attr1 [font c3 (toString w)], 					TdTag cellattr2 [font c4 s]]
								]
	  ,BrTag []]

	showTask att c1 c2 c3 c4 (w,i,op,tn,s)
	= [STable doneBackground 	
		[ [font c1 (toString w),font c2 ("T" <+++ showTaskNr i)]
		, [showStorage op.tasklife, font c3 tn]
		, [EmptyBody, font c4 s]
		]
		]
	isDone Nothing = False
	isDone (Just (b,(w,i,op,tn,s))) = b

	showStorage LSTemp		= font "silver" "Tmp"
	showStorage LSClient	= font "aqua" "Cli"
	showStorage LSPage		= font "navy" "Pag"
	showStorage LSSession	= font "navy" "Ssn"
	showStorage LSTxtFileRO	= font "red"   "TxF0"
	showStorage LSTxtFile	= font "red"   "TxF"
	showStorage LSDataFile	= font "red"   "DaF"
	showStorage LSDatabase	= font "red"   "DaB"

	doneBackground = 	[ CellpaddingAttr "pixels 1", CellspacingAttr "pixels 0", cellwidth
						, RulesAttr "none", FrameAttr "border" 
						]
	doneBackground2 = 	[ CellspacingAttr "pixels 0", CellspacingAttr "pixels 0", cellwidth
						]
	emptyBackground = 	[ CellpaddingAttr "pixels 0", CellspacingAttr "pixels 0"]
	cellattr1a		=	[ BgcolorAttr Green, WidthAttr "pixels 10", ValignAttr "absmiddle"]
	cellattr1b		=	[ BgcolorAttr Silver, WidthAttr "pixels 10", ValignAttr "absmiddle"]
	cellattr2		=	[ ValignAttr "top"]
	cellwidth		= 	WidthAttr "130"

	font color message
	= SpanTag [StyleAttr ("font-size: smaller; font-weight: bold; color: " +++ color)] [Text message]

	STable atts table		= TableTag atts (mktable table)
	where
		mktable table 	= [TrTag [] (mkrow rows)           \\ rows <- table]
		mkrow   rows 	= [TdTag [ValignAttr "top"]  [row] \\ row  <- rows ]

	EmptyBody = Text ""

	Black	= "#000000"
	Silver	= "#C0C0C0"
	Gray 	= "#808080"
	White	= "#FFFFFF"
	Maroon	= "#800000"
	Red		= "#FF0000"
	Purple	= "#800080"
	Fuchsia	= "#FF00FF"
	Green	= "#008000" 
	Lime	= "#00FF00"
	Olive	= "#808000" 
	Yellow	= "#FFFF00"
	Navy 	= "#000080" 
	Blue	= "#0000FF"
	Teal	= "#008080" 
	Aqua	= "#00FFFF"