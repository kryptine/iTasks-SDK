implementation module TaskTreeFilters

import StdEnv
import iDataFormlib
import InternaliTasksCommon, iTasksHtmlSupport

:: TaskStatus = TaskFinished | TaskActivated | TaskDeleted

instance == TaskStatus
where
	(==) TaskFinished 	TaskFinished 	= True
	(==) TaskActivated 	TaskActivated 	= True
	(==) TaskDeleted 	TaskDeleted 	= True
	(==) _ 				_ 				= False

determineTaskList :: !UserId !HtmlTree -> [([Bool],Bool,TaskDescription)]
determineTaskList thisuser tree = fst (determineTaskList` thisuser [] tree defaultTaskDescriptor)
where
	determineSubTaskList thisuser path [] taskDescr
		= []
	determineSubTaskList thisuser path [(desc,t)] taskDescr
		# collection = fst (determineTaskList` thisuser (path ++ [False]) t taskDescr)
		= [(path ++ [False],True,taskDescr):collection]
	determineSubTaskList thisuser path [(desc,t):ts] taskDescr
		# collection = fst (determineTaskList` thisuser (path ++ [False]) t taskDescr)
		# collections = determineSubTaskList thisuser path ts taskDescr
		= [(path ++ [False],True,taskDescr):collection] ++ collections
			

	determineTaskList` thisuser path (ntaskDescr @@: tree) taskDescr 	
		# (collected, more)						= determineTaskList` thisuser path tree ntaskDescr								
		| ntaskDescr.taskWorkerId == thisuser	= ([(path, not more,ntaskDescr):collected], True)
												= (collected, more)

	determineTaskList` thisuser path (CondAnd label nr ts) taskDescr
		# collections = determineSubTaskList thisuser path ts taskDescr
		= ([(path,True,taskDescr):collections],True)


	/*	
	determineTaskList` thisuser path (CondAnd label nr []) taskDescr
												= ([], False)
												
	determineTaskList` thisuser path (CondAnd label nr [t=:(condAndDescr,htmlTree):ts]) taskDescr
		# collection							= determineTaskList` thisuser path htmlTree taskDescr
		# collections 							= determineTaskList` thisuser path (CondAnd label nr ts) taskDescr
		= [(path,True,{taskDescr & taskNrId = condAndDescr.caTaskNrId, taskLabel = label, curStatus = condAndDescr.caStatus})] ++ collection ++ collections
	*/
	determineTaskList` thisuser path (tree1 +|+ tree2) taskDescr
		# (collection1, more1)					= determineTaskList` thisuser path tree1 taskDescr
		# (collection2, more2)					= determineTaskList` thisuser path tree2 taskDescr
		= (collection1 ++ collection2, more1 || more2) 
	
	determineTaskList` thisuser path (tree1 +-+ tree2) taskDescr
		# (collection1, more1)					= determineTaskList` thisuser path tree1 taskDescr
		# (collection2, more2)					= determineTaskList` thisuser path tree2 taskDescr
		= (collection1 ++ collection2, more1 || more2)
	
	determineTaskList` thisuser path (BT html inputs) taskDescr
		= ([], False)
	determineTaskList` thisuser path (DivCode id tree) taskDescr
		= determineTaskList` thisuser path tree taskDescr
	determineTaskList` thisuser path (TaskTrace traceinfo tree) taskDescr
		= determineTaskList` thisuser path tree taskDescr

defaultTaskDescriptor
	=	{ delegatorId	= 0								
		, taskWorkerId	= 0								
		, taskNrId		= ""								
		, processNr		= 0								
		, workflowLabel	= "Non-existing"							
		, taskLabel		= "Non-existing"								
		, timeCreated	= Time 0
		, taskPriority	= LowPriority
		, curStatus		= True
		}

determineTaskForTab :: !UserId !TaskNrId !HtmlTree -> (!TaskStatus,![HtmlTag],![InputId])
determineTaskForTab thisuser thistaskid tree
	= case determineMyTaskTree thisuser thistaskid tree of							//Find the subtree by task id
		Nothing
			= (TaskDeleted, [], [])													//Subtask not found, nothing to do anymore
		Just tree
			# (html,inputs)	= mkFilteredTaskTree thisuser thisuser tree				//Collect only the parts for the current user
			= (test tree, html, inputs)
	where
		test (description @@: html) 
		| description.taskNrId == thistaskid && description.curStatus = TaskFinished
		= TaskActivated

mkFilteredTaskTree :: !UserId !UserId !HtmlTree -> (![HtmlTag],![InputId])
mkFilteredTaskTree thisuser taskuser (description @@: tree) 						
	# (html,inputs)		= mkFilteredTaskTree thisuser description.taskWorkerId tree
	| thisuser == description.taskWorkerId
							= (html,inputs)
	| otherwise				= ([],[])
mkFilteredTaskTree thisuser taskuser (CondAnd label nr [])
	= ([],[])
mkFilteredTaskTree thisuser taskuser (CondAnd label nr [(index,tree):trees])
	# (tag,input)			= mkFilteredTaskTree thisuser taskuser tree
	# (tags,inputs)			= mkFilteredTaskTree thisuser taskuser (CondAnd label nr trees)
	= (tag ++ tags,input ++ inputs)
mkFilteredTaskTree thisuser taskuser (tree1 +|+ tree2)
	# (lhtml,linputs)	= mkFilteredTaskTree thisuser taskuser tree1
	# (rhtml,rinputs)	= mkFilteredTaskTree thisuser taskuser tree2
	= (lhtml <||> rhtml,linputs ++ rinputs)
mkFilteredTaskTree thisuser taskuser (tree1 +-+ tree2)
	# (lhtml,linputs)	= mkFilteredTaskTree thisuser taskuser tree1
	# (rhtml,rinputs)	= mkFilteredTaskTree thisuser taskuser tree2
	= (lhtml <=> rhtml,linputs ++ rinputs)
mkFilteredTaskTree thisuser taskuser (BT bdtg inputs)
	| thisuser == taskuser	= (bdtg,inputs)
	| otherwise				= ([],[])
mkFilteredTaskTree thisuser taskuser (DivCode id tree)
	# (html,inputs)			= mkFilteredTaskTree thisuser taskuser tree
	| thisuser == taskuser 	= ([DivTag [IdAttr id, ClassAttr "itasks-thread"] html],inputs)
	| otherwise				= ([],[])
mkFilteredTaskTree thisuser taskuser (TaskTrace traceinfo tree)
	# (html,inputs)			= mkFilteredTaskTree thisuser taskuser tree
	| thisuser == taskuser 	= (html,inputs)
	| otherwise				= ([],[])

mkUnfilteredTaskTree :: !HtmlTree -> (![HtmlTag],![InputId])
mkUnfilteredTaskTree (BT body inputs) 			= (body, inputs)
mkUnfilteredTaskTree (_ @@: html) 				= mkUnfilteredTaskTree html
mkUnfilteredTaskTree (CondAnd label nr []) 		= ([],[])
mkUnfilteredTaskTree (CondAnd label nr [(tn,tree):trees]) 		
												= (htmlL ++ htmlR,inpL ++ inpR)
where
	(htmlL,inpL) = mkUnfilteredTaskTree tree
	(htmlR,inpR) = mkUnfilteredTaskTree (CondAnd label nr trees)
mkUnfilteredTaskTree (DivCode str html) 		= mkUnfilteredTaskTree html
mkUnfilteredTaskTree (TaskTrace traceinfo html) = mkUnfilteredTaskTree html
mkUnfilteredTaskTree (nodeL +-+ nodeR) 			= (htmlL <=> htmlR,inpL ++ inpR)
where
	(htmlL,inpL) = mkUnfilteredTaskTree nodeL
	(htmlR,inpR) = mkUnfilteredTaskTree nodeR
mkUnfilteredTaskTree (nodeL +|+ nodeR) 			= (htmlL <||> htmlR, inpL ++ inpR)
where
	(htmlL,inpL) = mkUnfilteredTaskTree nodeL
	(htmlR,inpR) = mkUnfilteredTaskTree nodeR

// ******************************************************************************************************
// Search for that part of the task tree which is applicable for a given user and a given task
// ******************************************************************************************************

determineMyTaskTree :: !UserId !TaskNrId !HtmlTree -> Maybe HtmlTree
determineMyTaskTree thisuser thistaskid tree = determineMyTaskTree` thisuser thistaskid tree defaultTaskDescriptor
where
	determineMyTaskTree` thisuser thistaskid  (BT bdtg inputs) taskDescr
		= Nothing
	determineMyTaskTree` thisuser thistaskid (CondAnd label nr []) taskDescr
		= Nothing
	determineMyTaskTree` thisuser thistaskid (CondAnd label nr [(condAndDescr,tree):trees]) taskDescr
		| thistaskid == condAndDescr.caTaskNrId				
											= Just ({taskDescr 	& taskNrId 	= thistaskid
																, taskLabel = label <+++ condAndDescr.caIndex
																, curStatus = condAndDescr.caStatus} @@: (pruneTree tree))
		# mbTree							= determineMyTaskTree` thisuser thistaskid tree taskDescr
		| isNothing mbTree					= determineMyTaskTree` thisuser thistaskid (CondAnd label nr trees) taskDescr
		= mbTree
	determineMyTaskTree` thisuser thistaskid  (tree1 +-+ tree2) taskDescr
		# ntree1							= determineMyTaskTree` thisuser thistaskid tree1 taskDescr
		| isJust ntree1						= ntree1
											= determineMyTaskTree` thisuser thistaskid tree2 taskDescr
	determineMyTaskTree` thisuser thistaskid (tree1 +|+ tree2) taskDescr
		# ntree1							= determineMyTaskTree` thisuser thistaskid tree1 taskDescr
		| isJust ntree1						= ntree1
											= determineMyTaskTree` thisuser thistaskid tree2 taskDescr
	determineMyTaskTree` thisuser thistaskid (DivCode id tree) taskDescr
											= determineMyTaskTree` thisuser thistaskid tree taskDescr
	determineMyTaskTree` thisuser thistaskid (TaskTrace traceinfo tree) taskDescr
											= determineMyTaskTree` thisuser thistaskid tree taskDescr
	determineMyTaskTree` thisuser thistaskid (taskdescr @@: tree) taskDescr	
		| taskdescr.taskNrId 	 == thistaskid	&&
		  taskdescr.taskWorkerId == thisuser= Just (taskdescr @@: (pruneTree tree))
											= determineMyTaskTree` thisuser thistaskid tree taskdescr

	pruneTree :: !HtmlTree -> HtmlTree		// delete all sub trees not belonging to this task
	pruneTree (taskdescr @@: tree)			= BT [] []								// this task will appear in another tab						
	pruneTree (CondAnd label nr trees)		= BT [] [] 								// this task will appear in another tab as well
	pruneTree (tree1 +|+ tree2)				= pruneTree tree1 +|+ pruneTree tree2
	pruneTree (tree1 +-+ tree2)				= pruneTree tree1 +-+ pruneTree tree2
	pruneTree (BT bdtg inputs)				= BT bdtg inputs
	pruneTree (DivCode id tree)				= DivCode id (pruneTree tree)
	pruneTree (TaskTrace traceinfo tree)	= TaskTrace traceinfo (pruneTree tree)


// ******************************************************************************************************
// Trace Calculation
// ******************************************************************************************************

:: Trace		=	Trace !(Maybe !TraceInfo) ![Trace]							// traceinfo with possibly subprocess

getTraceFromTaskTree :: !UserId !TaskNrId !HtmlTree -> HtmlTag				
getTraceFromTaskTree userId taskNrId tree
	# mbtree			= determineMyTaskTree userId taskNrId tree
	| isNothing mbtree	= Text "Error: Cannot find task tree !"
	= getFullTraceFromTaskTree (fromJust mbtree)

getFullTraceFromTaskTree :: !HtmlTree -> HtmlTag
getFullTraceFromTaskTree html
# traceInfos	= collectTraceInfo html
# traces		= insertTraces traceInfos [] 
= showTaskTreeTrace (Just traces)
where
	collectTraceInfo :: !HtmlTree -> [TraceInfo]
	collectTraceInfo (TaskTrace traceinfo html) = [traceinfo : collectTraceInfo html]
	collectTraceInfo (BT body inputs) 			= []
	collectTraceInfo (_ @@: html) 				= collectTraceInfo html
	collectTraceInfo (CondAnd label nr html) 	= flatten (map collectTraceInfo (map snd html))
	collectTraceInfo (DivCode str html) 		= collectTraceInfo html
	collectTraceInfo (nodeL +-+ nodeR) 			= traceLeft ++ traceRight
	where
		traceLeft 	= collectTraceInfo nodeL
		traceRight	= collectTraceInfo nodeR
	collectTraceInfo (nodeL +|+ nodeR) 			= traceLeft ++ traceRight
	where
		traceLeft 	= collectTraceInfo nodeL
		traceRight 	= collectTraceInfo nodeR

	insertTraces [] traces     = traces
	insertTraces [i:is] traces = insertTraces is (insertTrace i traces)

	insertTrace :: !TraceInfo ![Trace] -> [Trace]
	insertTrace info trace = insertTrace` (reverse (parseTaskNr info.trTaskNr)) trace
	where
		insertTrace` :: !TaskNr ![Trace] -> [Trace]
		insertTrace` [i] traces
		| i < 0					= abort ("negative task numbers:" <+++ info.trTaskNr <+++ "," <+++ info.trUserId <+++ "," <+++ info.trTaskName)
		# (Trace _ itraces)		= select i traces
		= updateAt` i (Trace (Just info) itraces)  traces
		insertTrace` [i:is] traces
		| i < 0					= abort ("negative task numbers:" <+++ info.trTaskNr <+++ "," <+++ info.trUserId <+++ "," <+++ info.trTaskName)
		# (Trace ni itraces)	= select i traces
		# nistraces				= insertTrace` is itraces
		= updateAt` i (Trace ni nistraces) traces
	
		select :: !Int ![Trace] -> Trace
		select i list
		| i < length list = list!!i 
		=  Trace Nothing []
	
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
// Displaying Task Tree Trace information
// ******************************************************************************************************
		
showTaskTreeTrace :: !(Maybe [Trace]) -> HtmlTag
showTaskTreeTrace Nothing	= Text "No task tree trace "
showTaskTreeTrace (Just a)	= DivTag [] [showLabel "Task Tree Forest:", BrTag [] , STable emptyBackground (print False a),HrTag []]
where
	print _ []		= []
	print b trace	= [pr b x ++ [STable emptyBackground (print (isDone x||b) xs)]\\ (Trace x xs) <- trace] 

	pr _ Nothing 			= []
	pr dprev (Just info=:{trTaskName, trActivated})	
	| dprev && (not trActivated)							= pr False Nothing	// subtask not important anymore (assume no milestone tasks)
	| not trActivated	&& trTaskName%(0,4) == "Ajax "		= showTask cellattr1b Black Navy Aqua  Silver  info
	| not trActivated	&& trTaskName%(0,6) == "Server "	= showTask cellattr1b Black Navy Aqua  Silver  info
	| not trActivated	&& trTaskName%(0,6) == "Client "	= showTask cellattr1b Black Navy Aqua  Silver  info
	| not trActivated										= showTask cellattr1b Black Navy Maroon Silver info
	= showTask cellattr1a Black Yellow Red Black info
	
	showTask att c1 c2 c3 c4 info
	= [STable doneBackground 	
		[ [font c1 (toString info.trUserId),font c2 ("T" <+++ info.trTaskNr)]
		, [showStorage info.trOptions.tasklife, font c3 info.trTaskName]
		, [EmptyBody, font c4 info.trValue]
		]
		]
	isDone Nothing = False
	isDone (Just info) = info.trActivated

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
	
