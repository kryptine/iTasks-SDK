implementation module TaskTreeFilters

import StdEnv
import iDataFormlib
import InternaliTasksCommon, iTasksHtmlSupport

determineTaskList :: !UserId !HtmlTree -> [TaskDescription]
determineTaskList thisuser (taskdescr @@: tree) 	
	# collected				= determineTaskList thisuser tree									
	| taskdescr.taskWorkerId == thisuser	= [taskdescr:collected]
											= collected
determineTaskList thisuser (ntaskuser -@: tree)
	= determineTaskList thisuser tree
determineTaskList thisuser (tree1 +|+ tree2)
	# collection1	= determineTaskList thisuser tree1
	# collection2	= determineTaskList thisuser tree2
	= collection1 ++ collection2
determineTaskList thisuser (tree1 +-+ tree2)
	# collection1	= determineTaskList thisuser tree1
	# collection2	= determineTaskList thisuser tree2
	= collection1 ++ collection2
determineTaskList thisuser  (BT html inputs)
	= []
determineTaskList thisuser (DivCode id tree)
	= determineTaskList thisuser tree
determineTaskList thisuser (TaskTrace traceinfo tree)
	= determineTaskList thisuser tree

determineTaskForTab :: !UserId !TaskNrId !HtmlTree -> (!Bool,![HtmlTag],![InputId])
determineTaskForTab thisuser thistaskid tree
	= case determineTaskTree thisuser thistaskid tree of							//Find the subtree by task id
		Nothing
			= (True, [], [])														//Subtask not found, nothing to do anymore
		Just tree
			# (html,inputs)	= mkFilteredTaskTree thisuser thisuser tree				//Collect only the parts for the current user
			= (False, html, inputs)

determineTaskTree :: !UserId !TaskNrId !HtmlTree -> Maybe HtmlTree
determineTaskTree thisuser thistaskid (taskdescr @@: tree) 	
	| taskdescr.taskNrId == thistaskid	= Just (taskdescr @@: tree)
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
determineTaskTree thisuser thistaskid (TaskTrace traceinfo tree)
	= determineTaskTree thisuser thistaskid tree

mkFilteredTaskTree :: !UserId !UserId !HtmlTree -> (![HtmlTag],![InputId])
mkFilteredTaskTree thisuser taskuser (description @@: tree) 						
	# (html,inputs)		= mkFilteredTaskTree thisuser description.taskWorkerId tree
	| thisuser == description.taskWorkerId
							= (html,inputs)
	| otherwise				= ([],[])
mkFilteredTaskTree thisuser taskuser (nuser -@: tree)
	| thisuser == nuser 	= ([],[])
	| otherwise				= mkFilteredTaskTree thisuser taskuser tree
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
mkUnfilteredTaskTree (_ -@: html) 				= mkUnfilteredTaskTree html
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
// Trace Calculation
// ******************************************************************************************************

:: Trace		=	Trace !(Maybe !TraceInfo) ![Trace]							// traceinfo with possibly subprocess

filterTaskTree :: !HtmlTree -> HtmlTag
filterTaskTree html
# traceInfos	= collectTraceInfo html
# traces		= insertTraces traceInfos [] 
= showTaskTree (Just traces)
where
	collectTraceInfo :: !HtmlTree -> [TraceInfo]
	collectTraceInfo (TaskTrace traceinfo html) = [traceinfo : collectTraceInfo html]
	collectTraceInfo (BT body inputs) 			= []
	collectTraceInfo (_ @@: html) 				= collectTraceInfo html
	collectTraceInfo (_ -@: html) 				= collectTraceInfo html
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


filterTaskTreeOfTask :: !UserId !TaskNrId !HtmlTree -> HtmlTag				
filterTaskTreeOfTask userId taskNrId tree
	# mbtree			= determineTaskTree userId taskNrId tree
	| isNothing mbtree	= Text "Error: Cannot find task tree !"
	= filterTaskTree (fromJust mbtree)

insertTrace :: !TraceInfo ![Trace] -> [Trace]
insertTrace info trace = insertTrace` (reverse info.trTaskNr) trace
where
	insertTrace` :: !TaskNr ![Trace] -> [Trace]
	insertTrace` [i] traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr info.trTaskNr <+++ "," <+++ info.trUserId <+++ "," <+++ info.trTaskName)
	# (Trace _ itraces)		= select i traces
	= updateAt` i (Trace (Just info) itraces)  traces
	insertTrace` [i:is] traces
	| i < 0					= abort ("negative task numbers:" <+++ showTaskNr info.trTaskNr <+++ "," <+++ info.trUserId <+++ "," <+++ info.trTaskName)
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
		
showTaskTree :: !(Maybe [Trace]) -> HtmlTag
showTaskTree Nothing	= Text "No task tree trace "
showTaskTree (Just a)	= DivTag [] [showLabel "Task Tree Forest:", BrTag [] , STable emptyBackground (print False a),HrTag []]
where
	print _ []		= []
	print b trace	= [pr b x ++ [STable emptyBackground (print (isDone x||b) xs)]\\ (Trace x xs) <- trace] 

	pr _ Nothing 			= []
	pr dprev (Just info=:{trTaskName, trActivated})	
	| dprev && (not trActivated)							= pr False Nothing	// subtask not important anymore (assume no milestone tasks)
	| not trActivated	&& trTaskName%(0,4) == "Ajax "		= showTask cellattr1b White Navy Aqua  Silver  info
	| not trActivated	&& trTaskName%(0,6) == "Server "	= showTask cellattr1b White Navy Aqua  Silver  info
	| not trActivated	&& trTaskName%(0,6) == "Client "	= showTask cellattr1b White Navy Aqua  Silver  info
	| not trActivated										= showTask cellattr1b White Navy Maroon Silver info
	= showTask cellattr1a White Yellow Red White info
	
	showTask att c1 c2 c3 c4 info
	= [STable doneBackground 	
		[ [font c1 (toString info.trUserId),font c2 ("T" <+++ showTaskNr info.trTaskNr)]
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
	
