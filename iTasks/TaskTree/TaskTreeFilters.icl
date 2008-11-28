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
	= (lhtml <|.|> rhtml,linputs ++ rinputs)
mkFilteredTaskTree thisuser taskuser (tree1 +-+ tree2)
	# (lhtml,linputs)	= mkFilteredTaskTree thisuser taskuser tree1
	# (rhtml,rinputs)	= mkFilteredTaskTree thisuser taskuser tree2
	= ([lhtml <=> rhtml],linputs ++ rinputs)
mkFilteredTaskTree thisuser taskuser (BT bdtg inputs)
	| thisuser == taskuser	= (bdtg,inputs)
	| otherwise				= ([],[])
mkFilteredTaskTree thisuser taskuser (DivCode id tree)
	# (html,inputs)			= mkFilteredTaskTree thisuser taskuser tree
	| thisuser == taskuser 	= ([DivTag [IdAttr id, ClassAttr "itasks-thread"] html],inputs)
	| otherwise				= ([],[])

mkUnfilteredTaskTree :: !HtmlTree -> (![HtmlTag],![InputId])
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
	repair [0:tnrs] = [-1:tnrs]		// The task numbers obtained from client are one to low: this has to be made global consistent, very ughly
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