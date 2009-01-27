implementation module TaskTreeFilters

import StdEnv
import iDataFormlib
import InternaliTasksCommon

:: TaskStatus = TaskFinished | TaskActivated | TaskDeleted

instance == TaskStatus
where
	(==) TaskFinished 	TaskFinished 	= True
	(==) TaskActivated 	TaskActivated 	= True
	(==) TaskDeleted 	TaskDeleted 	= True
	(==) _ 				_ 				= False

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

// ******************************************************************************************************
// Trace Calculation
// ******************************************************************************************************

:: Trace		=	Trace !(Maybe TraceInfo) ![Trace]							// traceinfo with possibly subprocess

getTraceFromTaskTree :: !UserId !String !HtmlTree -> HtmlTag				
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
	insertTrace info trace = insertTrace` (reverse (taskNrFromString info.trTaskNr)) trace
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
	
