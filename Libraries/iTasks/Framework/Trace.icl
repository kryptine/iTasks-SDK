implementation module Trace

import StdList
import Html
import ProcessDB
import TaskTree

traceProcesses :: [Process] -> HtmlTag
traceProcesses processes = mkTable processes
where
	mkTable processes	= TableTag [ClassAttr "debug-table"] [mkHeader: [mkRow process \\ process <- processes]]
	mkHeader			= TrTag [] [ThTag [] [Text "Id"],ThTag [] [Text "Subject"],ThTag [] [Text "Owner"],ThTag [] [Text "Delegator"],ThTag [] [Text "Type"], ThTag [] [Text "Status"],ThTag [] [Text "Parent"] ]
	mkRow process		= TrTag []	[ TdTag [] [Text (toString process.Process.processId)]
							, TdTag [] [Text process.Process.properties.TaskProperties.subject]
							, TdTag [] [Text (toString process.Process.properties.TaskProperties.userId)]
							, TdTag [] [Text (toString process.Process.properties.TaskProperties.delegatorId)]
							, TdTag [] [Text (case process.Process.processType of
												(StaticProcess _) 		= "Static"
												(DynamicProcess _)		= "Dynamic"
												(EmbeddedProcess _ _)	= "Embedded"
											  )]
							, TdTag [] [Text (toString process.Process.status)]
							, TdTag [] (case process.Process.parent of
											0	= [Text "N/A"]
											x	= [Text (toString x)]
										)
							]

traceTaskTree :: TaskTree -> HtmlTag
traceTaskTree tree = DivTag [] (mkTree tree)
where
	mkTree (TTBasicTask info _ _)
		= [DivTag [ClassAttr "trace-node"] [
			DivTag [ClassAttr ("trace-node-title " +++ (activeClass info))] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel],
			DivTag [ClassAttr "trace-node-content " ] [Text info.TaskInfo.traceValue]
		    ]
		  ]
	mkTree (TTSequenceTask info trees)
		= [TableTag [ClassAttr "trace-sequence"] [
			TrTag [] [ThTag [ClassAttr (activeClass info)] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel] ]
			:
			[TrTag [] [TdTag [] (mkTree tree)] \\ tree <- trees]
		  ]]
	mkTree (TTParallelTask info combination trees)
		= [TableTag [ClassAttr "trace-parallel"] [
			TrTag [] [ThTag [ClassAttr (activeClass info), ColspanAttr (toString (length trees))] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel, Text " (",Text (showCombination combination), Text ")"] ],
			TrTag [] [TdTag [] (mkTree tree) \\ tree <- trees]
		  ]]
	mkTree (TTMainTask info mti trees)
		= [TableTag [ClassAttr "trace-sequence"] [
			TrTag [] [ThTag [ClassAttr (activeClass info)] [Text "MAINTASK:", Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel] ]
			:
			[TrTag [] [TdTag [] (mkTree tree)] \\ tree <- trees]
		  ]]


	activeClass info
		| info.TaskInfo.finished	= "trace-finished"
		| info.TaskInfo.active		= "trace-active"
									= "trace-inactive"

	showCombination TTVertical		= "Vertical"
	showCombination TTHorizontal	= "Horizontal"
	showCombination (TTCustom _)	= "Custom"
	showCombination (TTSplit _)		= "Split"
	showCombination	_ 				= ""
	
traceTaskForest :: [TaskTree] -> HtmlTag
traceTaskForest trees = DivTag [] [traceTaskTree tree \\ tree <- trees]
