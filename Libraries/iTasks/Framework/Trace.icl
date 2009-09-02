implementation module Trace

import StdList, StdTuple
import Html, Text
import ProcessDB
import TaskTree

traceProcesses :: [Process] -> HtmlTag
traceProcesses processes = mkTable processes
where
	mkTable processes	= TableTag [ClassAttr "debug-table"] [mkHeader: [mkRow process \\ process <- processes]]
	mkHeader			= TrTag [] [ThTag [] [Text "Id"],ThTag [] [Text "Subject"],ThTag [] [Text "Owner"],ThTag [] [Text "Delegator"],ThTag [] [Text "Type"], ThTag [] [Text "Status"],ThTag [] [Text "Parent"], ThTag [] [Text "Active changes"] ]
	mkRow process		= TrTag []	[ TdTag [] [Text (toString process.Process.processId)]
							, TdTag [] [Text process.Process.properties.TaskProperties.subject]
							, TdTag [] [Text (toString (fst process.Process.properties.TaskProperties.user) +++ ": " +++ snd process.Process.properties.TaskProperties.user)]
							, TdTag [] [Text (toString (fst process.Process.properties.TaskProperties.delegator) +++ ": " +++ snd process.Process.properties.TaskProperties.delegator)]
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
							, TdTag [] [Text (join ", " (map fst process.Process.changes))]
							]

traceTaskTree :: TaskTree -> HtmlTag
traceTaskTree tree = DivTag [] (mkTree tree)
where	
	mkTree (TTExtJSTask info _ )
		= [DivTag [ClassAttr "trace-node"] [
			DivTag [ClassAttr ("trace-node-title " +++ (activeClass info))] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel],
			DivTag [ClassAttr "trace-node-content " ] [Text info.TaskInfo.traceValue]
		    ]
		  ]
	mkTree (TTMonitorTask info _ )
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
	mkTree (TTFinishedTask info)
		= [DivTag [ClassAttr "trace-node"] [
			DivTag [ClassAttr ("trace-node-title " +++ (activeClass info))] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel],
			DivTag [ClassAttr "trace-node-content " ] [Text info.TaskInfo.traceValue]
		    ]
		  ]

	activeClass info
		| info.TaskInfo.finished	= "trace-finished"
		| info.TaskInfo.active		= "trace-active"
									= "trace-inactive"

	showCombination TTVertical		= "Vertical"
	showCombination TTHorizontal	= "Horizontal"
	
traceTaskForest :: [TaskTree] -> HtmlTag
traceTaskForest trees = DivTag [] [traceTaskTree tree \\ tree <- trees]
