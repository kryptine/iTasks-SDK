implementation module FIXMEDebug

import Html
import ProcessDB
import TaskTree

traceProcesses :: [Process] -> HtmlTag
traceProcesses processes = DivTag [ClassAttr "trace"] (mkTable processes)
where
	mkTable processes	= [TableTag [] [mkHeader: [mkRow process \\ process <- processes]]]
	mkHeader			= TrTag [] [ThTag [] [Text "Id"],ThTag [] [Text "Owner"],ThTag [] [Text "Type"], ThTag [] [Text "Status"],ThTag [] [Text "Parent"] ]
	mkRow process		= TrTag []	[ TdTag [] [Text (toString process.Process.id)]
							, TdTag [] [Text (toString process.Process.owner)]
							, TdTag [] [Text (case process.Process.process of
												(LEFT _) 	= "Static"
												(RIGHT _)	= "Dynamic"
											  )]
							, TdTag [] [Text (toString process.Process.status)]
							, TdTag [] (case process.Process.process of
											(LEFT _)	= []
											(RIGHT dyn)	= [Text (toString dyn.DynamicProcessEntry.parent)]
										)
							]

traceTaskTree :: TaskTree -> HtmlTag
traceTaskTree tree = DivTag [] (mkTree tree)
where
	mkTree (TTBasicTask info _ _)
		= [DivTag [ClassAttr "trace-node"] [
			DivTag [ClassAttr ("trace-node-title " +++ (activeClass info))] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel],
			DivTag [ClassAttr "trace-node-content " ] [
					TableTag [] [
						TrTag [] [ThTag [] [Text "User id:"] , TdTag [] [Text (toString info.TaskInfo.userId)] ]
					] 		
				]
		    ]
		  ]
	mkTree (TTSequenceTask info trees)
		= [TableTag [ClassAttr "trace-sequence"] [
			TrTag [] [ThTag [ClassAttr (activeClass info)] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel] ]
			:
			[TrTag [] [TdTag [] (mkTree tree)] \\ tree <- trees]
		  ]]
	mkTree (TTParallelTask info _ _ trees)
		= [TableTag [ClassAttr "trace-parallel"] [
			TrTag [] [ThTag [ClassAttr (activeClass info), ColspanAttr (toString (length trees))] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel] ],
			TrTag [] [TdTag [] (mkTree tree) \\ tree <- trees]
		  ]]
	mkTree (TTProcess info trees)		
		= [DivTag [ClassAttr "trace-process"] [H2Tag [] [Text "Process ",Text (toString info.ProcessInfo.processId),Text " (User ", Text (toString info.ProcessInfo.userId),Text ")"]
											  : flatten (map mkTree trees)]]

	activeClass info
		| info.TaskInfo.finished	= "trace-finished"
		| info.TaskInfo.active		= "trace-active"
									= "trace-inactive"

traceTaskForest :: [TaskTree] -> HtmlTag
traceTaskForest trees = DivTag [] [traceTaskTree tree \\ tree <- trees]
