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

traceTaskTree :: HtmlTree -> HtmlTag
traceTaskTree tree = DivTag [ClassAttr "trace"] (mkTree tree)
where
	//Visualize a task tree
	mkTree (BT _ _)
		= []
	mkTree (_ @@: tree)
		= mkTree tree
	mkTree (tree1 +-+ tree2)
		= [TableTag [ClassAttr "trace-split"] [TrTag [] [TdTag [] (mkTree tree1),TdTag [] (mkTree tree2)]]]
	mkTree (tree1 +|+ tree2)
		= [TableTag [ClassAttr "trace-split"] [TrTag [] [TdTag [] (mkTree tree1),TdTag [] (mkTree tree2)]]]
	mkTree (CondAnd _ _ trees)
		= flatten [mkTree tree \\ (_,tree) <- trees]
	mkTree (DivCode _ tree)
		= mkTree tree
	mkTree (TaskTrace info tree)
		= (visualizeTraceNode info) ++ (mkTree tree)

	//Visualize a trace node in the tree
	visualizeTraceNode info
		= [DivTag [ClassAttr ("trace-node " +++ (if info.trActivated "trace-node-inactive" "trace-node-active"))] [
				DivTag [ClassAttr "trace-node-title"] [Text info.trTaskNr, Text ": ", Text info.trTaskName],
				DivTag [ClassAttr "trace-node-content"] [
					TableTag [] [
						TrTag [] [ThTag [] [Text "User id:"] , TdTag [] [Text (toString info.trUserId)] ],
						TrTag [] [ThTag [] [Text "Value:"] , TdTag [] [Text info.trValue] ],
						TrTag [] [ThTag [] [Text "Storage:"], TdTag [] [Text (showStorage info.trOptions.tasklife)] ]
					] 		
				]
			]
		  ]
		  
	showStorage LSTemp		= "Tmp"
	showStorage LSClient	= "Cli"
	showStorage LSPage		= "Pag"
	showStorage LSSession	= "Ssn"
	showStorage LSTxtFileRO	= "TxF0"
	showStorage LSTxtFile	= "TxF"
	showStorage LSDataFile	= "DaF"
	showStorage LSDatabase	= "DaB"

traceTaskTree2 :: TaskTree -> HtmlTag
traceTaskTree2 tree = DivTag [] (mkTree tree)
where
	mkTree (TTBasicTask info _ _)
		= [DivTag [ClassAttr "trace-node"] [
			DivTag [ClassAttr ("trace-node-title " +++ (activeClass info))] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel],
			DivTag [ClassAttr "trace-node-content " ] [
					TableTag [] [
						TrTag [] [ThTag [] [Text "User id:"] , TdTag [] [Text (toString info.TaskInfo.userId)] ]//,
						//TrTag [] [ThTag [] [Text "Value:"] , TdTag [] [Text info.trValue] ],
						//TrTag [] [ThTag [] [Text "Storage:"], TdTag [] [Text (showStorage info.trOptions.tasklife)] ]
					] 		
				]
		    ]
		  ]
	mkTree (TTSequenceTask info trees)
		= [TableTag [ClassAttr "trace-sequence"] [
			TrTag [] [ThTag [ClassAttr (activeClass info)] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel] ]
			:
			[TrTag [] [TdTag [] (mkTree tree)] \\ tree <- (reverse trees)]
		  ]]
	mkTree (TTParallelTask info _ _ trees)
		= [TableTag [ClassAttr "trace-parallel"] [
			TrTag [] [ThTag [ClassAttr (activeClass info), ColspanAttr (toString (length trees))] [Text info.TaskInfo.taskId, Text ": ", Text info.TaskInfo.taskLabel] ],
			TrTag [] [TdTag [] (mkTree tree) \\ tree <- (reverse trees)]
		  ]]
	mkTree (TTProcess info trees)		
		= [DivTag [ClassAttr "trace-process"] [H2Tag [] [Text "Process ",Text (toString info.ProcessInfo.processId)]: flatten (map mkTree (reverse trees))]]

	activeClass info
		| info.TaskInfo.finished	= "trace-finished"
		| info.TaskInfo.active		= "trace-active"
									= "trace-inactive"

traceTaskForest :: [HtmlTree] -> HtmlTag
traceTaskForest trees = DivTag [] [traceTaskTree tree \\ tree <- trees]

traceTaskForest2 :: [TaskTree] -> HtmlTag
traceTaskForest2 trees = DivTag [] [traceTaskTree2 tree \\ tree <- trees]