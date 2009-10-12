implementation module Trace

import StdList, StdTuple
import Html, Text
import ProcessDB
import TaskTree
import JSON

derive JSONEncode TraceTree
derive JSONDecode TraceTree

traceProcesses :: [Process] -> HtmlTag
traceProcesses processes = mkTable processes
where
	mkTable processes	= TableTag [ClassAttr "debug-table"] [mkHeader: [mkRow process \\ process <- processes]]
	mkHeader			= TrTag [] [ThTag [] [Text "Id"],ThTag [] [Text "Subject"],ThTag [] [Text "Owner"],ThTag [] [Text "Delegator"],ThTag [] [Text "Type"], ThTag [] [Text "Status"],ThTag [] [Text "Parent"], ThTag [] [Text "Active changes"] ]
	mkRow process		= TrTag []	[ TdTag [] [Text (toString process.Process.processId)]
							, TdTag [] [Text process.Process.properties.systemProps.subject]
							, TdTag [] [Text (toString (fst process.Process.properties.managerProps.worker) +++ ": " +++ snd process.Process.properties.managerProps.worker)]
							, TdTag [] [Text (toString (fst process.Process.properties.systemProps.manager) +++ ": " +++ snd process.Process.properties.systemProps.manager)]
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

traceTaskTree :: TaskTree -> TraceTree
traceTaskTree tree = mkTree tree
where
	mkTree (TTExtJSTask info _ ) 
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-int"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = info.TaskInfo.taskLabel
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "INT"
		  , activeClass = activeClass info
		  , children = []
		  }
	
	mkTree (TTMonitorTask info _ )
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-mon"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = info.TaskInfo.taskLabel
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "MON"
		  , activeClass = activeClass info
		  , children = []
		  }
	
	mkTree (TTRpcTask info _ )
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-rpc"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = info.TaskInfo.taskLabel
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "RPC"
		  , activeClass = activeClass info
		  , children = []
		  }		  
		  
	mkTree (TTSequenceTask info trees)
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = checkIfLeaf trees
		  , iconCls = "task-seq"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = info.TaskInfo.taskLabel
		  , traceValue = ""
		  , taskClass = "SEQ"
		  , activeClass = activeClass info
		  , children = [traceTaskTree tree \\ tree <- trees]
		  }
	
	mkTree (TTParallelTask info combination trees)
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = checkIfLeaf trees
		  , iconCls = "task-par"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = info.TaskInfo.taskLabel
		  , traceValue = ""
		  , taskClass = "PAR"
		  , activeClass = activeClass info
		  , children = [traceTaskTree tree \\ tree <- trees]
		  }
	
	mkTree (TTMainTask info mti trees)
		# (userId, userName) =  mti.TaskProperties.managerProps.TaskManagerProperties.worker
		= { cls = "master-task"
		  , uiProvider = "col"
		  , user = userName+++" ("+++toString userId+++")"
		  , leaf = checkIfLeaf trees
		  , iconCls = "task-mnt"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = info.TaskInfo.taskLabel
		  , traceValue = ""
		  , taskClass = "MNT"
		  , activeClass = activeClass info
		  , children = [traceTaskTree tree \\ tree <- trees]
		  }
	
	mkTree (TTFinishedTask info)
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-fin"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = info.TaskInfo.taskLabel
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "FIN"
		  , activeClass = activeClass info
		  , children = []
		  }
	
	activeClass info
		| info.TaskInfo.finished	= "finished"
		| info.TaskInfo.active		= "active"
									= "inactive"

	showCombination TTVertical		= "Vertical"
	showCombination TTHorizontal	= "Horizontal"
	
	checkIfLeaf trees
		| length trees > 0 			= False
		| otherwise 				= True
	
traceTaskForest :: [TaskTree] -> String
traceTaskForest trees = toJSON [traceTaskTree tree \\ tree <- trees]
