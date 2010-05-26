implementation module Trace

import StdList, StdTuple
import Html, Text
import ProcessDB
import TaskTree
import JSON

derive JSONEncode TraceTree
derive JSONDecode TraceTree

derive bimap	Maybe, (,)

traceProcesses :: [Process] -> HtmlTag
traceProcesses processes = mkTable processes
where
	mkTable processes	= TableTag [ClassAttr "debug-table"] [mkHeader: [mkRow process \\ process <- processes]]
	mkHeader			= TrTag [] [ThTag [] [Text "Id"],ThTag [] [Text "Subject"],ThTag [] [Text "Owner"],ThTag [] [Text "Subtask access"], ThTag [] [Text "Delegator"], ThTag [] [Text "Status"],ThTag [] [Text "Parent"],ThTag [] [Text "Mutable"],ThTag [] [Text "In Open Parallel?" ], ThTag [] [Text "Delete when done"] ]
	mkRow process		= TrTag []	[ TdTag [] [Text process.Process.processId]
							, TdTag [] [Text process.Process.properties.managerProps.subject]
							, TdTag [] [Text (toString process.Process.properties.managerProps.TaskManagerProperties.worker)]
							, TdTag [] [Text (foldr (+++) "" ["("+++toString p +++": "+++toString u+++") " \\ (p,u) <- process.Process.properties.systemProps.subTaskWorkers])]
							, TdTag [] [Text (toString process.Process.properties.systemProps.manager)]
							, TdTag [] [Text (toString process.Process.status)]
							, TdTag [] (case process.Process.parent of
											""	= [Text "N/A"]
											x	= [Text (toString x)]
										)
							, TdTag [] [Text (printToString process.mutable)]
							, TdTag [] [Text (printToString process.inParallelType)]
							, TdTag [] [Text (printToString process.Process.properties.systemProps.deleteWhenDone)]
							]

traceTaskTree :: TaskTree -> TraceTree
traceTaskTree tree = mkTree tree
where
	mkTree (TTInteractiveTask info _) 
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-int"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "INT"
		  , children = []
		  }
	
	mkTree (TTMonitorTask info _ )
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-mon"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "MON"
		  , children = []
		  }
  
	mkTree (TTInstructionTask info _ _)
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-ins"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "INS"
		  , children = []
		  }
	
	mkTree (TTRpcTask info _ )
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-rpc"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "RPC"
		  , children = []
		  }
		  
	mkTree (TTExtProcessTask info _ )
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-prc"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "RPC"
		  , children = []
		  }	  
		  
	mkTree (TTSequenceTask info trees)
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = checkIfLeaf trees
		  , iconCls = "task-seq"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "SEQ"
		  , children = [traceTaskTree tree \\ tree <- trees]
		  }
	
	mkTree (TTParallelTask info tpi trees)
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = checkIfLeaf trees
		  , iconCls = "task-par"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = tpi.TaskParallelInfo.description
		  , taskClass = "PAR"
		  , children = [traceTaskTree tree \\ tree <- trees]
		  }
		  
	mkTree (TTGroupedTask info trees _)
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = checkIfLeaf trees
		  , iconCls = "task-grp"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "GRP"
		  , children = [traceTaskTree tree \\ tree <- trees]
		  }
	
	mkTree (TTMainTask info mti menus inptype tree)
		= { cls = "master-task"
		  , uiProvider = "col"
		  , user = toString mti.TaskProperties.managerProps.TaskManagerProperties.worker
		  , leaf = False
		  , iconCls = "task-mnt"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = printToString inptype
		  , taskClass = "MNT"
		  , children = [traceTaskTree tree]
		  }
	
	mkTree (TTFinishedTask info _)
		= { cls = "master-task"
		  , user = toString info.TaskInfo.worker
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-fin"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "FIN"
		  , children = []
		  }
	
	checkIfLeaf trees
		| length trees > 0 			= False
		| otherwise 				= True
	
traceTaskForest :: [TaskTree] -> String
traceTaskForest trees = toJSON [traceTaskTree tree \\ tree <- trees]
