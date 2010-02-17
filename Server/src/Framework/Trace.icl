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
	mkHeader			= TrTag [] [ThTag [] [Text "Id"],ThTag [] [Text "Subject"],ThTag [] [Text "Owner"],ThTag [] [Text "Delegator"], ThTag [] [Text "Status"],ThTag [] [Text "Parent"], ThTag [] [Text "Active changes"] ]
	mkRow process		= TrTag []	[ TdTag [] [Text process.Process.processId]
							, TdTag [] [Text process.Process.properties.managerProps.subject]
							, TdTag [] [Text (toString (fst process.Process.properties.managerProps.worker) +++ ": " +++ snd process.Process.properties.managerProps.worker)]
							, TdTag [] [Text (toString (fst process.Process.properties.systemProps.manager) +++ ": " +++ snd process.Process.properties.systemProps.manager)]
							, TdTag [] [Text (toString process.Process.status)]
							, TdTag [] (case process.Process.parent of
											""	= [Text "N/A"]
											x	= [Text (toString x)]
										)
							, TdTag [] [Text (join ", " (map fst process.Process.changes))]
							]

traceTaskTree :: TaskTree -> TraceTree
traceTaskTree tree = mkTree tree
where
	mkTree (TTInteractiveTask info _ ) 
		= { cls = "master-task"
		  , user = ""
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
		  , user = ""
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-mon"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "MON"
		  , children = []
		  }
	
	mkTree (TTRpcTask info _ )
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = True
		  , iconCls = "task-rpc"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "RPC"
		  , children = []
		  }		  
		  
	mkTree (TTSequenceTask info trees)
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = checkIfLeaf trees
		  , iconCls = "task-seq"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "SEQ"
		  , children = [traceTaskTree tree \\ tree <- trees]
		  }
	
	mkTree (TTParallelTask info trees)
		= { cls = "master-task"
		  , user = ""
		  , uiProvider = "col"
		  , leaf = checkIfLeaf trees
		  , iconCls = "task-par"
		  , taskId = info.TaskInfo.taskId
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "PAR"
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
		  , taskLabel = toString (Text info.TaskInfo.taskLabel)
		  , traceValue = info.TaskInfo.traceValue
		  , taskClass = "MNT"
		  , children = [traceTaskTree tree \\ tree <- trees]
		  }
	
	mkTree (TTFinishedTask info)
		= { cls = "master-task"
		  , user = ""
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
