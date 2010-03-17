definition module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB

derive JSONEncode FormPanel, FormUpdate, MonitorPanel, MainTaskPanel, ParallelInfoPanel
derive JSONEncode TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, SubtaskInfo
derive JSONEncode STFormPanel, STFormUpdate, STMonitorPanel, STMainTaskPanel
derive JSONEncode TaskPanel

:: TaskPanel
	= FormPanel FormPanel
	| FormUpdate FormUpdate
	| MonitorPanel MonitorPanel
	| MainTaskPanel MainTaskPanel
	| ParallelInfoPanel ParallelInfoPanel
	| STFormPanel STFormPanel
	| STFormUpdate STFormUpdate
	| STMonitorPanel STMonitorPanel
	| STMainTaskPanel STMainTaskPanel
	| TaskDone
	| TaskRedundant

:: SubtaskNr :== [Int]

:: SubtaskContainer =
	{ subtaskNr		:: SubtaskNr
	, inClosedPar	:: Bool
	, tasktree		:: TaskTree
	, taskpanel		:: TaskPanel
	, manager		:: UserName
	}
	
:: MonitorPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, html			:: String
	}	
:: FormPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, items			:: [TUIDef]
	, tbar			:: [TUIDef]
	}
:: FormUpdate =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, updates		:: [TUIUpdate]
	}
:: MainTaskPanel =
	{ xtype			:: String
	, taskId		:: String
	, properties	:: TaskProperties
	}
:: STMonitorPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, html			:: String
	, subtaskId		:: String
	}	
:: STFormPanel =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, items			:: [TUIDef]
	, subtaskId		:: String
	, tbar			:: [TUIDef]
	}
:: STFormUpdate =
	{ xtype			:: String
	, id			:: String
	, taskId		:: String
	, updates		:: [TUIUpdate]
	, subtaskId		:: String
	}
:: STMainTaskPanel =
	{ xtype			:: String
	, taskId		:: String
	, properties	:: TaskProperties
	, subtaskId		:: String
	} 

:: ParallelInfoPanel =
	{ xtype			:: String
	, taskId		:: String
	, label			:: String
	, subtaskInfo	:: [SubtaskInfo]
	}

:: SubtaskInfo =
	{ finished		:: Bool
	, taskId		:: String
	, subject		:: String
	, delegatedTo	:: String
	, subtaskId		:: String
	, description	:: String
	}

buildTaskPanels :: !TaskTree !(Maybe [Menu]) !UserName !*TSt -> (![TaskPanel],!*TSt)