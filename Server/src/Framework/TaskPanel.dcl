definition module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB

derive JSONEncode MonitorPanel, MainTaskPanel
derive JSONEncode TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, SubtaskInfo
derive JSONEncode STMonitorPanel, STMainTaskPanel

derive JSONEncode TaskPanel,TTCParallelContainer, TTCFormContainer

:: TaskPanel
	//OLD STUFF
	=
	MonitorPanel MonitorPanel
	| MainTaskPanel MainTaskPanel
	| STMonitorPanel STMonitorPanel
	| STMainTaskPanel STMainTaskPanel
	| TaskDone
	| TaskRedundant
	//NEW STUFF	
	| TTCFormContainer TTCFormContainer
	//| TTCMonitorContainer
	//| TTCProcessControlContainer
	| TTCParallelContainer TTCParallelContainer
	| TTCGroupContainer TTCGroupContainer
	

:: SubtaskNr :== [Int]

:: SubtaskContainer =
	{ subtaskNr		:: SubtaskNr
	, inClosedPar	:: Bool
	, tasktree		:: TaskTree
	, taskpanel		:: TaskPanel
	, manager		:: UserName
	}

//==== NEW ======
:: TTCFormContainer = 
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, items			:: !(Maybe [TUIDef])
	, updates		:: !(Maybe [TUIUpdate])
	, tbar			:: ![TUIDef]
	, subtaskId		:: !(Maybe String)
	}
	
:: TTCParallelContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, label			:: !String
	, subtaskInfo	:: ![SubtaskInfo]
	, content		:: ![TaskPanel]
	}
	
:: TTCGroupContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, label			:: !String
	, subtaskInfo	:: ![SubtaskInfo] //todo: remove
	, content		:: ![TaskPanel]
	}

//==== OLD ======	
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

:: SubtaskInfo =
	{ finished		:: Bool
	, taskId		:: String
	, subject		:: String
	, delegatedTo	:: String
	, subtaskId		:: String
	, description	:: String
	}

buildTaskPanel :: !TaskTree !(Maybe [Menu]) !UserName !*TSt -> (!TaskPanel,!*TSt)