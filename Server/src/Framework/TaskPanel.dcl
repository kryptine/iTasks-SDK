definition module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB


derive JSONEncode TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, SubtaskInfo

derive JSONEncode TaskPanel, TTCParallelContainer, TTCGroupContainer
derive JSONEncode TTCFormContainer, TTCMonitorContainer, TTCResultContainer, TTCProcessControlContainer	

:: TaskPanel
	= TaskDone
	| TaskRedundant
	| TTCFormContainer TTCFormContainer
	| TTCMessageContainer TTCMessageContainer
	| TTCMonitorContainer TTCMonitorContainer
	| TTCResultContainer TTCResultContainer
	| TTCProcessControlContainer TTCProcessControlContainer
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

:: FormContent =
	{ form		:: !TUIDef
	, tbar		:: ![TUIDef]
	, buttons	:: ![TUIDef]
	}

:: TTCFormContainer = 
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, content		:: !(Maybe FormContent)
	, updates		:: !(Maybe [TUIUpdate])
	, subtaskId		:: !(Maybe String)
	, description	:: !String
	}
	
:: TTCMessageContainer = 
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, message		:: !String
	, subtaskId		:: !(Maybe String)
	}
	
:: TTCMonitorContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, html			:: !String
	, subtaskId		:: !(Maybe String)
	}
	
:: TTCResultContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, label			:: !String
	, result		:: !String
	, subtaskId		:: !(Maybe String)
	}
	
:: TTCProcessControlContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, properties	:: !TaskProperties
	, subtaskId		:: !(Maybe String)
	}
	
:: TTCParallelContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, label			:: !String
	, description	:: !String
	, subtaskInfo	:: ![SubtaskInfo]
	, content		:: ![TaskPanel]
	}

:: GroupContainerElement =
	{ panel		:: TaskPanel
	, behaviour	:: GroupedBehaviour
	, index		:: Int
	}

:: TTCGroupContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, content		:: ![GroupContainerElement]
	}

:: SubtaskInfo =
	{ finished		:: !Bool
	, taskId		:: !String
	, subject		:: !String
	, delegatedTo	:: !String
	, subtaskId		:: !String
	, description	:: !String
	}

buildTaskPanel :: !TaskTree !(Maybe [Menu]) !UserName !*TSt -> (!TaskPanel,!*TSt)