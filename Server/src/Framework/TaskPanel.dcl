definition module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB


derive JSONEncode TaskProperties, TaskSystemProperties, TaskManagerProperties, TaskWorkerProperties, TaskPriority, TaskProgress, SubtaskInfo

derive JSONEncode TaskPanel, TTCParallelContainer, TTCGroupContainer
derive JSONEncode TTCFormContainer, TTCMonitorContainer, TTCResultContainer, TTCProcessControlContainer	

:: TaskPanel
	= TaskDone
	| TaskRedundant
	| TTCFormContainer TTCFormContainer
	| TTCMonitorContainer TTCMonitorContainer
	| TTCInstructionContainer TTCInstructionContainer
	| TTCResultContainer TTCResultContainer
	| TTCProcessControlContainer TTCProcessControlContainer
	| TTCParallelContainer TTCParallelContainer
	| TTCGroupContainer TTCGroupContainer
	
:: SubtaskNr :== [Int]

:: SubtaskContainer =
	{ subtaskNr				:: !SubtaskNr
	, inClosedPar			:: !Bool
	, tasktree				:: !TaskTree
	, taskpanel				:: !TaskPanel
	, manager				:: !UserName
	, processProperties		:: !(Maybe TaskProperties)
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

:: TTCMonitorContainer =
	{ xtype			:: !String
	, id 			:: !String 
	, taskId		:: !String 
	, html 			:: !String 
	, subtaskId 	:: !(Maybe String)
	}
	
:: TTCInstructionContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, label			:: !String
	, instruction	:: !String
	, context		:: !(Maybe String)
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
	, properties	:: !(Maybe TaskProperties)
	, taskId		:: !String
	, subject		:: !String
	, delegatedTo	:: !String
	, subtaskId		:: !String
	, description	:: !String
	}

buildTaskPanel :: !TaskTree !(Maybe [Menu]) !UserName !*TSt -> (!TaskPanel,!*TSt)