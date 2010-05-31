definition module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB


derive JSONEncode TaskProperties, SystemProperties, ManagerProperties, WorkerProperties, TaskPriority, TaskProgress, SubtaskInfo

derive JSONEncode TaskPanel, TTCParallelContainer, TTCGroupContainer
derive JSONEncode TTCFormContainer, TTCMonitorContainer, TTCResultContainer, TTCProcessControlContainer	

:: TaskPanel
	= TaskDone
	| TaskRedundant
	| TTCFormContainer TTCFormContainer
	| TTCMessageContainer TTCMessageContainer
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
	, manager				:: !User
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
	, hotkeys		:: ![(!String, !Hotkey)]
	, subtaskId		:: !(Maybe String)
	, description	:: !String
	}
	
:: TTCMessageContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, content		:: !FormContent
	, hotkeys		:: ![(!String, !Hotkey)]
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

:: TTCGroupContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, content		:: ![GroupContainerElement]
	, subtaskId		:: !(Maybe String)
	, groupAMenu	:: ![TUIDef]
	}

:: GroupContainerElement =
	{ panel		:: !TaskPanel
	, behaviour	:: !GroupedBehaviour
	, index		:: !String
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

buildTaskPanel :: !TaskTree !(Maybe [Menu]) !User !*TSt -> (!TaskPanel,!*TSt)