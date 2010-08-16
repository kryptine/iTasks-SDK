definition module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB

derive JSONEncode TaskPanel

:: TaskPanel
	= TaskDone
	| TaskNotDone
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
	{ form		:: ![TUIDef]
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
	, subject		:: !String
	, description	:: !String
	}
	
:: TTCMessageContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, content		:: !FormContent
	, subtaskId		:: !(Maybe String)
	, subject		:: !String
	, description	:: !String
	}

:: TTCMonitorContainer =
	{ xtype			:: !String
	, id 			:: !String 
	, taskId		:: !String
	, html			:: !String 
	, subtaskId 	:: !(Maybe String)
	, subject		:: !String
	, description	:: !String 
	}
	
:: TTCInstructionContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, context		:: !(Maybe String)
	, subtaskId		:: !(Maybe String)
	, subject		:: !String
	, description	:: !String
	}
	
:: TTCResultContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, subject		:: !String
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
	, subject		:: !String
	, description	:: !String
	, subtaskInfo	:: ![SubtaskInfo]
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
	, focus		:: !Bool
	}

:: SubtaskInfo =
	{ finished		:: !Bool
	, taskId		:: !String
	, subject		:: !String
	, description	:: !String
	, delegatedTo	:: !String
	}

buildTaskPanel 		:: !TaskTree !(Maybe [Menu]) !Bool !User	-> TaskPanel
buildResultPanel 	:: !TaskTree 					 			-> TaskPanel