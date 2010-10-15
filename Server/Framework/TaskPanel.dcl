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

:: FormContent =
	{ form		:: ![TUIDef]
	, buttons	:: ![TUIDef]
	}

:: TTCFormContainer = 
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, content		:: !(Maybe FormContent)
	, updates		:: !(Maybe [TUIUpdate])
	, subject		:: !String
	, description	:: !String
	, menu			:: !Maybe [TUIDef]
	}
	
:: TTCMessageContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, content		:: !FormContent
	, subject		:: !String
	, description	:: !String
	, menu			:: !Maybe [TUIDef]
	}

:: TTCMonitorContainer =
	{ xtype			:: !String
	, id 			:: !String 
	, taskId		:: !String
	, html			:: !String 
	, subject		:: !String
	, description	:: !String 
	, menu			:: !Maybe [TUIDef]
	}
	
:: TTCInstructionContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, context		:: !(Maybe String)
	, subject		:: !String
	, description	:: !String
	, menu			:: !Maybe [TUIDef]
	}
	
:: TTCResultContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, subject		:: !String
	, result		:: !String
	}
	
:: TTCProcessControlContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, properties	:: !TaskProperties
	, menu			:: !Maybe [TUIDef]
	}
	
:: TTCParallelContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, subject		:: !String
	, description	:: !String
	, subtaskInfo	:: ![TTCParallelContainerElement]
	, menu			:: !Maybe [TUIDef]
	}

:: TTCParallelContainerElement =
	{ finished		:: !Bool
	, taskId		:: !String
	, subject		:: !String
	, description	:: !String
	, delegatedTo	:: !String
	}
	
:: TTCGroupContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, subject		:: !String
	, description	:: !String
	, content		:: ![TTCGroupContainerElement]
	, subtaskId		:: !(Maybe String)
	, groupAMenu	:: !Maybe [TUIDef]
	, menu			:: !Maybe [TUIDef]
	}

:: TTCGroupContainerElement =
	{ panel		:: !TaskPanel
	, behaviour	:: !GroupedBehaviour
	, index		:: !String
	, focus		:: !Bool
	}

buildTaskPanel 		:: !TaskTree !User	-> TaskPanel
buildResultPanel 	:: !TaskTree 		-> TaskPanel