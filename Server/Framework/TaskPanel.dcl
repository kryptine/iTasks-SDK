definition module TaskPanel

import JSON, TUIDefinition, TSt, ProcessDB

derive JSONEncode TaskPanel, InteractiveTaskType

:: TaskPanel
	= TaskDone
	| TaskNotDone
	| TaskRedundant
	| TTCInteractiveContainer TTCInteractiveContainer
	| TTCMonitorContainer TTCMonitorContainer
	| TTCResultContainer TTCResultContainer
	| TTCProcessControlContainer TTCProcessControlContainer
	| TTCParallelContainer TTCParallelContainer
	| TTCGroupContainer TTCGroupContainer

:: SubtaskNr :== [Int]

:: FormContent =
	{ form		:: ![TUIDef]
	, buttons	:: ![TUIDef]
	}
	
:: InteractiveTaskType = Information | Message | Instruction

:: TTCInteractiveContainer = 
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, content		:: !(Maybe FormContent)
	, updates		:: !(Maybe [TUIUpdate])
	, subject		:: !String
	, description	:: !String
	, menu			:: !Maybe [TUIDef]
	, formWidth		:: !Maybe FormWidth
	, type			:: !InteractiveTaskType
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
	, menu			:: !Maybe [TUIDef]
	, bbar			:: !Maybe [TUIDef]
	}

:: TTCGroupContainerElement =
	{ panel		:: !TaskPanel
	, behaviour	:: !GroupedBehaviour
	, index		:: !String
	, focus		:: !Bool
	}

buildTaskPanel 		:: !TaskTree !User	-> TaskPanel
buildResultPanel 	:: !TaskTree 		-> TaskPanel