definition module TaskPanel

import JSON, TUIDefinition, Types, TaskTree

derive JSONEncode TaskPanel, InteractiveTaskType

:: TaskPanel
	= TaskDone
	| TaskNotDone
	| TaskRedundant
	| TTCInteractiveContainer !TTCInteractiveContainer
	| TTCResultContainer !TTCResultContainer
	| TTCParallelContainer !TTCParallelContainer
	| TTCGroupContainer !TTCGroupContainer

:: SubtaskNr :== [Int]

:: FormContent =
	{ form		:: ![TUIDef]
	, buttons	:: ![TUIDef]
	}

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
	
:: TTCResultContainer =
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !String
	, subject		:: !String
	, result		:: !String
	}
	
:: TTCParallelContainer =
	{ xtype			:: !String
	, taskId		:: !String
	, subject		:: !String
	, description	:: !String
	, subtaskInfo	:: ![TTCParallelContainerElement]
	, content		:: ![TaskPanel]
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

buildTaskPanel 		:: !UITree !User	-> TaskPanel
buildResultPanel 	:: !UITree 			-> TaskPanel
