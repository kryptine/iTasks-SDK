definition module TaskPanel

import JSON, TUIDefinition, Types, TaskTree

derive JSONEncode TaskPanel, InteractiveTaskType, TUIUpdate, TUIDef

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
	{ form		:: !TUIDef
	, buttons	:: ![TUIDef]
	}

:: TTCInteractiveContainer = 
	{ xtype			:: !String
	, id			:: !String
	, taskId		:: !TaskId
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
	, taskId		:: !TaskId
	, subject		:: !String
	, result		:: !String
	}
	
:: TTCParallelContainer =
	{ xtype			:: !String
	, taskId		:: !TaskId
	, subject		:: !String
	, description	:: !String
	, content		:: ![TaskPanel]
	}
	
:: TTCGroupContainer =
	{ xtype			:: !String
	, taskId		:: !TaskId
	, subject		:: !String
	, description	:: !String
	, content		:: ![TTCGroupContainerElement]
	, subtaskId		:: !(Maybe String)
	, menu			:: !Maybe [TUIDef]
	, bbar			:: !Maybe [TUIDef]
	}

:: TTCGroupContainerElement =
	{ panel		:: !TaskPanel
	, index		:: !String
	, focus		:: !Bool
	}

buildTaskPanel 		:: !UITree !User	-> TaskPanel
buildResultPanel 	:: !UITree 			-> TaskPanel
