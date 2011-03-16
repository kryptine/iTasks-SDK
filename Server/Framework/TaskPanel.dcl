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
	, menu			:: ![TUIDef]
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
	, menu			:: ![TUIDef]
	}

buildTaskPanel 		:: !UITreeContainer	-> TaskPanel
buildResultPanel 	:: !UITreeContainer	-> TaskPanel
