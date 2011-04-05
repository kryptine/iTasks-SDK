definition module TaskPanel

import JSON, TUIDefinition, Types, TaskTree
from TUIDiff import :: TUIUpdate

:: TaskPanel
	= TaskDone
	| TaskNotDone
	| TaskRedundant
	| TTCInteractiveContainer !TTCInteractiveContainer
	| TTCResultContainer !TTCResultContainer
	| TTCParallelContainer !TTCParallelContainer

:: TTCInteractiveContainer = 
	{ xtype			:: !String
	, content		:: !(Maybe TUIDef)
	, updates		:: !(Maybe [TUIUpdate])
	, subject		:: !String
	, description	:: !String
	, menu			:: ![TUIDef]
	, formWidth		:: !Maybe FormWidth
	, type			:: !InteractiveTaskType
	}
	
:: TTCResultContainer =
	{ xtype			:: !String
	, subject		:: !String
	, result		:: !String
	}
	
:: TTCParallelContainer =
	{ xtype			:: !String
	, subject		:: !String
	, description	:: !String
	, content		:: ![TaskPanel]
	, menu			:: ![TUIDef]
	}

buildTaskPanel 		:: !UITreeContainer			-> TaskPanel
buildResultPanel 	:: !UITreeContainer			-> TaskPanel
diffTaskPanels		:: !TaskPanel !TaskPanel	-> TaskPanel
