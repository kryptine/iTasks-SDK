definition module TaskPanel

import JSON, TUIDefinition, Types, TaskTree
from TUIDiff import :: TUIUpdate

:: TaskPanel
	= TaskDone
	| TaskNotDone
	| TaskRedundant
	| TTCInteractiveContainer !TTCInteractiveContainer
	| TTCResultContainer !TTCResultContainer

:: TTCInteractiveContainer = 
	{ xtype			:: !String
	, content		:: !(Maybe TUIDef)
	, updates		:: !(Maybe [TUIUpdate])
	, menu			:: ![TUIDef]
	, type			:: !InteractiveTaskType
	}
	
:: TTCResultContainer =
	{ xtype			:: !String
	, subject		:: !String
	, result		:: !String
	}

buildTaskPanel 		:: !UITreeContainer			-> TaskPanel
buildResultPanel 	:: !UITreeContainer			-> TaskPanel
diffTaskPanels		:: !TaskPanel !TaskPanel	-> TaskPanel
