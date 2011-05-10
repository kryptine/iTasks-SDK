definition module TaskPanel

import JSON, TUIDefinition, Types, TaskTree
from TUIDiff import :: TUIUpdate

:: TaskPanel
	= TaskDone
	| TaskNotDone
	| TaskRedundant
	| TUIPanel !TUIPanel

:: TUIPanel = 
	{ content		:: !(Maybe TUIDef)
	, updates		:: !(Maybe [TUIUpdate])
	, menu			:: ![TUIDef]
	}

buildTaskPanel 		:: !UITreeContainer			-> TaskPanel
buildResultPanel 	:: !UITreeContainer			-> TaskPanel
diffTaskPanels		:: !TaskPanel !TaskPanel	-> TaskPanel
