definition module TUIDiff

import GenUpdate, TUIDefinition

:: TUIUpdate
	//Leaf updates
	= TUISetValue		!TUIPath !JSONNode			// Set the value of a component
	| TUISetTaskId		!TUIPath !TaskId			// Set taskId a component belongs to
	| TUISetName		!TUIPath !TUIName			// Set name of a component
	| TUISetError		!TUIPath !String			// Set the error messages on a component
	| TUISetHint		!TUIPath !String			// Set the hint messages on a component
	| TUISetEnabled		!TUIPath !Bool				// Enable/disable form elements
	//Complete replacement
	| TUIReplace		!TUIPath !TUIDef
	| TUIUpdate			!TUIPath !TUIDef			// Let a component update itself with a new TUI definition (for custom components)
	//Structure edits
	| TUIAdd			!TUIPath !Int !TUIDef		//Add child element at index
	| TUIRemove			!TUIPath !Int				//Remove child element at index
	//| TUIReorder		!TUIPath !Int !Int			//Move child element from index to index
	
:: TUIPath	:== String

diffEditorDefinitions :: !TUIDef !TUIDef -> [TUIUpdate]
