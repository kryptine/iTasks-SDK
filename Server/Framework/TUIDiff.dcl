definition module TUIDiff

import GenUpdate, TUIDefinition

:: TUIUpdate
	//Leaf updates
	= TUISetValue		!TUIPath !JSONNode					// Set the value of a component
	| TUISetTaskId		!TUIPath !TaskId					// Set taskId a component belongs to
	| TUISetName		!TUIPath !TUIName					// Set name of a component
	| TUISetEnabled		!TUIPath !Bool						// Enable/disable form elements
	| TUISetTitle		!TUIPath !(!String,!Maybe String)	// Set title & icon of a layout container
	| TUISetSize		!TUIPath !TUISize !TUISize			// Set component's size (width & height)
	| TUIUpdate			!TUIPath !TUIDef					// Let a component update itself with a new TUI definition (for custom components)
	//Structure edits
	| TUIAdd			!TUIPath !Int !TUIDef				//Add child element at index
	| TUIRemove			!TUIPath !Int						//Remove child element at index
	| TUIReplace		!TUIPath !Int !TUIDef
	
:: TUIPath	:== String

diffTUIDefinitions :: !TUIDef !TUIDef -> [TUIUpdate]
