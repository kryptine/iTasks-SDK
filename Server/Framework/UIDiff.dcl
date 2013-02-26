definition module UIDiff

import UIDefinition
from Task import :: Event

:: UIUpdate = UIUpdate !UIPath !UIUpdateOperation
:: UIUpdateOperation
	//Component updates
	= UISetValue		!JSONNode			// Set the value of a component
	| UISetOptions		!JSONNode			// Change the options in a choice component
	| UISetTaskId		!String				// Set taskId a component belongs to
	| UISetEditorId		!String				// Set taskId a component belongs to
	| UISetName			!String				// Set name of a component
	| UISetEnabled		!Bool				// Enable/disable form elements
	| UISetActive		!Bool				// Make a tab active/inactive
	| UISetTitle		!(Maybe String)		// Set/reset title of a container
	| UISetText			!(Maybe String)		// Set/reset text of a button
	| UISetIconCls		!(Maybe String)		// Set/reset icon of component
	| UISetTooltip		!(Maybe String)		// Set/reset tooltip of a component
	| UISetHotkeys		![UIKeyAction]		// Set hotkeys for a container
	| UISelfUpdate		!UIControl			// Let a component update itself with a new UI definition (for custom components)
	//Structure edits
	| UIAdd				!Int !UIControl		//Add child element at index
	| UIRemove			!Int				//Remove child element at index
	| UIReplace			!Int !UIControl		//Replace child element at index
	| UIAddWindow		!Int !UIWindow		//Add a window
	| UIRemoveWindow	!Int 				//Remove a window
	//Changing size
	| UIResize			!UISizeOpts
	
:: UIPath :== [UIStep] 
:: UIStep
	= ItemStep !Int		//Select item i
	| MenuStep			//Select the menu bar
	| WindowStep !Int	//Select window i (only possible as first step)

diffUIDefinitions :: !UIDef !UIDef !Event -> [UIUpdate]	

encodeUIUpdates :: ![UIUpdate] -> JSONNode
