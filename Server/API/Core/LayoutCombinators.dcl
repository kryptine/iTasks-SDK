definition module LayoutCombinators

import SystemTypes, TUIDefinition

from Task import :: TaskAttribute
from Task import :: TaskAction
from Task import :: TaskTUI

import Maybe

//Defines which layout is used by default
DEFAULT_LAYOUT	:== panelingLayout

// Definition of a layout algorithm
// The same layouts are used for layouting out forms of basic tasks as
// well as combinations of tasks

:: Layout		= Layout !([TaskTUI] [TaskAction] [TaskAttribute] -> TaskTUI)

// These types are used to specify modifications to layouts

:: BeforeLayout	= BeforeLayout (([TaskTUI],[TaskAction],[TaskAttribute]) -> ([TaskTUI],[TaskAction],[TaskAttribute]))
:: AfterLayout	= AfterLayout (TaskTUI -> TaskTUI)
/**
* This is a very simple layout which accumulates actions,
* wraps all guis in a container and overwrites/appends attributes
*/
accumulatingLayout :: Layout
/**
* This is a simple accumulating layout, which puts its content in a
* panel if a title attribute is available. If actions placed in the panels
* when possible or accumulated otherwise.
*/
panelingLayout :: Layout
/**
* This layout arranges its parts into a tab panel.
*/
tabbedLayout :: Layout
/**
* This layout hides the gui, but accumulates actions and attributes
*/
hideLayout :: Layout

//Useful functions for tweaking or roll-your-own layouts

//Modifiers on interface definitions
setSize			:: !TUISize	!TUISize	!TUIDef -> TUIDef
setWidth		:: !TUISize				!TUIDef -> TUIDef
setHeight		:: !TUISize				!TUIDef -> TUIDef
fill			:: 						!TUIDef -> TUIDef
fillHeight		:: 						!TUIDef -> TUIDef
fillWidth		:: 						!TUIDef -> TUIDef
fixedHeight		:: !Int 				!TUIDef -> TUIDef
fixedWidth		:: !Int 				!TUIDef -> TUIDef
wrapHeight		::						!TUIDef -> TUIDef
wrapWidth		:: 						!TUIDef -> TUIDef
setMargins		:: !Int !Int !Int !Int	!TUIDef -> TUIDef
setTopMargin	:: !Int 				!TUIDef -> TUIDef
setRightMargin	:: !Int 				!TUIDef -> TUIDef
setBottomMargin	:: !Int 				!TUIDef -> TUIDef
setLeftMargin	:: !Int 				!TUIDef -> TUIDef
setPadding		:: !Int					!TUIDef -> TUIDef
setTitle 		:: !String 				!TUIDef -> TUIDef
setFramed		:: !Bool				!TUIDef -> TUIDef
setIconCls		:: !String				!TUIDef -> TUIDef
setBaseCls		:: !String				!TUIDef -> TUIDef

//Combinators on interface definitions
hjoin :: ![TUIDef] -> TUIDef
vjoin :: ![TUIDef] -> TUIDef

paneled :: !String !(Maybe String) ![TUIDef] -> TUIDef

//Operations on containers
addItemToTUI	:: TUIDef TUIDef -> TUIDef
addMenusToTUI	:: [TUIMenuButton] TUIDef -> TUIDef

//Coercion between different types of containers
toPanel	:: !TUIDef -> TUIDef
toTab	:: !TUIDef -> TUIDef

//Predefined panels
hintPanel		:: !String		-> TUIDef	//Panel with task instructions
buttonPanel		:: ![TUIDef]	-> TUIDef	//Container for a set of horizontally layed out buttons
isButtonPanel	:: !TUIDef		-> Bool		//Test if some component is a button panel

//Predefined action placement
actionsToButtons			:: ![TaskAction]	-> (![TUIDef],![TaskAction])
actionsToMenus				:: ![TaskAction]	-> (![TUIMenuButton],![TaskAction])

//Util
tuiOf			:: TaskTUI -> TUIDef
actionsOf		:: TaskTUI -> [TaskAction]
attributesOf	:: TaskTUI -> [TaskAttribute]

appLayout		:: Layout [TaskTUI] [TaskAction] [TaskAttribute] -> TaskTUI
appDeep			:: [Int] (TUIDef -> TUIDef) TUIDef -> TUIDef	//Modify an element inside the tree of components