definition module LayoutCombinators

import SystemTypes, TUIDefinition

from Task import :: TaskCompositionType, :: TaskAttribute, :: TaskAction, :: TaskTUIRep

import Maybe

//Defines which layout is used by default
DEFAULT_LAYOUT	:== heuristicLayout

// Definition of a layout algorithm
// The same layouts are used for layouting out forms of basic tasks as
// well as combinations of tasks

:: Layout		:== TaskCompositionType [TaskTUIRep] [TaskAction] [TaskAttribute] -> TaskTUIRep

// These types are used to specify modifications to layouts
:: SetLayout	= SetLayout Layout
:: ModifyLayout	= ModifyLayout (Layout -> Layout)
:: BeforeLayout	= BeforeLayout ((TaskCompositionType,[TaskTUIRep],[TaskAction],[TaskAttribute]) -> (TaskCompositionType,[TaskTUIRep],[TaskAction],[TaskAttribute]))
:: AfterLayout	= AfterLayout (TaskTUIRep -> TaskTUIRep)

/**
* This is a layout following some simple layout heuristics. It puts its content in a
* panel if a title attribute is available. If actions placed in the panels
* when possible or accumulated otherwise.
*/
heuristicLayout :: Layout
/**
* This is a very simple layout which accumulates actions,
* wraps all guis in a container and overwrites/appends attributes
*/
accumulatingLayout :: Layout
/**
* This layout puts all of its parts into a panel.
*/
paneledLayout :: Layout
/**
* This layout arranges its parts into a tab panel.
*/
tabbedLayout :: Layout
/**
* This layout hides the gui, but accumulates actions and attributes
*/
hideLayout :: Layout
/**
* Fill out available space
*/
fillLayout :: TUIDirection -> Layout
/**
* Use the gui of a specific part, but keep merge attributes and actions of all parts
*/
partLayout :: Int -> Layout
/**
* Split the available space into two areas with their own layout
*/
splitLayout :: TUISide TUIFixedSize ([TaskTUIRep] -> ([TaskTUIRep],[TaskTUIRep])) Layout Layout -> Layout
/**
* Split available space into a main area and a side panel.
*/
sideLayout :: TUISide TUIFixedSize Layout -> Layout

//PLEASE DON'T USE (For backwards compat only)
vsplitLayout :: Int ([TUIDef] -> ([TUIDef],[TUIDef])) -> Layout

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
setDirection	:: !TUIDirection		!TUIDef -> TUIDef
setHalign		:: !TUIHAlign			!TUIDef -> TUIDef
setValign		:: !TUIVAlign			!TUIDef -> TUIDef
setPurpose		:: !String				!TUIDef -> TUIDef
setTaskId		:: !String				!TUIDef -> TUIDef
setListId		:: !String				!TUIDef -> TUIDef

//Combinators on interface definitions
hjoin :: ![TUIDef] -> TUIDef
vjoin :: ![TUIDef] -> TUIDef

paneled :: !(Maybe String) !(Maybe String) !(Maybe String) ![TUIDef] -> TUIDef

//Operations on containers
addItemToTUI	:: (Maybe Int) TUIDef TUIDef -> TUIDef
addMenusToTUI	:: [TUIMenuButton] TUIDef -> TUIDef
getItemsOfTUI	:: TUIDef -> [TUIDef]
setItemsOfTUI	:: [TUIDef] TUIDef -> TUIDef

//Coercion between different types of containers
toPanel			:: !TUIDef -> TUIDef
toContainer		:: !TUIDef -> TUIDef
toTab			:: !TUIDef -> TUIDef

//Predefined panels
hintPanel		:: !String		-> TUIDef	//Panel with task instructions
buttonPanel		:: ![TUIDef]	-> TUIDef	//Container for a set of horizontally layed out buttons
isButtonPanel	:: !TUIDef		-> Bool		//Test if some component is a button panel

//Predefined action placement
actionsToButtons			:: ![TaskAction]	-> (![TUIDef],![TaskAction])
actionsToMenus				:: ![TaskAction]	-> (![TUIMenuButton],![TaskAction])

//Util

tuiOf			:: TaskTUIRep -> TUIDef
actionsOf		:: TaskTUIRep -> [TaskAction]
attributesOf	:: TaskTUIRep -> [TaskAttribute]

mergeAttributes :: [TaskAttribute] [TaskAttribute] -> [TaskAttribute]

appLayout		:: Layout TaskCompositionType [TaskTUIRep] [TaskAction] [TaskAttribute] -> TaskTUIRep
appDeep			:: [Int] (TUIDef -> TUIDef) TUIDef -> TUIDef	//Modify an element inside the tree of components

tweakTUI		:: (TUIDef -> TUIDef) TaskTUIRep -> TaskTUIRep
tweakAttr		:: ([TaskAttribute] -> [TaskAttribute]) TaskTUIRep -> TaskTUIRep 
