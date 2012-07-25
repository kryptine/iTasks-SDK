definition module LayoutCombinators

import SystemTypes, UIDefinition

from Task import :: TaskCompositionType

import Maybe

//Defines which layout is used by default
DEFAULT_LAYOUT	:== heuristicLayout

// Definition of a layout algorithm
// The same layouts are used for layouting out forms of basic tasks as
// well as combinations of tasks

:: Layout		:== Layoutable -> UIDef

// The layoutable defines the possible situations in which layouting is required
:: Layoutable
	= DataLayout UIDef				//Reduce a composite data structure
	| InteractLayout UIDef UIDef 	//Prompt, editor	
	| StepLayout UIDef [UIAction]	//Part and actions if the step has not been made yet
	| ParallelLayout UIDef [UIDef] 	//Prompt, parallel parts
	| FinalLayout UIDef				//Reduce the final composition

// These types are used to specify modifications to layouts
:: SetLayout	= SetLayout Layout
:: ModifyLayout	= ModifyLayout (Layout -> Layout)
:: BeforeLayout	= BeforeLayout (Layoutable -> Layoutable)
:: AfterLayout	= AfterLayout (UIDef -> UIDef)

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
fillLayout :: UIDirection -> Layout
/**
* Use the gui of a specific part, but keep merge attributes and actions of all parts
*/
partLayout :: Int -> Layout
/**
* Split the available space into two areas with their own layout
*/
splitLayout :: UISide Int ([UIDef] -> ([UIDef],[UIDef])) Layout Layout -> Layout
/**
* Split available space into a main area and a side panel.
*/
sideLayout :: UISide Int Layout -> Layout

//PLEASE DON'T USE (For backwards compat only)
vsplitLayout :: Int ([UIControl] -> ([UIControl],[UIControl])) -> Layout

//Useful functions for tweaking or roll-your-own layouts

//Modifiers on interface definitions
setSize			:: !UISize	!UISize		!UIControl -> UIControl
setWidth		:: !UISize				!UIControl -> UIControl
setHeight		:: !UISize				!UIControl -> UIControl
fill			:: 						!UIControl -> UIControl
fillHeight		:: 						!UIControl -> UIControl
fillWidth		:: 						!UIControl -> UIControl
fixedHeight		:: !Int 				!UIControl -> UIControl
fixedWidth		:: !Int 				!UIControl -> UIControl
wrapHeight		::						!UIControl -> UIControl
wrapWidth		:: 						!UIControl -> UIControl
setMargins		:: !Int !Int !Int !Int	!UIControl -> UIControl
setTopMargin	:: !Int 				!UIControl -> UIControl
setRightMargin	:: !Int 				!UIControl -> UIControl
setBottomMargin	:: !Int 				!UIControl -> UIControl
setLeftMargin	:: !Int 				!UIControl -> UIControl
setPadding		:: !Int !Int !Int !Int	!UIControl -> UIControl
setTitle 		:: !String 				!UIControl -> UIControl
setFramed		:: !Bool				!UIControl -> UIControl
setIconCls		:: !String				!UIControl -> UIControl
setBaseCls		:: !String				!UIControl -> UIControl
setDirection	:: !UIDirection			!UIControl -> UIControl
setHalign		:: !UIHAlign			!UIControl -> UIControl
setValign		:: !UIVAlign			!UIControl -> UIControl
setPurpose		:: !String				!UIControl -> UIControl

//Combinators on interface definitions
hjoin :: ![UIControl] -> UIControl
vjoin :: ![UIControl] -> UIControl

paneled :: !(Maybe String) !(Maybe String) !(Maybe String) ![UIControl] -> UIControl

//Operations on containers
addItemToTUI	:: (Maybe Int) UIControl UIControl -> UIControl
addMenusToTUI	:: [UIControl] UIControl -> UIControl
getItemsOfTUI	:: UIControl -> [UIControl]
setItemsOfTUI	:: [UIControl] UIControl -> UIControl

//Coercion between different types of containers
toPanel			:: !UIControl -> UIControl
toContainer		:: !UIControl -> UIControl

//Predefined panels
hintPanel		:: !String		-> UIControl	//Panel with task instructions
buttonPanel		:: ![UIControl]	-> UIControl	//Container for a set of horizontally layed out buttons
isButtonPanel	:: !UIControl	-> Bool			//Test if some component is a button panel

//Predefined action placement
actionsToButtons			:: ![UIAction]	-> (![UIControl],![UIAction])
actionsToMenus				:: ![UIAction]	-> (![UIControl],![UIAction])

//Util
uiOf			:: UIDef -> UIControl
actionsOf		:: UIDef -> [UIAction]
attributesOf	:: UIDef -> UIAttributes

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes

appLayout		:: Layout Layoutable -> UIDef
appDeep			:: [Int] (UIControl -> UIControl) UIControl -> UIControl	//Modify an element inside the tree of components

tweakTUI		:: (UIControl -> UIControl) UIDef -> UIDef
tweakAttr		:: (UIAttributes -> UIAttributes) UIDef -> UIDef 
