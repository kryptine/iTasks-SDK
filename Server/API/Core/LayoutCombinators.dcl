definition module LayoutCombinators

import SystemTypes, UIDefinition

from Task import :: TaskCompositionType

import Maybe

// Definition of a layout algorithm
// The same layouts are used for layouting out forms of basic tasks as
// well as combinations of tasks

:: Layout		:== Layoutable -> UIDef

// The layoutable defines the possible situations in which layouting is required
:: Layoutable
	= DataLayout UIDef					//Reduce a composite data structure
	| InteractLayout UIDef UIDef 		//Prompt, editor	
	| StepLayout UIDef [UIAction] 		//Part, actions to make the step
	| ParallelLayout UIDef [UIDef] 		//Prompt, parallel parts
	| FinalLayout UIDef					//Reduce the final composition

// These types are used to specify modifications to layouts
:: SetLayout	= SetLayout Layout
:: ModifyLayout	= ModifyLayout (Layout -> Layout)
:: BeforeLayout	= BeforeLayout (Layoutable -> Layoutable)
:: AfterLayout	= AfterLayout (UIDef -> UIDef)

/**
* This is a layout that aims to automatically determine a simple, but
* functional and visually pleasing layout by following some simple layout heuristics.
*/
autoLayout :: Layout
/**
* This layout hides ui controls, but accumulates actions and attributes.
*/
hideLayout :: Layout
/**
* Split the available space into two areas with their own layout
*/
splitLayout :: UISide Int ([UIDef] -> ([UIDef],[UIDef])) Layout Layout -> Layout
/**
* Split available space into a main area and a side panel.
*/
sideLayout :: UISide Int Layout -> Layout
/**
* This layout arranges its parallel parts into a set of tabs.
*/
tabbedLayout :: Layout
/**
* Use the gui of a specific part, but keep merge attributes and actions of all parts
*/
partLayout :: Int -> Layout

//Useful functions for tweaking or roll-your-own layouts

//Modifiers on interface definitions
setSize			:: !UISize	!UISize			!UIControl -> UIControl
setWidth		:: !UISize					!UIControl -> UIControl
setHeight		:: !UISize					!UIControl -> UIControl
setMinSize		:: !UIMinSize !UIMinSize	!UIControl -> UIControl
setMinWidth		:: !UIMinSize				!UIControl -> UIControl
setMinHeight	:: !UIMinSize				!UIControl -> UIControl
fill			:: 							!UIControl -> UIControl
fillHeight		:: 							!UIControl -> UIControl
fillWidth		:: 							!UIControl -> UIControl
fixedHeight		:: !Int 					!UIControl -> UIControl
fixedWidth		:: !Int 					!UIControl -> UIControl
wrapHeight		::							!UIControl -> UIControl
wrapWidth		:: 							!UIControl -> UIControl
setMargins		:: !Int !Int !Int !Int		!UIControl -> UIControl
setTopMargin	:: !Int 					!UIControl -> UIControl
setRightMargin	:: !Int 					!UIControl -> UIControl
setBottomMargin	:: !Int 					!UIControl -> UIControl
setLeftMargin	:: !Int 					!UIControl -> UIControl
setPadding		:: !Int !Int !Int !Int		!UIControl -> UIControl
setTitle 		:: !String 					!UIControl -> UIControl
setFramed		:: !Bool					!UIControl -> UIControl
setIconCls		:: !String					!UIControl -> UIControl
setBaseCls		:: !String					!UIControl -> UIControl
setDirection	:: !UIDirection				!UIControl -> UIControl
setHalign		:: !UIHAlign				!UIControl -> UIControl
setValign		:: !UIVAlign				!UIControl -> UIControl

//Combinators on interface definitions
hjoin :: ![UIControl] -> UIControl
vjoin :: ![UIControl] -> UIControl

//Operations on containers
addItemToUI		:: (Maybe Int) UIControl UIControl -> UIControl
getItemsOfUI	:: UIControl -> [UIControl]
setItemsOfUI	:: [UIControl] UIControl -> UIControl

//Coercion between different types of containers
toPanel			:: !UIControl -> UIControl
toContainer		:: !UIControl -> UIControl

//Predefined panels
buttonPanel		:: ![UIControl]	-> UIControl	//Container for a set of horizontally layed out buttons

//Predefined action placement
actionsToButtons			:: ![UIAction]	-> (![UIControl],![UIAction])
actionsToMenus				:: ![UIAction]	-> (![UIControl],![UIAction])

//Util
uiOf			:: UIDef -> UIControl
actionsOf		:: UIDef -> [UIAction]
attributesOf	:: UIDef -> UIAttributes

mergeDefs		:: UIDef UIDef -> UIDef
mergeAttributes :: UIAttributes UIAttributes -> UIAttributes

appControls		:: (UIControl -> UIControl) UIDef -> UIDef
appDeep			:: [Int] (UIControl -> UIControl) UIControl -> UIControl	//Modify an element inside the tree of components

tweakUI			:: (UIControl -> UIControl) UIDef -> UIDef
tweakAttr		:: (UIAttributes -> UIAttributes) UIDef -> UIDef 
