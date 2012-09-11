definition module LayoutCombinators

import SystemTypes, UIDefinition

from Task import :: TaskCompositionType

import Maybe

// Definition of a layout as collection of combination functions
:: Layout =
	{ editor	:: UIDef -> UIDef															//Combine multiple controls to a single one in editors
	, interact	:: UIDef UIDef -> UIDef														//Combine the prompt and editor of an interact
	, step		:: UIDef [UIAction] -> UIDef												//Combine current definition with the step actions
	, parallel	:: UIDef [UIDef] -> UIDef													//Combine the promp and parts of a parallel composition
	, final		:: UIDef -> UIDef															//Last touches to the composition
	}

// When the multiple parts of a parallel combinator need to be merged into a single definition
// we call it a parallel merger
:: ParallelMerger :== UIDef [UIDef] -> UIDef

// These types are used to specify modifications to layouts
:: SetLayout	= SetLayout Layout
:: AfterLayout	= AfterLayout (UIDef -> UIDef)
:: ModifyLayout	= ModifyLayout (Layout -> Layout)

/**
* This is a layout that aims to automatically determine a simple, but
* functional and visually pleasing layout by following some simple layout heuristics.
*/
autoLayout :: Layout
//Partial layouts of autolayout
autoEditorLayout		:: UIDef -> UIDef
autoInteractionLayout	:: UIDef UIDef -> UIDef
autoStepLayout			:: UIDef [UIAction]-> UIDef
autoParallelLayout		:: UIDef [UIDef] -> UIDef
autoFinalLayout			:: UIDef -> UIDef

/**
* This layout hides ui controls, but accumulates actions and attributes.
*/
hideLayout :: Layout
/**
* Use the gui of a specific part, but keep attributes and actions of all parts
*/
partLayout :: Int -> Layout
/**
* Use a custom function for merging parallel combinations
*/
customMergeLayout :: ParallelMerger -> Layout

minimalMerge	:: ParallelMerger
groupedMerge	:: ParallelMerger
sideMerge		:: UISide Int ParallelMerger -> ParallelMerger
splitMerge		:: UIDirection -> ParallelMerger
tabbedMerge		:: ParallelMerger

//Shorthands for layouts with custom parallel mergers
tabbedLayout				:== customMergeLayout tabbedMerge 
splitLayout direction		:== customMergeLayout (splitMerge direction)
sideLayout side size rest	:== customMergeLayout (sideMerge side size rest)

//Useful functions for tweaking or roll-your-own layouts
autoReduce :: UIDef -> UIDef


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
