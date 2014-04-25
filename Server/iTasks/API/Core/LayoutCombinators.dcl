definition module iTasks.API.Core.LayoutCombinators

import iTasks.API.Core.Types
from iTasks.API.Core.TaskCombinators import class tune

from iTasks.Framework.TaskState import :: TIMeta
import iTasks.Framework.UIDefinition

from Data.Maybe import :: Maybe

// Definition of a layout as collection of combination functions
:: LayoutRules =
	{ accuInteract	:: UIDef UIForm                         -> UIForm       //Combine the prompt and editor of an interact
	, accuStep		:: UIDef [UIAction]                     -> UIDef				//Combine current definition with the step actions
	, accuParallel	:: [UIDef] [UIAction]                   -> UIDef		        //Combine the prompt, parts of a parallel composition and possible actions
	, accuWorkOn	:: UIDef TIMeta                         -> UIDef		        //When a detached task is worked on

    , layoutSubEditor	:: UIForm                           -> [(UIControl,UIAttributes)] //Combine multiple controls in editors
    , layoutForm        :: UIForm                           -> UIBlock              //Lay out the controls of a control stack to create a sub-user interface
    , layoutBlocks      :: [UIBlock] [UIAction]             -> UIBlock              //Combine a stack of sub-user interfaces into one
	}

:: UIFormCombinator     :== UIForm -> UIBlock
:: UIBlocksCombinator   :== [UIBlock] [UIAction] -> UIBlock

// These types are used to specify modifications to layouts
:: SetLayout	= SetLayout LayoutRules
:: AfterLayout	= AfterLayout (UIDef -> UIDef)
:: ModifyLayout	= ModifyLayout (LayoutRules -> LayoutRules)

:: SetValueAttribute a = SetValueAttribute !String (a -> String)

/**
* This is a layout that aims to automatically determine a simple, but
* functional and visually pleasing layout by following some simple layout heuristics.
*/
autoLayoutRules :: LayoutRules

//Partial layouts of autolayout
autoAccuInteract        :: UIDef UIForm -> UIForm
autoAccuStep            :: UIDef [UIAction]-> UIDef
autoAccuParallel        :: [UIDef] [UIAction] -> UIDef
autoAccuWorkOn          :: UIDef TIMeta -> UIDef

autoLayoutSubEditor     :: UIForm -> [(UIControl,UIAttributes)]
autoLayoutForm          :: UIForm -> UIBlock
autoLayoutBlocks        :: [UIBlock] [UIAction] -> UIBlock

//Applied automatically when a published has a UI other than UIFinal
autoLayoutFinal        :: UIDef -> UIDef

//Alternative plain final layout
plainLayoutFinal       :: UIDef -> UIDef

//Placement tuning types
:: InWindow         = InWindow          //Indicate that a task should be put in a window
:: InPanel          = InPanel           //Indicate that a task should be wrapped in a panel
:: InContainer      = InContainer       //Indicate that a task should be wrapped in a panel
:: FullScreen       = FullScreen        //Indicate that the full screen space should be used during final layout

instance tune InWindow
instance tune InPanel
instance tune InContainer
instance tune FullScreen

//Attribute tuning types
instance tune Title
instance tune Icon
instance tune Attribute

:: NoUserInterface  = NoUserInterface   //Don't create a user interface for this task
instance tune NoUserInterface

:: ForceLayout = ForceLayout            //Force layout ofo accumulated user interface parts so far
instance tune ForceLayout

//Alternative Sub-UI Combinators and their layout tuning types
:: ArrangeVertical = ArrangeVertical
instance tune ArrangeVertical
arrangeVertical         ::                      UIBlocksCombinator

:: ArrangeHorizontal = ArrangeHorizontal
instance tune ArrangeHorizontal
arrangeHorizontal       ::                      UIBlocksCombinator

:: ArrangeWithTabs = ArrangeWithTabs
instance tune ArrangeWithTabs
arrangeWithTabs         ::                      UIBlocksCombinator


:: ArrangeWithSideBar = ArrangeWithSideBar !Int !UISide !Int !Bool
instance tune ArrangeWithSideBar
/*
* @param Index of the task in the set that should be put in the sidebar
* @param Location of the sidebar
* @param Initial size of the sidebar
* @param Enable resize?
*/
arrangeWithSideBar      :: !Int !UISide !Int !Bool -> UIBlocksCombinator

:: ArrangeCustom = ArrangeCustom UIBlocksCombinator
instance tune ArrangeCustom

subUIToControl      :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToContainer    :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToPanel        :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])

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

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes

//Predefined action placement
actionsToButtons			:: ![UIAction]	-> (![UIControl],![UIKeyAction],![UIAction])
actionsToMenus				:: ![UIAction]	-> (![UIControl],![UIKeyAction],![UIAction])
actionsToCloseId			:: ![UIAction]	-> (!Maybe String,![UIAction])

tweakUI			:: (UIControl -> UIControl) UIDef -> UIDef
tweakAttr		:: (UIAttributes -> UIAttributes) UIDef -> UIDef 
tweakControls	:: ([(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]) UIDef -> UIDef

decorateControls    :: [(UIControl,UIAttributes)] -> [UIControl]
decorateControl     :: Bool (!UIControl,!UIAttributes) -> UIControl
