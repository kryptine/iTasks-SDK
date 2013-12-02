definition module iTasks.API.Core.LayoutCombinators

import iTasks.API.Core.SystemTypes
from iTasks.API.Core.CoreCombinators import class tune

from iTasks.Framework.Task import :: TaskCompositionType
from iTasks.Framework.TaskState import :: TIMeta
import iTasks.Framework.UIDefinition

from Data.Maybe import :: Maybe

// Definition of a layout as collection of combination functions
:: LayoutRules =
	{ accuInteract	:: UIDef UIControlStack                 -> UIControlStack       //Combine the prompt and editor of an interact
	, accuStep		:: UIDef [UIAction]                     -> UIDef				//Combine current definition with the step actions
	, accuParallel	:: UIDef [UIDef]                        -> UIDef		        //Combine the promp and parts of a parallel composition
	, accuWorkOn	:: UIDef TIMeta                         -> UIDef		        //When a detached task is worked on

    , layoutSubEditor	   :: UIControlStack               -> UIAnnotatedControls	//Combine multiple controls in editors
    , layoutControlStack   :: UIControlStack               -> UISubUI              //Lay out the controls of a control stack to create a sub-user interface
    , layoutSubUIStack     :: UISubUIStack                 -> UISubUI              //Combine a stack of sub-user interfaces into one
	}

:: UIControlCombinator  :== UIControlStack -> UISubUI
:: SubUICombinator      :== UISubUIStack -> UISubUI

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
autoAccuInteract        :: UIDef UIControlStack -> UIControlStack
autoAccuStep            :: UIDef [UIAction]-> UIDef
autoAccuParallel        :: UIDef [UIDef] -> UIDef
autoAccuWorkOn          :: UIDef TIMeta -> UIDef

autoLayoutSubEditor    :: UIControlStack -> UIAnnotatedControls
autoLayoutControlStack :: UIControlStack -> UISubUI
autoLayoutSubUIStack   :: UISubUIStack -> UISubUI

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

:: NoUserInterface  = NoUserInterface   //Don't create a user interface for this task
instance tune NoUserInterface

:: ForceLayout = ForceLayout            //Force layout ofo accumulated user interface parts so far
instance tune ForceLayout

//Alternative Sub-UI Combinators and their layout tuning types
:: ArrangeVertical = ArrangeVertical
instance tune ArrangeVertical
arrangeVertical         ::                      SubUICombinator

:: ArrangeHorizontal = ArrangeHorizontal
instance tune ArrangeHorizontal
arrangeHorizontal       ::                      SubUICombinator

:: ArrangeWithTabs = ArrangeWithTabs
instance tune ArrangeWithTabs
arrangeWithTabs         ::                      SubUICombinator

:: ArrangeWithSideBar = ArrangeWithSideBar !Int !UISide !Int
instance tune ArrangeWithSideBar
arrangeWithSideBar      :: !Int !UISide !Int -> SubUICombinator

:: ArrangeCustom = ArrangeCustom SubUICombinator
instance tune ArrangeCustom
toSubUIStack :: [UISubUI] -> UISubUIStack

subUIToControl      :: UISubUI -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToContainer    :: UISubUI -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
subUIToPanel        :: UISubUI -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])

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
