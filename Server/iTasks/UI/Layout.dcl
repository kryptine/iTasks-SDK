definition module iTasks.UI.Layout

import iTasks.API.Core.Types
from iTasks.API.Core.TaskCombinators import class tune

import iTasks.UI.Definition
import iTasks.UI.Diff

from Data.Maybe import :: Maybe

// Definition of a layout as collection of combination functions
:: LayoutRules =
	{ accuInteract	:: ContentLayout 										// Combine the prompt and editor of an interact
	, accuStep		:: ContentLayout										// Combine current definition with the step actions
	, accuParallel	:: ContentLayout 										// Combine the prompt, parts of a parallel composition and possible actions
	, accuAttach	:: ContentLayout										// When a detached task is worked on

    , layoutForm        :: UIForm                           -> UIBlock      //Lay out the controls of a control stack to create a sub-user interface
    , layoutBlocks      :: [UIBlock] [UIAction]             -> UIBlock      //Combine a stack of sub-user interfaces into one
	}

:: UIFormCombinator     :== UIForm -> UIBlock
:: UIBlocksCombinator   :== [UIBlock] [UIAction] -> UIBlock

// These types are used to specify modifications to layouts
:: SetLayout	= SetLayout LayoutRules
:: AfterLayout	= AfterLayout (UIDef -> UIDef)
:: ModifyLayout	= ModifyLayout (LayoutRules -> LayoutRules)

:: SetValueAttribute a = SetValueAttribute !String (a -> String)

// When a layout changes the stucture of the UI, changes to the UI have to be
// changed too to route the changes to the correct place in the structure
:: ContentLayout =
	{ layout 	:: UIDef  		-> UIDef
	, route  	:: UIChangeDef  -> UIChangeDef
	}

/**
* This is a layout that aims to automatically determine a simple, but
* functional and visually pleasing layout by following some simple layout heuristics.
*/
autoLayoutRules :: LayoutRules

//Partial layouts of autolayout
autoAccuInteract        :: ContentLayout 
autoAccuStep            :: ContentLayout
autoAccuParallel        :: ContentLayout
autoAccuAttach 			:: ContentLayout

autoLayoutForm          :: UIForm -> UIBlock
autoLayoutBlocks        :: [UIBlock] [UIAction] -> UIBlock

//Applied automatically when a published has a UI other than UIFinal
autoLayoutFinal        :: ContentLayout

//Alternative plain final layout
plainLayoutFinal       :: ContentLayout

//Generation of prompts
class descr d
where
	toPrompt		:: !d -> UIDef

instance descr ()                           //No prompt
instance descr String						//Simple instruction
instance descr (!String, !String)			//Title attribute + instruction

//Placement tuning types
:: ToWindow     = ToWindow UIWindowType UIVAlign UIHAlign

InWindow                :== InFloatingWindow  //Indicate that a task should be put in a window
InFloatingWindow        :== ToWindow FloatingWindow AlignMiddle AlignCenter
InNotificationBubble    :== ToWindow NotificationBubble AlignTop AlignRight
InModalDialog           :== ToWindow ModalDialog AlignMiddle AlignCenter

:: InPanel          = InPanel           //Indicate that a task should be wrapped in a panel
:: InContainer      = InContainer       //Indicate that a task should be wrapped in a panel
:: FullScreen       = FullScreen        //Indicate that the full screen space should be used during final layout

instance tune ToWindow
instance tune InPanel
instance tune InContainer
instance tune FullScreen

//Attribute tuning types
instance tune Title
instance tune Label
instance tune Icon
instance tune Attribute

:: NoUserInterface  = NoUserInterface   //Don't create a user interface for this task
instance tune NoUserInterface

:: ForceLayout = ForceLayout            //Force layout ofo accumulated user interface parts so far
instance tune ForceLayout

//Alternative block combinators and their layout tuning types
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

/*
* @param Direction to split the available space in
* @param Enable resize?
*/
:: ArrangeSplit = ArrangeSplit !UIDirection !Bool
instance tune ArrangeSplit
arrangeSplit            :: !UIDirection !Bool -> UIBlocksCombinator

:: ArrangeCustom = ArrangeCustom UIBlocksCombinator
instance tune ArrangeCustom

blockToControl      :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
blockToContainer    :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
blockToPanel        :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])

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
