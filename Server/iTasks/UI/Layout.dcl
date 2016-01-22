definition module iTasks.UI.Layout

import iTasks.UI.Layout.Common

import iTasks.API.Core.Types
from iTasks.API.Core.TaskCombinators import class tune

import iTasks.UI.Definition
import iTasks.UI.Diff

from Data.Maybe import :: Maybe

// When a layout changes the stucture of the UI, changes to the UI have to be
// changed too to route the changes to the correct place in the structure
:: Layout      :== LayoutFun JSONNode
:: LayoutFun s :== (UIChangeDef,s) -> (UIChangeDef,s)

// These types are used to specify when to apply layouts
:: ApplyLayout	= E.s: ApplyLayout (LayoutFun s) & iTask s
:: AutoLayout = WithAutoLayout | WithoutAutoLayout

//Addresing nodes in a UI definition
:: NodePath :== [Int]

//Basic DSL for creating more complex layouts
layoutChild :: NodePath Layout-> Layout

insertChild :: NodePath UI -> Layout
removeChild :: NodePath -> Layout

moveChild :: NodePath NodePath -> Layout
moveChildren :: NodePath (UI -> Bool) NodePath -> Layout

layoutChildrenOf :: NodePath Layout -> Layout

changeContainerType :: (UI -> UI) -> Layout

wrap :: UINodeType -> Layout

sequenceLayouts :: [Layout] -> Layout
conditionalLayout :: (UI -> Bool) Layout -> Layout
selectLayout :: [(UI -> Bool, Layout)] -> Layout

// OBSOLETE
// OBSOLETE
// OBSOLETE
// OBSOLETE

:: SetValueAttribute a = SetValueAttribute !String (a -> String)

:: InPanel          = InPanel           //Indicate that a task should be wrapped in a panel
:: InContainer      = InContainer       //Indicate that a task should be wrapped in a panel
:: FullScreen       = FullScreen        //Indicate that the full screen space should be used during final layout

instance tune InPanel
instance tune InContainer
instance tune FullScreen

//Attribute tuning types
instance tune Title
instance tune Label
instance tune Icon
instance tune Attribute


//:: ForceLayout = ForceLayout            //Force layout ofo accumulated user interface parts so far
//instance tune ForceLayout

//Alternative block combinators and their layout tuning types
//:: ArrangeVertical = ArrangeVertical
//instance tune ArrangeVertical
//arrangeVertical         ::                      UIBlocksCombinator

//:: ArrangeHorizontal = ArrangeHorizontal
//instance tune ArrangeHorizontal
//arrangeHorizontal       ::                      UIBlocksCombinator

//:: ArrangeWithSideBar = ArrangeWithSideBar !Int !UISide !Int !Bool
//instance tune ArrangeWithSideBar

/*
* @param Index of the task in the set that should be put in the sidebar
* @param Location of the sidebar
* @param Initial size of the sidebar
* @param Enable resize?
*/
//arrangeWithSideBar      :: !Int !UISide !Int !Bool -> UIBlocksCombinator

/*
* @param Direction to split the available space in
* @param Enable resize?
*/

//:: ArrangeCustom = ArrangeCustom UIBlocksCombinator
//instance tune ArrangeCustom

//blockToControl      :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
//blockToContainer    :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
//blockToPanel        :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])

//Combinators on interface definitions
hjoin :: ![UI] -> UI
vjoin :: ![UI] -> UI

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes

//Predefined action placement
//actionsToButtons			:: ![UIAction]	-> (![UIControl],![UIKeyAction],![UIAction])
//actionsToMenus				:: ![UIAction]	-> (![UIControl],![UIKeyAction],![UIAction])
actionsToCloseId			:: ![UIAction]	-> (!Maybe String,![UIAction])

//tweakUI			:: (UIControl -> UIControl) UI -> UI
tweakAttr		:: (UIAttributes -> UIAttributes) UI -> UI
//tweakControls	:: ([(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]) UI -> UI
