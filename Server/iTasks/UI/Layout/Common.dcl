definition module iTasks.UI.Layout.Common
/**
* This module provides common alternative layout annotations
* that you can apply at strategic points in your task specifications
* to optimize the user experience for specific tasks
*/
import iTasks.UI.Layout
from iTasks.UI.Definition import :: UISide(..), :: UIDirection(..), :: UIWindowType(..), :: UIHAlign(..), :: UIVAlign(..)
from iTasks.API.Core.Types import :: Title, :: Label, :: Icon, :: Attribute

/**
* Create a tabset with all child items as separate tabs
*/
arrangeWithTabs :: Layout

/**
* Extract one child item and put it in a separate panel at the side of the screen
*
* @param Index of the task in the set that should be put in the sidebar
* @param Location of the sidebar
* @param Initial size of the sidebar
* @param Enable resize?
*/
arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout

/**
* Divide the available screen space
*
* @param Direction to split the available space in
* @param Enable resize?
*/
arrangeSplit :: !UIDirection !Bool -> Layout

/**
*  Turn current UI into a panel and set direction to vertical.
*/
arrangeVertical :: Layout

/**
*  Turn current UI into a panel and set direction to vertical.
*/
arrangeHorizontal :: Layout

/**
* Turn the UI into a wrapping framed container inside a general container
* 
* Use this is if you don't want to use the entire viewport
*/
frameCompact :: Layout

/**
* Apply a layout only before a step has been made
*/
beforeStep :: Layout -> Layout

//Convenient annotatation types
:: ArrangeWithTabs = ArrangeWithTabs
instance tune ArrangeWithTabs

:: ArrangeWithSideBar = ArrangeWithSideBar !Int !UISide !Int !Bool
instance tune ArrangeWithSideBar

:: ArrangeSplit = ArrangeSplit !UIDirection !Bool
instance tune ArrangeSplit

:: ArrangeVertical = ArrangeVertical
instance tune ArrangeVertical

:: ArrangeHorizontal = ArrangeHorizontal
instance tune ArrangeHorizontal

//Changing container types

toContainer ::                                   Layout
toPanel     ::                                   Layout
toWindow    :: UIWindowType UIVAlign UIHAlign -> Layout
toEmpty     ::                                   Layout

:: ToWindow = ToWindow UIWindowType UIVAlign UIHAlign
InWindow                :== InFloatingWindow
InFloatingWindow        :== ToWindow FloatingWindow AlignMiddle AlignCenter
InNotificationBubble    :== ToWindow NotificationBubble AlignTop AlignRight
InModalDialog           :== ToWindow ModalDialog AlignMiddle AlignCenter
instance tune ToWindow

:: InPanel          = InPanel           //Indicate that a task should be wrapped in a panel
instance tune InPanel

:: InContainer      = InContainer       //Indicate that a task should be wrapped in a panel
instance tune InContainer

:: NoUserInterface  = NoUserInterface   //Replace the UI by an empty UI
instance tune NoUserInterface

actionToButton :: Layout

//Setting attributes 
instance tune Title
instance tune Label
instance tune Icon
instance tune Attribute

