definition module iTasks.UI.Layout.Common
/**
* This module provides common alternative layout annotations
* that you can apply at strategic points in your task specifications
* to optimize the user experience for specific tasks
*/
import iTasks.UI.Layout

/**
* Create a tabset with all child items as separate tabs
*/
arrangeWithTabs :: Layout

/**
* Extract one child item and put it in a separate panel at the side of the screen
*/
arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout

/**
* Divide the available screen space
*/
arrangeSplit :: !UIDirection !Bool -> Layout

//Changing container types

toWindow :: UIWindowType UIVAlign UIHAlign -> Layout
toEmpty  ::                                   Layout

//Convenient annotatation types
:: ArrangeWithTabs = ArrangeWithTabs
instance tune ArrangeWithTabs

:: ArrangeVertical = ArrangeVertical
instance tune ArrangeVertical

:: ArrangeHorizontal = ArrangeHorizontal
instance tune ArrangeHorizontal

:: ArrangeWithSideBar = ArrangeWithSideBar !Int !UISide !Int !Bool
instance tune ArrangeWithSideBar

:: ArrangeSplit = ArrangeSplit !UIDirection !Bool
instance tune ArrangeSplit

:: ToWindow = ToWindow UIWindowType UIVAlign UIHAlign
InWindow                :== InFloatingWindow
InFloatingWindow        :== ToWindow FloatingWindow AlignMiddle AlignCenter
InNotificationBubble    :== ToWindow NotificationBubble AlignTop AlignRight
InModalDialog           :== ToWindow ModalDialog AlignMiddle AlignCenter
instance tune ToWindow

:: NoUserInterface  = NoUserInterface   //Replace the UI by an empty UI
instance tune NoUserInterface
