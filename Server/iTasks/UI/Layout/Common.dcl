definition module iTasks.UI.Layout.Common
/**
* This module provides common alternative layout annotations
* that you can apply at strategic points in your task specifications
* to optimize the user experience for specific tasks
*/
import iTasks.UI.Layout

arrangeWithTabs :: Layout

//Changing container types
toWindow :: UIWindowType UIVAlign UIHAlign -> Layout

//Convenient annotatation types
:: ArrangeWithTabs = ArrangeWithTabs
instance tune ArrangeWithTabs

:: ArrangeVertical = ArrangeVertical
instance tune ArrangeVertical

:: ArrangeHorizontal = ArrangeHorizontal
instance tune ArrangeHorizontal

:: ArrangeWithSideBar = ArrangeWithSideBar !Int !UISide !Int !Bool
instance tune ArrangeWithSideBar

:: ToWindow = ToWindow UIWindowType UIVAlign UIHAlign
InWindow                :== InFloatingWindow
InFloatingWindow        :== ToWindow FloatingWindow AlignMiddle AlignCenter
InNotificationBubble    :== ToWindow NotificationBubble AlignTop AlignRight
InModalDialog           :== ToWindow ModalDialog AlignMiddle AlignCenter
instance tune ToWindow

