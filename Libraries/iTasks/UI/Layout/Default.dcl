definition module iTasks.UI.Layout.Default
/**
* This module defines the functions that drive iTask's automagic default UI layouting
*/

import iTasks.UI.Layout

defaultSessionLayout    :: LayoutRule //Added when a task instance is 'published' (can be easily removed or replaced by publishing a task explicitly)

finalizeUI 				:: LayoutRule
finalizeInteract 		:: LayoutRule
finalizeStep 			:: LayoutRule
finalizeParallel 		:: LayoutRule
