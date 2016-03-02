definition module iTasks.UI.Layout.Default
/**
* This module defines the functions that drive iTask's automagic default UI layouting
*/

import iTasks.UI.Layout

defaultSessionLayout    :: Layout //Added when a task instance is 'published' (can be easily removed or replaced by publishing a task explicitly)

//Partial layouts used in the automatic layouts 
editorToForm            :: Layout 

finalizeUI 				:: Layout
finalizeForm 			:: Layout
finalizeInteract 		:: Layout
finalizeStep 			:: Layout
finalizeParallel 		:: Layout
