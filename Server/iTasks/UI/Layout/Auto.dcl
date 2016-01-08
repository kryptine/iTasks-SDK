definition module iTasks.UI.Layout.Auto
/**
* This module defines the functions that drive iTask's automagic UI layouting
*/

import iTasks.UI.Layout

autoLayoutInteract 		:: Layout //Automatically applied by 'interact'
autoLayoutStep  		:: Layout //Automatically applied by 'step'
autoLayoutParallel 		:: Layout //Automatically applied by 'parallel'
autoLayoutAttach 		:: Layout //Automatically applied by 'attach'

autoLayoutSession 		:: Layout //Added when a task instance is 'published' (can be easily removed or replaced by publishing a task explicitly)

//Partial layouts used in the automatic layouts 
editorToForm            :: Layout 

finalizeUI 				:: Layout
finalizeForm 			:: Layout
finalizeInteract 		:: Layout
finalizeStep 			:: Layout
finalizeParallel 		:: Layout
