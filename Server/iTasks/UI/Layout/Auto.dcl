definition module iTasks.UI.Layout.Auto
/**
* This module defines the functions that drive iTask's automagic UI layouting
*/

import iTasks.UI.Layout

autoLayoutInteract 		:: Layout JSONNode //Automatically applied by 'interact'
autoLayoutStep  		:: Layout JSONNode //Automatically applied by 'step'
autoLayoutParallel 		:: Layout JSONNode //Automatically applied by 'parallel'
autoLayoutAttach 		:: Layout JSONNode //Automatically applied by 'attach'

autoLayoutSession 		:: Layout JSONNode //Added when a task instance is 'published' (can be easily removed or replaced by publishing a task explicitly)

//Partial layouts used in the automatic layouts 
editorToForm            :: Layout JSONNode
formToBlock             :: Layout JSONNode
