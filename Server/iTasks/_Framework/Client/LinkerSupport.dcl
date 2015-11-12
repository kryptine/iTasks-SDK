definition module iTasks._Framework.Client.LinkerSupport

import StdString
import Data.Maybe, Data.Void
import iTasks._Framework.IWorld
import iTasks.UI.Editor

taskletLinker :: !st 							// state
	![(!String, !iarg -> Void)] 				// interface functions
	![(!String, !String, *JSWorld -> Void)]		// event handlers
	!rs											// result function
	!(Maybe cf)									// controller function
	!*IWorld
	->
	*(!String									// JS code of the state
	 ,!String 									// JS code of the support code for all the expressions
	 ,![(!String,!String,!String)]				// JS code of the eventhandlers
	 ,![(!String,!String)]						// JS code of the interface functions
	 ,!String									// JS code of the result function
	 ,!Maybe String								// JS code of the controller function
	 ,!*IWorld)


/**
* Links all necessary Sapl functions for an editlet and compiles them to Javascript 
*
* @param initDiff function
* @param defVal function
* @param adddiff function
* @param IWorld state
*
* @return JS code of the support code for all the expressions
* @return JS code of the support code for all the expressions
* @return JS code of the initDiff function
* @return JS code of the defVal function
* @return JS code of the adddiff function
* @return IWorld state
*/
editletLinker :: !id !icf !adf !*IWorld -> *(!MaybeErrorString (!String,!String,!String,!String),!*IWorld)

/**
* Links additional Sapl functions when an editlet computes diffs
*/
diffLinker :: !cdf !idf !*IWorld -> (!MaybeErrorString (!String,!String,!String),!*IWorld)
