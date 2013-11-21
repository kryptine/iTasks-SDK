definition module iTasks.Framework.Client.LinkerSupport

import StdString
import Data.Maybe, Data.Void
import iTasks.Framework.IWorld
import iTasks.API.Core.Client.Editlet

//taskletUpdateLinker :: !val !*IWorld -> *(!String, !*IWorld)

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

editletLinker :: 
	![(!String, !String, EditletEventHandlerFunc a)]	// event handlers
	!idf												// initDiff function
	!prf												// previousDiff value
	!dvf												// defVal function
	!uui												// updateUI function
	!gdf												// gendiff function
	!adf												// adddiff function
	!*IWorld
	->
	*(!String									// JS code of the support code for all the expressions
	 ,![(!String,!String,!String)]				// JS code of the eventhandlers
	 ,!String									// JS code of the initDiff function
	 ,!String									// JS code of the prevDiff value
	 ,!String									// JS code of the defVal function
	 ,!String									// JS code of the updateUI function
	 ,!String									// JS code of the gendiff function
	 ,!String									// JS code of the adddiff function
	 ,!*IWorld)
	

