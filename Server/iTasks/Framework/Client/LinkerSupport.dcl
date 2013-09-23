definition module iTasks.Framework.Client.LinkerSupport

import StdString
import Data.Maybe, Data.Void
import iTasks.Framework.Client.SaplHtml
import iTasks.Framework.IWorld
import iTasks.API.Core.Client.Editlet

taskletUpdateLinker :: !val !*IWorld -> *(!String, !*IWorld)

taskletLinker :: !st 							// state
	![(!String, !iarg -> Void)] 				// interface functions
	![(!String, !String, *JSWorld -> Void)]		// event handlers
	!rs											// result function
	!(Maybe cf)									// controller function
	!(Maybe uf)									// update function
	!(Maybe uv)									// update value
	!*IWorld
	->
	*(!String									// JS code of the state
	 ,!String 									// JS code of the support code for all the expressions
	 ,![(!String,!String,!String)]				// JS code of the eventhandlers
	 ,![(!String,!String)]						// JS code of the interface functions
	 ,!String									// JS code of the result function
	 ,!Maybe String								// JS code of the controller function
	 ,!Maybe String								// JS code of the update function
	 ,!*IWorld)

editletLinker :: 
	![(!String, !String, ComponentEventHandlerFunc a)]	// event handlers
	!ivf												// init value function
	!gdf												// gendiff function
	!adf												// adddiff function
	!*IWorld
	->
	*(!String									// JS code of the support code for all the expressions
	 ,![(!String,!String,!String)]				// JS code of the eventhandlers
	 ,!String									// JS code of the init value function
	 ,!String									// JS code of the gendiff function
	 ,!String									// JS code of the adddiff function
	 ,!*IWorld)
	

