definition module iTasks.API.Core.Client.Interface

import StdString, StdGeneric, Data.Void, Data.Maybe

:: DomElementId	:== String

/**
* This module provides access to the javascript world of webbrowsers
* where all the client side objects of which the iTask world live.
*/

:: JSWorld
:: JSVal a		//Pointer to a javascript object
:: JSArg

:: JSFunction a	//A javascript function object
:: JSWindow		//Represents the global window object
:: JSDocument	//Represents the global window.document object
:: JSEvent		//Represents an event object
:: JSObject		//Just for fun

//CORE JAVASCRIPT ACCESS

//Constants
jsNull				:: (JSVal a)		  // Can be any type
jsWindow			:: (JSVal JSWindow)	  // Singleton 'window' object that serves a global scope
jsDocument			:: (JSVal JSDocument) // Singleton? 'document'

//Manipulating objects
jsEmptyObject		:: 									!*JSWorld -> *(!JSVal a, !*JSWorld) // {}
jsNewObject			:: !String ![JSArg]					!*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectAttr 	:: !String !(JSVal o)				!*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectEl		:: !Int !(JSVal o) 					!*JSWorld -> *(!JSVal b, !*JSWorld)
jsSetObjectAttr		:: !String !(JSVal v) !(JSVal o) 	!*JSWorld -> *JSWorld
jsSetObjectEl		:: !Int !(JSVal v) !(JSVal o) 		!*JSWorld -> *JSWorld
jsDeleteObjectAttr	:: !String !(JSVal o) 				!*JSWorld -> *JSWorld

//Calling js functions
jsApply				:: !(JSVal (JSFunction f)) !(JSVal scope) ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)

//Special keywords
jsThis				:: !*JSWorld -> *(!JSVal a, !*JSWorld)
jsTypeof			:: !(JSVal a) -> String
jsAbort             :: a -> b

toJSVal 			:: !a -> JSVal b
toJSArg 			:: !a -> JSArg
toJSArgs 			:: ![a] -> [JSArg]
fromJSValUnsafe		:: !(JSVal a) -> Dynamic
fromJSVal 			:: !(JSVal a) !*JSWorld -> *(!Dynamic, !*JSWorld)

newJSArray          :: !*JSWorld                          -> *(!JSVal [a], !*JSWorld)

//USEFUL DERIVED UTIL FUNCTIONS

jsArrayPush         :: !(JSVal a) !(JSVal [a])    !*JSWorld -> *(!JSVal [a], !*JSWorld)
jsArrayPop          :: !(JSVal [a])               !*JSWorld -> *(!JSVal a,   !*JSWorld)
jsArrayReverse      :: !(JSVal [a])               !*JSWorld -> *(!JSVal [a], !*JSWorld)
toJSArray           :: ![a]                       !*JSWorld -> *(!JSVal [a], !*JSWorld)
fromJSArray         :: (JSVal a) ((JSVal b) -> c) !*JSWorld -> *([c], !*JSWorld)

jsIsUndefined :: !(JSVal a) -> Bool

getDomElement		:: !DomElementId					!*JSWorld -> *(!JSVal a, !*JSWorld)
getDomAttr			:: !DomElementId !String			!*JSWorld -> *(!JSVal a, !*JSWorld)
setDomAttr			:: !DomElementId !String !(JSVal a)	!*JSWorld -> *JSWorld

//Call a method on a javascript object. Object can be (JSVal null)
callObjectMethod	:: !String ![JSArg] !(JSVal o) !*JSWorld -> *(!JSVal c, !*JSWorld)

//Get a value from the global scope.
//The argument may be in dotted notation (e.g. google.maps.MayTypeId.ROADMAP) for deep searching
findObject			:: !String !*JSWorld -> *(!JSVal a, !*JSWorld)

//Load external JS by its URL. A continuation can be given,
//which is called when script is actually loaded
addJSFromUrl		:: !String !(Maybe (JSVal (JSFunction f))) !*JSWorld -> *JSWorld
//Loaf external CSS stylesheet by its URL
addCSSFromUrl       :: !String !*JSWorld -> *JSWorld

jsTrace :: a *JSWorld -> *JSWorld

jsValToString :: !(JSVal a) -> String
jsValToReal   :: !(JSVal a) -> Real
jsValToInt    :: !(JSVal a) -> Int
jsValToBool   :: !(JSVal a) -> Bool

withDef     :: !((JSVal a) -> b) !b !(JSVal a) -> b

callFunction :: String [JSArg] *JSWorld -> *(JSVal a, *JSWorld)

jsUnsafeCoerce :: (JSVal a) -> (JSVal b)
