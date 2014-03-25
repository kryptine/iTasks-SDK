definition module iTasks.API.Core.Client.Interface

import StdString, StdGeneric, Data.Void, Data.Maybe

:: DomElementId	:== String

/**
* This module provides access to the javascript world of webbrowsers
* where all the client side objects of which the iTask world live.
*/

:: JSWorld
:: JSVal a		//Pointer to a javascript value
:: JSObj a :== JSVal a
:: JSArg

:: JSFunction a	//A javascript function object
:: JSWindow		//Represents the global window object
:: JSDocument	//Represents the global window.document object
:: JSEvent		//Represents an event object
:: JSObject a   = JSObject


//CORE JAVASCRIPT ACCESS

//Constants
jsNull				:: JSVal a		  // Can be any type
jsWindow			:: JSObj JSWindow	  // Singleton 'window' object that serves a global scope
jsDocument			:: JSObj JSDocument // Singleton? 'document'

//Manipulating objects
jsEmptyObject		:: 									!*JSWorld -> *(!JSObj a, !*JSWorld) // {}
jsNewObject			:: !String ![JSArg]					!*JSWorld -> *(!JSObj b, !*JSWorld)
jsGetObjectAttr 	:: !String !(JSObj o)				!*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectEl		:: !Int !(JSObj o) 					!*JSWorld -> *(!JSVal b, !*JSWorld)
jsSetObjectAttr		:: !String !(JSVal v) !(JSObj o) 	!*JSWorld -> *JSWorld
jsSetObjectEl		:: !Int !(JSVal v) !(JSObj o) 		!*JSWorld -> *JSWorld
jsDeleteObjectAttr	:: !String !(JSObj o) 				!*JSWorld -> *JSWorld
(.#) infixl 3       :: a b -> (a, b)
.?                  :: !(!JSObj o, !String) !*JSWorld -> !*(!JSVal r, !*JSWorld)
(.=) infixl 2       :: !(!JSObj o, !String) !(JSVal v) -> !*(!*JSWorld -> !*JSWorld)

//Calling js functions
jsApply				:: !(JSVal (JSObject (JSFunction f))) !(JSObj scope) ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)

//Wrapping clean functions
jsWrapFun           :: !([JSArg] *JSWorld -> *(!JSVal a, !*JSWorld)) !*JSWorld -> *(!JSVal (JSFunction f), !*JSWorld)

//Special keywords
jsThis				:: !*JSWorld -> *(!JSObj a, !*JSWorld)
jsTypeof			:: !(JSVal a) -> String
jsAbort             :: a -> b

toJSVal 			:: !a -> JSVal b
toJSArg 			:: !a -> JSArg
toJSArgs 			:: ![a] -> [JSArg]
fromJSValUnsafe		:: !(JSVal a) -> Dynamic
fromJSVal 			:: !(JSVal a) !*JSWorld -> *(!Dynamic, !*JSWorld)

newJSArray          :: !*JSWorld                          -> *(!JSObj [a], !*JSWorld)

//USEFUL DERIVED UTIL FUNCTIONS

jsArrayPush         :: !(JSVal a) !(JSObj [a])      !*JSWorld -> *(!JSObj [a], !*JSWorld)
jsArrayPop          :: !(JSObj [a])                 !*JSWorld -> *(!JSVal a,   !*JSWorld)
jsArrayReverse      :: !(JSObj [a])                 !*JSWorld -> *(!JSObj [a], !*JSWorld)
toJSArray           :: ![a]                         !*JSWorld -> *(!JSObj [a], !*JSWorld)
fromJSArray         :: (JSObj [a]) ((JSVal b) -> c) !*JSWorld -> *([c], !*JSWorld)

jsIsUndefined :: !(JSVal a) -> Bool

getDomElement		:: !DomElementId					!*JSWorld -> *(!JSVal a, !*JSWorld)
getDomAttr			:: !DomElementId !String			!*JSWorld -> *(!JSVal a, !*JSWorld)
setDomAttr			:: !DomElementId !String !(JSVal a)	!*JSWorld -> *JSWorld

//Call a method on a javascript object. Object can be (JSVal null)
callObjectMethod	:: !String ![JSArg] !(JSObj o) !*JSWorld -> *(!JSVal c, !*JSWorld)

//Get a value from the global scope.
//The argument may be in dotted notation (e.g. google.maps.MayTypeId.ROADMAP) for deep searching
findObject			:: !String !*JSWorld -> *(!JSObj a, !*JSWorld)

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
