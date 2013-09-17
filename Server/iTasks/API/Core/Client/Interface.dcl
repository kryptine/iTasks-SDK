definition module iTasks.API.Core.Client.Interface

import StdString, Data.Void, Data.Maybe
/**
* This module provides access to the javascript world of webbrowsers
* where all the client side objects of which the iTask world live.
*/

:: JSWorld
:: JSPtr a		//Pointer to a javascript object

:: JSFunction a	//A javascript function object
:: JSWindow		//Represents the global window object
:: JSDocument	//Represents the global window.document object

//CORE JAVASCRIPT ACCESS

//Constants
jsNull				:: (JSPtr a)		// Can be any type
jsWindow			:: (JSPtr JSWindow)	// Singleton 'window' object that serves a global scope

//Manipulating objects
jsEmptyObject		:: 							!*JSWorld -> *(!JSPtr a, !*JSWorld) // {}
jsNewObject			:: !(JSPtr (JSFunction f))	!*JSWorld -> *(!JSPtr a, !*JSWorld) //creates a new object using 'new' keyword
jsGetObjectAttr 	:: !String !(JSPtr a)		!*JSWorld -> *(!b, !*JSWorld)
jsGetObjectEl		:: !Int !(JSPtr a) 			!*JSWorld -> *(!b, !*JSWorld)
jsSetObjectAttr		:: !String !b !(JSPtr a) 	!*JSWorld -> *JSWorld
jsSetObjectEl		:: !Int !b !(JSPtr a) 		!*JSWorld -> *JSWorld

//Calling js functions
jsApply				:: !(JSPtr (JSFunction f)) !(JSPtr a) !(JSPtr b) !*JSWorld -> *(!c, !*JSWorld)

//Special keywords
jsThis				:: 							!*JSWorld -> *(!JSPtr a, !*JSWorld)
jsTypeof			:: !a						!*JSWorld -> *(!String, !*JSWorld)

//Creating js functions from clean functions
jsWrapFun			:: !f !*JSWorld -> *(!JSPtr (JSFunction f), !*JSWorld)

toJSPtr :: !a !*JSWorld -> *(!JSPtr b, !*JSWorld)

//USEFUL DERIVED UTIL FUNCTIONS
jsDocument			::							!*JSWorld -> *(!JSPtr JSDocument, !*JSWorld)

newJSArray :: !*JSWorld -> *(!JSPtr [a], !*JSWorld)

jsArrayPush :: !a (!JSPtr [a]) !*JSWorld -> *(!JSPtr [a], !*JSWorld)

jsArrayReverse :: (!JSPtr [a]) !*JSWorld -> *(!JSPtr [a], !*JSWorld)

toJSArray :: ![a] !*JSWorld -> *(!JSPtr [a], !*JSWorld)

jsIsUndefined		:: !a						!*JSWorld -> *(!Bool, !*JSWorld)

:: DomElementId	:== String

getDomElement		:: !DomElementId			!*JSWorld -> *(!JSPtr a, !*JSWorld)
getDomAttr			:: !DomElementId !String	!*JSWorld -> *(!a, !*JSWorld)
setDomAttr			:: !DomElementId !String !a	!*JSWorld -> *JSWorld

//Get a value from the global scope.
//The argument may be in dotted notation (e.g. google.maps.MayTypeId.ROADMAP) for deep searching
findObject			:: !String !*JSWorld -> *(!JSPtr a, !*JSWorld)

//Call a method on a javascript object
callObjectMethod	:: !String ![b] !(JSPtr a)	!*JSWorld -> *(!c, !*JSWorld)

//Load external JS by its URL. A continuation must be given,
//which is called when script is actually loaded
addJSFromUrl		:: !String !(Maybe (JSPtr (JSFunction f))) *JSWorld -> *JSWorld

