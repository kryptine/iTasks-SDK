definition module iTasks.Framework.ClientInterface

import StdString, Data.Void

// :: EventQueue
:: HtmlDocument
:: HtmlWindow
:: HtmlObject

:: HtmlElementId :== String
:: HtmlObjAttr :== String
:: HtmlEventName :== String
:: ComponentId :== String

:: HtmlEvent st = HtmlEvent !HtmlElementId !HtmlEventName (HtmlEventHandlerFunc st)
:: HtmlEventHandlerFunc st :== (st TaskInstanceId HtmlObject *HtmlDocument -> *(!*HtmlDocument, st))

/**
* TODO:
* - createFunction :: !String ![String] -> HtmlObject
* - createEventhandler :: !(HtmlEventHandlerFunc st) !TaskSID -> HtmlObject
* - Functions are wrapped into {funcValue: ...} on creation, and unwrapped on passing as argument
*/

/**
* Three layers of event handler wrapping:
* 3. Clean event handler function of type "HtmlEventHandlerFunc a"
* 2. SaplHtml.handleJSEvent: creates the context for 3., still Clean function
* 1. JavaScript event handler function created by createEventHandler
*/

// 2. layer
// handleJSEvent :: (HtmlEventHandlerFunc a) !TaskInstanceId *HtmlObject -> Void

// creates 1. layer
// createEventHandler :: (HtmlEventHandlerFunc a) !TaskInstanceId -> HtmlObject 

/*
* Fire own event
*/
// fireEvent :: !*EventQueue !TaskInstanceId !String a -> *EventQueue

// Handle interface call on the client. Similar to handleJSEvent
// handleInterfaceCall :: !f !TaskInstanceId !arg -> Void 

getDomElement :: !HtmlElementId !*HtmlWindow -> *(!HtmlObject, !*HtmlWindow)

getObjectAttr :: !HtmlObject !HtmlObjAttr !*HtmlWindow -> *(!HtmlObject, !HtmlObject, !*HtmlWindow)

setObjectAttr :: !HtmlObject !HtmlObjAttr !a !*HtmlWindow -> *(!HtmlObject, !HtmlObject, !*HtmlWindow)

runObjectMethod :: !HtmlObject !String ![HtmlObject] !*HtmlWindow -> *(!HtmlObject, !HtmlObject, !*HtmlWindow)

getDomAttr :: !HtmlElementId !HtmlObjAttr !*HtmlWindow -> *(!String, !*HtmlWindow)

setDomAttr :: !HtmlElementId !HtmlObjAttr !a !*HtmlWindow -> *(!a, !*HtmlWindow)

/*
* Find a browser object or constant like:
* - window, document
* - google.maps.MapTypeId.ROADMAP
*/
findObject :: !String !*HtmlWindow -> *(!HtmlObject, !*HtmlWindow)

/*
* Create a JS object, like:
* - new google.maps.LatLng(-34.397, 150.644)
*/
createObject :: !String ![HtmlObject] !*HtmlWindow -> *(!HtmlObject, !*HtmlWindow)

/*
* Load external JS by its URL. A continuation must be given,
* which is called when script is actually loaded
*/
loadExternalJS :: !String !HtmlObject !*HtmlWindow -> *HtmlWindow

isUndefined :: !HtmlObject !*HtmlWindow -> (!Bool, !*HtmlWindow)

// calls SAPL.toJS
toHtmlObject :: !a !*HtmlWindow -> (!HtmlObject, !*HtmlWindow)

// does nothing, use it carefully!
fromHtmlObject :: !HtmlObject !*HtmlWindow -> (a, !*HtmlWindow) 
