definition module SaplHtml

import StdString, Void, Tasklet

:: EventQueue
:: HtmlDocument
:: HtmlObject

:: HtmlElementId :== String
:: HtmlObjAttr :== String
:: HtmlEventName :== String

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
handleJSEvent :: (HtmlEventHandlerFunc a) !TaskInstanceId *HtmlObject -> Void

// creates 1. layer
createEventHandler :: (HtmlEventHandlerFunc a) !TaskInstanceId -> HtmlObject 

/*
* Fire own event
*/
fireEvent :: !*EventQueue !TaskInstanceId !String a -> *EventQueue

// Handle interface call on the client. Similar to handleJSEvent
handleInterfaceCall :: !f !TaskInstanceId !arg -> Void 

getObjectAttr       :: !*HtmlDocument !HtmlObject !HtmlObjAttr    -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
setObjectAttr       :: !*HtmlDocument !HtmlObject !HtmlObjAttr !a -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)

// calls SAPL.toJS
toHtmlObject :: !a -> HtmlObject
// does nothing, use it carefully!
fromHtmlObject :: HtmlObject -> a

runObjectMethod :: !*HtmlDocument !HtmlObject !String ![HtmlObject] -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)

getDomElement :: !*HtmlDocument !HtmlElementId                 -> *(!*HtmlDocument, !HtmlObject)
getDomAttr    :: !*HtmlDocument !HtmlElementId !HtmlObjAttr    -> *(!*HtmlDocument, !String)
setDomAttr    :: !*HtmlDocument !HtmlElementId !HtmlObjAttr !a -> *(!*HtmlDocument, !a)

isUndefined :: !HtmlObject -> Bool

/*
* Find a browser object or constant like:
* - window, document
* - google.maps.MapTypeId.ROADMAP
*/
findObject :: !*HtmlDocument !String -> *(!*HtmlDocument, !HtmlObject)

/*
* Create a JS object, like:
* - new google.maps.LatLng(-34.397, 150.644)
*/
createObject :: !*HtmlDocument !String ![HtmlObject] -> *(!*HtmlDocument, !HtmlObject)

/*
* Load external JS by its URL. A continuation must be given,
* which is called when script is actually loaded
*/
loadExternalJS :: !*HtmlDocument !String !HtmlObject -> *HtmlDocument

