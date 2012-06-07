definition module SaplHtml

import StdString, Void

:: HtmlDocument
:: HtmlObject
:: TaskSID :== String	// String TaskId

:: HtmlElementId :== String
:: HtmlObjAttr :== String
:: HtmlEventName :== String

:: HtmlEvent st = HtmlEvent !HtmlElementId !HtmlEventName (HtmlEventHandlerFunc st)
:: HtmlEventHandlerFunc st :== (st TaskSID HtmlObject *HtmlDocument -> *(!*HtmlDocument, st))

/**
* TODO:
* - createFunction :: !String ![String] -> HtmlObject
* - createEventhandler :: !(HtmlEventHandlerFunc st) !TaskSID -> HtmlObject
* - Functions are wrapped into {funcValue: ...} on creation, and unwrapped on passing as argument
*/

/**
* Wrapper for JS call back functions
*/
handleJSEvent :: (HtmlEventHandlerFunc a) !TaskSID *HtmlObject -> Void

getObjectAttr       :: !*HtmlDocument !HtmlObject !HtmlObjAttr             -> *(!*HtmlDocument, !HtmlObject, !String)
getObjectAttrObject :: !*HtmlDocument !HtmlObject !HtmlObjAttr             -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
setObjectAttr       :: !*HtmlDocument !HtmlObject !HtmlObjAttr !String     -> *(!*HtmlDocument, !HtmlObject, !String)
setObjectAttrObject :: !*HtmlDocument !HtmlObject !HtmlObjAttr !HtmlObject -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)

:: JSFuncArg = E.a: JSFuncArg a

runObjectMethod :: !*HtmlDocument !HtmlObject !String [JSFuncArg] -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)

getDomElement :: !*HtmlDocument !HtmlElementId                 -> *(!*HtmlDocument, !HtmlObject)
getDomAttr    :: !*HtmlDocument !HtmlElementId !HtmlObjAttr    -> *(!*HtmlDocument, !String)
setDomAttr    :: !*HtmlDocument !HtmlElementId !HtmlObjAttr !a -> *(!*HtmlDocument, !a)

isUndefined :: !*HtmlDocument !HtmlObject -> *(!*HtmlDocument, Bool)

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
createObject :: !*HtmlDocument !String [JSFuncArg] -> *(!*HtmlDocument, !HtmlObject)

/*
* Load external JS by its URL. A continuation must be given,
* which is called when script is actually loaded
*/
loadExternalJS :: !*HtmlDocument !String (*HtmlObject -> Void) -> *HtmlDocument

