definition module SaplHtml

import StdString, Void

:: HtmlDocument
:: HtmlObject
:: TaskSID :== String	// String TaskId

:: HtmlEvent st = HtmlEvent !String !String (HtmlEventHandlerFunc st)
:: HtmlEventHandlerFunc st :== (st TaskSID HtmlObject *HtmlDocument -> *(!*HtmlDocument, st))

:: HtmlId :== String
:: HtmlObjAttr :== String

/**
* Wrapper for JS call back functions
*/
handleJSEvent :: (HtmlEventHandlerFunc a) !TaskSID *HtmlObject -> Void

getObjectAttr       :: !*HtmlDocument !HtmlObject !String         -> *(!*HtmlDocument, !HtmlObject, !String)
getObjectAttrObject :: !*HtmlDocument !HtmlObject !String         -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
setObjectAttr       :: !*HtmlDocument !HtmlObject !String !String -> *(!*HtmlDocument, !HtmlObject, !String)

:: JSFuncArg = E.a: JSFuncArg a

runObjectMethod :: !*HtmlDocument !HtmlObject !String [JSFuncArg] -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)

getDomElement :: !*HtmlDocument !HtmlId                 -> *(!*HtmlDocument, !HtmlObject)
getDomAttr    :: !*HtmlDocument !HtmlId !HtmlObjAttr    -> *(!*HtmlDocument, !String)
setDomAttr    :: !*HtmlDocument !HtmlId !HtmlObjAttr !a -> *(!*HtmlDocument, !a)

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

