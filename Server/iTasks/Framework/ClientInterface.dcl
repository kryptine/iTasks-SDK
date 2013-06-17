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
:: HtmlEventHandlerFunc st :== (st ComponentId HtmlObject *HtmlDocument -> *(!*HtmlDocument, st))


// class JS a
// class JSObject a | JS a

getDomElement :: !HtmlElementId !*HtmlWindow -> *(!a, !*HtmlWindow) // | JSObject a

getObjectAttr :: !a !HtmlObjAttr !*HtmlWindow -> *(!a, !b, !*HtmlWindow) // | JSObject a & JS b

setObjectAttr :: !a !HtmlObjAttr !b !*HtmlWindow -> *(!a, !b, !*HtmlWindow) // | JSObject a & JS b

runObjectMethod :: !a !String ![b] !*HtmlWindow -> *(!a, !c, !*HtmlWindow) // | JSObject a & JS b & JS c

getDomAttr :: !HtmlElementId !HtmlObjAttr !*HtmlWindow -> *(!String, !*HtmlWindow)

setDomAttr :: !HtmlElementId !HtmlObjAttr !a !*HtmlWindow -> *(!a, !*HtmlWindow) // | JS a

/*
* Find a browser object or constant like:
* - window, document
* - google.maps.MapTypeId.ROADMAP
*/
findObject :: !String !*HtmlWindow -> *(!a, !*HtmlWindow) // | JSObject a

/*
* Create a JS object, like:
* - new google.maps.LatLng(-34.397, 150.644)
*/
createObject :: !String ![a] !*HtmlWindow -> *(!b, !*HtmlWindow) // | JS a & JSObject b

/*
* Load external JS by its URL. A continuation must be given,
* which is called when script is actually loaded
*/
loadExternalJS :: !String !a !*HtmlWindow -> *HtmlWindow // | JS a

isUndefined :: !a !*HtmlWindow -> (!Bool, !*HtmlWindow) // | JS a

// calls SAPL.toJS
toHtmlObject :: !a !*HtmlWindow -> (!b, !*HtmlWindow) // | JS b

// does nothing, use it carefully!
fromHtmlObject :: !b !*HtmlWindow -> (!a, !*HtmlWindow) // | JS b
