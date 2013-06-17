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


getDomElement :: !HtmlElementId !*HtmlWindow -> *(!a, !*HtmlWindow)

getObjectAttr :: !a !HtmlObjAttr !*HtmlWindow -> *(!a, !b, !*HtmlWindow)

setObjectAttr :: !a !HtmlObjAttr !b !*HtmlWindow -> *(!a, !b, !*HtmlWindow)

runObjectMethod :: !a !String ![b] !*HtmlWindow -> *(!a, !c, !*HtmlWindow)

getDomAttr :: !HtmlElementId !HtmlObjAttr !*HtmlWindow -> *(!String, !*HtmlWindow)

setDomAttr :: !HtmlElementId !HtmlObjAttr !a !*HtmlWindow -> *(!a, !*HtmlWindow)

/*
* Find a browser object or constant like:
* - window, document
* - google.maps.MapTypeId.ROADMAP
*/
findObject :: !String !*HtmlWindow -> *(!a, !*HtmlWindow)

/*
* Create a JS object, like:
* - new google.maps.LatLng(-34.397, 150.644)
*/
createObject :: !String ![a] !*HtmlWindow -> *(!b, !*HtmlWindow)

/*
* Load external JS by its URL. A continuation must be given,
* which is called when script is actually loaded
*/
loadExternalJS :: !String !a !*HtmlWindow -> *HtmlWindow

isUndefined :: !HtmlObject !*HtmlWindow -> (!Bool, !*HtmlWindow)

// calls SAPL.toJS
toHtmlObject :: !a !*HtmlWindow -> (!HtmlObject, !*HtmlWindow)

// does nothing, use it carefully!
fromHtmlObject :: !HtmlObject !*HtmlWindow -> (!a, !*HtmlWindow) 
