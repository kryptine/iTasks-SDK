implementation module iTasks.Framework.ClientInterface

import StdEnv, Data.Void

//:: EventQueue :== Void
:: HtmlDocument = HtmlDocument
:: HtmlWindow = HtmlWindow
:: HtmlObject = HtmlObject

// TODO: Add a JS constraint on universally quantified types.

getDomElement :: !HtmlElementId !*HtmlWindow -> *(!a, !*HtmlWindow)
getDomElement id win = undef

getObjectAttr :: !a !HtmlObjAttr !*HtmlWindow -> *(!a, !b, !*HtmlWindow)
getObjectAttr object attr win = undef

setObjectAttr :: !a !HtmlObjAttr !b !*HtmlWindow -> *(!a, !b, !*HtmlWindow)
setObjectAttr object attr value win = undef

runObjectMethod :: !a !String ![b] !*HtmlWindow -> *(!a, !c, !*HtmlWindow)
runObjectMethod object method args win = undef

getDomAttr :: !HtmlElementId !HtmlObjAttr !*HtmlWindow -> *(!String, !*HtmlWindow)
getDomAttr id attr win = undef

setDomAttr :: !HtmlElementId !HtmlObjAttr !a !*HtmlWindow -> *(!a, !*HtmlWindow)
setDomAttr id attr value win = undef

findObject :: !String !*HtmlWindow -> *(!a, !*HtmlWindow)
findObject objname win = undef 

createObject :: !String ![a] !*HtmlWindow -> *(!b, !*HtmlWindow)
createObject objname args win = undef 

loadExternalJS :: !String !a !*HtmlWindow -> *HtmlWindow
loadExternalJS url continuation win = undef

isUndefined :: !a !*HtmlWindow -> (!Bool, !*HtmlWindow)
isUndefined object win = undef

toHtmlObject :: !a !*HtmlWindow -> (!HtmlObject, !*HtmlWindow)
toHtmlObject a win = undef

fromHtmlObject :: !HtmlObject !*HtmlWindow -> (!a, !*HtmlWindow) 
fromHtmlObject obj win = undef

