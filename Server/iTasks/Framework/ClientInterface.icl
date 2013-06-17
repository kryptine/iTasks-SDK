implementation module iTasks.Framework.ClientInterface

import StdEnv, Data.Void, Tasklet

//:: EventQueue :== Void
:: HtmlDocument = HtmlDocument
:: HtmlWindow = HtmlWindow
:: HtmlObject = HtmlObject

// TODO: Add a JS constraint on universally quantified types.

getDomElement :: !HtmlElementId !*HtmlWindow -> *(!HtmlObject, !*HtmlWindow)
getDomElement id win = undef

getObjectAttr :: !HtmlObject !HtmlObjAttr !*HtmlWindow -> *(!HtmlObject, !HtmlObject, !*HtmlWindow)
getObjectAttr object attr win = undef

setObjectAttr :: !HtmlObject !HtmlObjAttr !a !*HtmlWindow -> *(!HtmlObject, !HtmlObject, !*HtmlWindow)
setObjectAttr object attr value win = undef

runObjectMethod :: !HtmlObject !String ![HtmlObject] !*HtmlWindow -> *(!HtmlObject, !HtmlObject, !*HtmlWindow)
runObjectMethod object method args win = undef

getDomAttr :: !HtmlElementId !HtmlObjAttr !*HtmlWindow -> *(!String, !*HtmlWindow)
getDomAttr id attr win = undef

setDomAttr :: !HtmlElementId !HtmlObjAttr !a !*HtmlWindow -> *(!a, !*HtmlWindow)
setDomAttr id attr value win = undef

findObject :: !String !*HtmlWindow -> *(!HtmlObject, !*HtmlWindow)
findObject objname win = undef 

createObject :: !String ![HtmlObject] !*HtmlWindow -> *(!HtmlObject, !*HtmlWindow)
createObject objname args win = undef 

loadExternalJS :: !String !HtmlObject !*HtmlWindow -> *HtmlWindow
loadExternalJS url continuation win = undef

isUndefined :: !HtmlObject !*HtmlWindow -> (!Bool, !*HtmlWindow)
isUndefined object win = undef

toHtmlObject :: !a !*HtmlWindow -> (!HtmlObject, !*HtmlWindow)
toHtmlObject a win = undef

fromHtmlObject :: !HtmlObject !*HtmlWindow -> (a, !*HtmlWindow) 
fromHtmlObject obj win = undef

