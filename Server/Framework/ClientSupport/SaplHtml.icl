implementation module SaplHtml

import StdEnv, Void

:: HtmlDocument :== Void
:: HtmlObject :== Void

handleJSEvent :: (HtmlEventHandlerFunc a) !TaskSID *HtmlObject -> Void
handleJSEvent origHandler taskId event = undef

getDomElement :: !*HtmlDocument !HtmlElementId -> *(!*HtmlDocument, !HtmlObject)
getDomElement document id = undef

getObjectAttr :: !*HtmlDocument !HtmlObject !HtmlObjAttr -> *(!*HtmlDocument, !HtmlObject, !String)
getObjectAttr d object attr = undef

getObjectAttrObject :: !*HtmlDocument !HtmlObject !HtmlObjAttr -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
getObjectAttrObject d object attr = undef

setObjectAttr :: !*HtmlDocument !HtmlObject !HtmlObjAttr !String -> *(!*HtmlDocument, !HtmlObject, !String)
setObjectAttr d object attr value = undef

setObjectAttrObject :: !*HtmlDocument !HtmlObject !HtmlObjAttr !HtmlObject -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
setObjectAttrObject d object attr value = undef

runObjectMethod :: !*HtmlDocument !HtmlObject !String [JSFuncArg] -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod d object method args = undef

getDomAttr :: !*HtmlDocument !HtmlElementId !HtmlObjAttr -> *(!*HtmlDocument, !String)
getDomAttr document id attr = undef

setDomAttr :: !*HtmlDocument !HtmlElementId !HtmlObjAttr !a -> *(!*HtmlDocument, !a)
setDomAttr document id attr value = undef

findObject :: !*HtmlDocument !String -> *(!*HtmlDocument, !HtmlObject)
findObject document objname = undef 

createObject :: !*HtmlDocument !String [JSFuncArg] -> *(!*HtmlDocument, !HtmlObject)
createObject document objname args = undef 

loadExternalJS :: !*HtmlDocument !String (*HtmlObject -> Void) -> *HtmlDocument
loadExternalJS document url continuation = undef

isUndefined :: !*HtmlDocument !HtmlObject -> *(!*HtmlDocument, Bool)
isUndefined document object = undef


