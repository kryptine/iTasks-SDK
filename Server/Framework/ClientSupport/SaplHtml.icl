implementation module SaplHtml

import StdEnv, Void

:: HtmlDocument :== Void
:: HtmlObject :== Void

handleJSEvent :: (HtmlEventHandlerFunc a) !TaskSID *HtmlObject -> Void
handleJSEvent origHandler taskId event = undef

getDomElement :: !*HtmlDocument !HtmlId -> *(!*HtmlDocument, !HtmlObject)
getDomElement document id = undef

getObjectAttr :: !*HtmlDocument !HtmlObject !String -> *(!*HtmlDocument, !HtmlObject, !String)
getObjectAttr d object attr = undef

getObjectAttrObject :: !*HtmlDocument !HtmlObject !String -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
getObjectAttrObject d object attr = undef

setObjectAttr :: !*HtmlDocument !HtmlObject !String !String -> *(!*HtmlDocument, !HtmlObject, !String)
setObjectAttr d object attr value = undef

setObjectAttrObject :: !*HtmlDocument !HtmlObject !String !HtmlObject -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
setObjectAttrObject d object attr value = undef

runObjectMethod :: !*HtmlDocument !HtmlObject !String [JSFuncArg] -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod d object method args = undef

getDomAttr :: !*HtmlDocument !HtmlId !HtmlObjAttr -> *(!*HtmlDocument, !String)
getDomAttr document id attr = undef

setDomAttr :: !*HtmlDocument !HtmlId !HtmlObjAttr !a -> *(!*HtmlDocument, !a)
setDomAttr document id attr value = undef

findObject :: !*HtmlDocument !String -> *(!*HtmlDocument, !HtmlObject)
findObject document objname = undef 

createObject :: !*HtmlDocument !String [JSFuncArg] -> *(!*HtmlDocument, !HtmlObject)
createObject document objname args = undef 

loadExternalJS :: !*HtmlDocument !String (*HtmlObject -> Void) -> *HtmlDocument
loadExternalJS document url continuation = undef

isUndefined :: !*HtmlDocument !HtmlObject -> *(!*HtmlDocument, Bool)
isUndefined document object = undef


