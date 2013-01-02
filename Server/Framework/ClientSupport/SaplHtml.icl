implementation module SaplHtml

import StdEnv, Void, Tasklet

:: HtmlDocument :== Void
:: HtmlObject :== Void

handleJSEvent :: (HtmlEventHandlerFunc a) !TaskInstanceId *HtmlObject -> Void
handleJSEvent origHandler iid event = undef

createEventHandler :: (HtmlEventHandlerFunc a) !TaskInstanceId -> HtmlObject 
createEventHandler origHandler iid = undef

fireEvent :: !*HtmlDocument !TaskInstanceId !String a -> *HtmlDocument
fireEvent document iid eventName eventValue = undef

handleInterfaceCall :: !f !TaskInstanceId !arg -> Void 
handleInterfaceCall origHandler iid arg = undef

getDomElement :: !*HtmlDocument !HtmlElementId -> *(!*HtmlDocument, !HtmlObject)
getDomElement document id = undef

getObjectAttr :: !*HtmlDocument !HtmlObject !HtmlObjAttr -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
getObjectAttr d object attr = undef

setObjectAttr :: !*HtmlDocument !HtmlObject !HtmlObjAttr !a -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
setObjectAttr d object attr value = undef

runObjectMethod :: !*HtmlDocument !HtmlObject !String ![HtmlObject] -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod d object method args = undef

getDomAttr :: !*HtmlDocument !HtmlElementId !HtmlObjAttr -> *(!*HtmlDocument, !String)
getDomAttr document id attr = undef

setDomAttr :: !*HtmlDocument !HtmlElementId !HtmlObjAttr !a -> *(!*HtmlDocument, !a)
setDomAttr document id attr value = undef

findObject :: !*HtmlDocument !String -> *(!*HtmlDocument, !HtmlObject)
findObject document objname = undef 

createObject :: !*HtmlDocument !String ![HtmlObject] -> *(!*HtmlDocument, !HtmlObject)
createObject document objname args = undef 

loadExternalJS :: !*HtmlDocument !String !HtmlObject -> *HtmlDocument
loadExternalJS document url continuation = undef

isUndefined :: !HtmlObject -> Bool
isUndefined object = undef

toHtmlObject :: !a -> HtmlObject
toHtmlObject a = undef

fromHtmlObject :: HtmlObject -> a 
fromHtmlObject obj = undef

