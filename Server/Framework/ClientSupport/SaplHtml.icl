implementation module SaplHtml

import StdEnv, Void

:: HtmlDocument :== Int
:: HtmlObject :== Int

handleJSEvent :: (HtmlEventHandlerFunc a) !String *HtmlObject -> Void
handleJSEvent origHandler taskId eventJS = undef

getDomElement :: !*HtmlDocument !HtmlId -> *(!*HtmlDocument, !HtmlObject)
getDomElement document id = undef

getObjectAttr :: !*HtmlDocument !HtmlObject !String -> *(!*HtmlDocument, !HtmlObject, !String)
getObjectAttr d object attr = undef

getObjectAttrObject :: !*HtmlDocument !HtmlObject !String -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
getObjectAttrObject d object attr = undef

setObjectAttr :: !*HtmlDocument !HtmlObject !String !String -> *(!*HtmlDocument, !HtmlObject, !String)
setObjectAttr d object attr value = undef

runObjectMethod0 :: !*HtmlDocument !HtmlObject !String -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod0 d object method = undef

runObjectMethod1 :: !*HtmlDocument !HtmlObject !String a -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod1 d object method p = undef

runObjectMethod2 :: !*HtmlDocument !HtmlObject !String a b -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod2 d object method p1 p2 = undef

runObjectMethod3 :: !*HtmlDocument !HtmlObject !String a b c -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod3 d object method p1 p2 p3 = undef

runObjectMethod4 :: !*HtmlDocument !HtmlObject !String a b c d -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod4 d object method p1 p2 p3 p4 = undef

runObjectMethod6 :: !*HtmlDocument !HtmlObject !String a b c d e f -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod6 d object method p1 p2 p3 p4 p5 p6 = undef

getDomAttr :: !*HtmlDocument !HtmlId !HtmlAttr2 -> *(!*HtmlDocument, !String)
getDomAttr document id attr = undef

setDomAttr :: !*HtmlDocument !HtmlId !HtmlAttr2 !a -> *(!*HtmlDocument, !a)
setDomAttr document id attr value = undef

findObject :: !*HtmlDocument !String -> *(!*HtmlDocument, !HtmlObject)
findObject document objname = undef 

createWorld :: *World
createWorld = undef	
	
getTime :: !*World -> *(!Int, !*World)
getTime world = undef


