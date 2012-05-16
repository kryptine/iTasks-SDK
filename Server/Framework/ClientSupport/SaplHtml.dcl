definition module SaplHtml

import StdString, Void

:: HtmlEvent st = HtmlEvent !String !String (HtmlEventHandlerFunc st)
//TaskId
:: HtmlEventHandlerFunc st :== (st String HtmlObject *HtmlDocument -> *(!*HtmlDocument, !HtmlEventResult st))
:: HtmlEventResult st = KeepState | SaveState st | PersistState st

:: HtmlDocument
:: HtmlObject

:: HtmlId :== String
:: HtmlAttr2 :== String // TODO: 2?

/**
* Wrapper for JS call back functions
*/
handleJSEvent :: (HtmlEventHandlerFunc a) !String *HtmlObject -> Void

getObjectAttr :: !*HtmlDocument !HtmlObject !String -> *(!*HtmlDocument, !HtmlObject, !String)
getObjectAttrObject :: !*HtmlDocument !HtmlObject !String -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
setObjectAttr :: !*HtmlDocument !HtmlObject !String !String -> *(!*HtmlDocument, !HtmlObject, !String)

// TODO: runObjectMethodx -> runObjectMethod
:: JSFuncArg = E.a: JSFuncArg [a]

//runObjectMethod :: !*HtmlDocument !HtmlObject !String (E.a: [a]) -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)

runObjectMethod0 :: !*HtmlDocument !HtmlObject !String -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod1 :: !*HtmlDocument !HtmlObject !String a -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod2 :: !*HtmlDocument !HtmlObject !String a b -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod3 :: !*HtmlDocument !HtmlObject !String a b c -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod4 :: !*HtmlDocument !HtmlObject !String a b c d -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)
runObjectMethod6 :: !*HtmlDocument !HtmlObject !String a b c d e f -> *(!*HtmlDocument, !HtmlObject, !HtmlObject)

getDomElement :: !*HtmlDocument !HtmlId -> *(!*HtmlDocument, !HtmlObject)
getDomAttr    :: !*HtmlDocument !HtmlId !HtmlAttr2 -> *(!*HtmlDocument, !String)
setDomAttr    :: !*HtmlDocument !HtmlId !HtmlAttr2 !a -> *(!*HtmlDocument, !a)

// http://www.w3schools.com/js/js_ex_browser.asp
findObject :: !*HtmlDocument !String -> *(!*HtmlDocument, !HtmlObject)

// TODO: move it out somewhere
createWorld :: *World
getTime :: !*World -> *(!Int, !*World)
