implementation module iTasks.Framework.ClientInterface

import StdEnv, Data.Void, Data.Maybe, Text

:: JSWorld = JSWorld
:: JSPtr a = JSPtr

:: JSWindow = JSWindow
:: JSDocument = JSDocument
:: JSFunction a = JSFunction

jsNull :: (JSPtr a)
jsNull = undef

jsWindow :: (JSPtr JSWindow)
jsWindow = undef

jsEmptyObject :: !*JSWorld -> (!JSPtr a, !*JSWorld)
jsEmptyObject world = undef

jsNewObject	:: !(JSPtr (JSFunction f)) !*JSWorld -> (!JSPtr a, !*JSWorld)
jsNewObject constructor world = undef

jsGetObjectAttr :: !String !(JSPtr a) !*JSWorld -> (!b,!*JSWorld)
jsGetObjectAttr attr obj world = undef

jsGetObjectEl :: !Int !(JSPtr a) !*JSWorld -> (!b,!*JSWorld)
jsGetObjectEl index obj world = undef

jsSetObjectAttr :: !String !b !(JSPtr a) !*JSWorld -> *JSWorld
jsSetObjectAttr attr value obj world = undef

jsSetObjectEl :: !Int !b !(JSPtr a) !*JSWorld -> *JSWorld
jsSetObjectEl index value obj world = undef

jsApply :: !(JSPtr (JSFunction f)) !(JSPtr a) !(JSPtr b) !*JSWorld -> (!c,!*JSWorld)
jsApply fun scope args world = undef

jsThis :: !*JSWorld -> (!JSPtr a,!*JSWorld)
jsThis world = undef

jsTypeof :: !a !*JSWorld -> (!String,!*JSWorld)
jsTypeof obj world = undef

jsWrapFun :: !f !*JSWorld -> (!JSPtr (JSFunction f), !*JSWorld)
jsWrapFun fun world = undef

toJSPtr :: !a !*JSWorld -> (!JSPtr b, !*JSWorld)
toJSPtr val world = undef

//UTIL

jsDocument :: !*JSWorld -> (!JSPtr JSDocument,!*JSWorld)
jsDocument world
	= jsGetObjectAttr "document" jsWindow world

newJSArray :: !*JSWorld -> (!JSPtr [a], !*JSWorld)
newJSArray world
	# (constructor,world) = jsGetObjectAttr "Array" (jsWindow) world
	= jsNewObject constructor world

jsArrayPush :: !a (!JSPtr [a]) !*JSWorld -> (!JSPtr [a], !*JSWorld)
jsArrayPush x arr world = callObjectMethod "push" [x] arr world

jsArrayReverse :: (!JSPtr [a]) !*JSWorld -> (!JSPtr [a], !*JSWorld)
jsArrayReverse arr world = callObjectMethod "reverse" [] arr world

toJSArray :: ![a] !*JSWorld -> (!JSPtr [a], !*JSWorld)
toJSArray xs world
  # (arr, world) = newJSArray world
  # world = foldl (op arr) world (zip2 [0..] xs)
  = (arr, world)
  where op arr world (i, arg) = jsSetObjectEl i arg arr world

jsIsUndefined :: !a !*JSWorld -> (!Bool,!*JSWorld)
jsIsUndefined obj world
	# (type,world) = jsTypeof obj world
	= (type == "undefined",world)
	
getDomElement :: !DomElementId !*JSWorld ->(!JSPtr a, !*JSWorld)
getDomElement elemId world
	# (document,world) = jsDocument world
	= callObjectMethod "getElementById" [elemId] document world

getDomAttr :: !DomElementId !String !*JSWorld -> (!a, !*JSWorld)
getDomAttr elemId attr world
	# (elem,world)	= getDomElement elemId world
	= jsGetObjectAttr attr elem world
	
setDomAttr :: !DomElementId !String !b !*JSWorld -> *JSWorld
setDomAttr elemId attr value world
	# (elem,world)	= getDomElement elemId world
	= jsSetObjectAttr attr value elem world

findObject :: !String !*JSWorld -> *(!JSPtr a, !*JSWorld)
findObject query world
	# (obj,world)		= jsGetObjectAttr attr jsWindow world //deref first attr separate to make the typechecker happy
	= case attrs of
		[]	= (obj,world)
			= foldl op (obj,world) attrs
where
	[attr:attrs]	= split "." query
	op (obj,world) attr = jsGetObjectAttr attr obj world

callObjectMethod :: !String ![b] !(JSPtr a) !*JSWorld -> (!c,!*JSWorld)
callObjectMethod method args obj world
	# (fun,world) = jsGetObjectAttr method obj world
	# (arr,world) = toJSArray args world
	= jsApply fun obj arr world

addJSFromUrl :: !String !(Maybe (JSPtr (JSFunction a))) *JSWorld -> *JSWorld
addJSFromUrl url mbCallback world
	# (document,world) = jsDocument world
	//Create script tag
	# (script,world)	= callObjectMethod "createElement" ["script"] document world
	# world				= jsSetObjectAttr "src" url script world
	# world				= jsSetObjectAttr "type" "text/javascript" script world
	# world				= case mbCallback of
		Nothing			= world
		Just callback	= jsSetObjectAttr "onload" callback script world
	//Inject into the document head
	# (head,world)		= callObjectMethod "getElementsByTagName" ["head"] document world
	# (head,world)		= jsGetObjectEl 0 head world
	# (_,world)			= callObjectMethod "appendChild" [script] head world
	= world

jsTrace :: a *JSWorld -> *JSWorld
jsTrace val world
	# (console,world)	= findObject "console" world
	# (_,world)			= callObjectMethod "log" [val] console world
	= world

