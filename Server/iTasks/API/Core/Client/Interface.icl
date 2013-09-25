implementation module iTasks.API.Core.Client.Interface

import StdEnv, Data.Void, Data.Maybe, Text

:: JSWorld = JSWorld
:: JSVal a = JSVal !a

// It describes what is the goal, but the actual wrapping doesn't happen,
// don't try to unwrap it!
:: JSArg = E.a: JSArg (JSVal a)

:: JSWindow = JSWindow
:: JSDocument = JSDocument
:: JSFunction a = JSFunction
:: JSObject	= JSObject

jsNull :: (JSVal a)
jsNull = undef

jsWindow :: (JSVal JSWindow)
jsWindow = undef

jsDocument :: (JSVal JSDocument)
jsDocument = undef

jsEmptyObject :: !*JSWorld -> *(!JSVal a, !*JSWorld)
jsEmptyObject world = undef

jsNewObject	:: !String ![JSArg] !*JSWorld -> *(!JSVal b, !*JSWorld)
jsNewObject cons_name args world = undef

jsGetObjectAttr :: !String !(JSVal a) !*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectAttr attr obj world = undef

jsGetObjectEl :: !Int !(JSVal o) !*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectEl index obj world = undef

jsSetObjectAttr	:: !String !(JSVal v) !(JSVal o) !*JSWorld -> *(!JSVal o, !*JSWorld)
jsSetObjectAttr attr value obj world = undef

jsSetObjectEl :: !Int !(JSVal v) !(JSVal o) !*JSWorld -> *(!JSVal o, !*JSWorld)
jsSetObjectEl index value obj world = undef

jsDeleteObjectAttr :: !String !(JSVal o) !*JSWorld -> *(!JSVal o, !*JSWorld)
jsDeleteObjectAttr value obj world = undef

jsApply	:: !(JSVal (JSFunction f)) !(JSVal scope) ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)
jsApply fun scope args world = undef

jsThis :: !*JSWorld -> *(!JSVal a, !*JSWorld)
jsThis world = undef

jsTypeof :: !(JSVal a) -> String
jsTypeof obj = undef

newJSArray :: !*JSWorld -> *(!JSVal [a], !*JSWorld)
newJSArray world  = undef

toJSVal :: !a -> JSVal b
toJSVal val = undef

toJSArg :: !a -> JSArg
toJSArg val = undef

fromJSVal :: !(JSVal a) -> Dynamic
fromJSVal ptr = undef

//UTIL

jsArrayPush :: !(JSVal a) !(JSVal [a]) !*JSWorld -> *(!JSVal [a], !*JSWorld)
jsArrayPush x arr world = let (arr, _, w) = callObjectMethod "push" [toJSArg x] arr world in (arr, w)

jsArrayReverse :: !(JSVal [a]) !*JSWorld -> *(!JSVal [a], !*JSWorld)
jsArrayReverse arr world = let (arr, _, w) = callObjectMethod "reverse" [] arr world in (arr, w)

toJSArray :: ![a] !*JSWorld -> *(!JSVal [a], !*JSWorld)
toJSArray xs world
  # (arr, world) = newJSArray world
  # world = foldl (op arr) world (zip2 [0..] xs)
  = (arr, world)
  where op arr world (i, arg) = snd (jsSetObjectEl i (toJSVal arg) arr world)

jsIsUndefined :: !(JSVal a) -> Bool
jsIsUndefined obj = jsTypeof obj == "undefined"
	
getDomElement :: !DomElementId !*JSWorld -> *(!JSVal a, !*JSWorld)
getDomElement elemId world
	= let (val, _, w) = callObjectMethod "getElementById" [toJSArg elemId] jsDocument world in (val, w)

getDomAttr :: !DomElementId !String !*JSWorld -> *(!JSVal a, !*JSWorld)
getDomAttr elemId attr world
	# (elem,world)	= getDomElement elemId world
	= jsGetObjectAttr attr elem world
	
setDomAttr :: !DomElementId !String !(JSVal a) !*JSWorld -> *JSWorld
setDomAttr elemId attr value world
	# (elem, world)	= getDomElement elemId world
	= snd (jsSetObjectAttr attr value elem world)

findObject :: !String !*JSWorld -> *(!JSVal a, !*JSWorld)
findObject query world
	# (obj,world) = jsGetObjectAttr attr jsWindow world //deref first attr separate to make the typechecker happy
	= case attrs of
		[]	= (obj,world)
			= foldl op (obj,world) attrs
where
	[attr:attrs] = split "." query
	op (obj,world) attr | jsIsUndefined obj
		= (obj, world)
		= jsGetObjectAttr attr obj world

callObjectMethod	:: !String ![JSArg] !(JSVal o) !*JSWorld -> *(!JSVal c, !JSVal o, !*JSWorld)
callObjectMethod method args obj world
	# (fun, world) = jsGetObjectAttr method obj world
	= let (r, w) = jsApply fun obj args world in (r, obj, w)

addJSFromUrl :: !String !(Maybe (JSVal (JSFunction a))) !*JSWorld -> *JSWorld
addJSFromUrl url mbCallback world
	//Create script tag
	# (script,_,world)	= callObjectMethod "createElement" [toJSArg "script"] jsDocument world
	# (script,world)	= jsSetObjectAttr "src" (toJSVal url) script world
	# (script,world)	= jsSetObjectAttr "type" (toJSVal "text/javascript") script world
	# world				= case mbCallback of
		Nothing			= world
		Just callback	= snd (jsSetObjectAttr "onload" callback script world)
	//Inject into the document head
	# (head,_,world)	= callObjectMethod "getElementsByTagName" [toJSArg "head"] jsDocument world
	# (head,world)		= jsGetObjectEl 0 head world
	# (_,head,world)	= callObjectMethod "appendChild" [toJSArg script] head world
	= world

jsTrace :: a *JSWorld -> *JSWorld
jsTrace val world
	# (console,world)	= findObject "console" world
	# (_,console,world)	= callObjectMethod "log" [toJSArg val] console world
	= world

jsValToString :: !(JSVal a) -> String
jsValToString ptr = case fromJSVal ptr of
					(val :: String) = val
					(val :: Real)   = toString val
					(val :: Int)    = toString val
									= abort "JSVal cannot be converted to String"

jsValToReal :: !(JSVal a) -> Real
jsValToReal ptr = case fromJSVal ptr of
					(val :: Real)   = val
									= abort "Real was expected but something else came"

jsValToInt :: !(JSVal a) -> Int
jsValToInt ptr = case fromJSVal ptr of
					(val :: Int)	= val
								   	= abort "Integer was expected but something else came"

withDef :: !((JSVal a) -> b) !b !(JSVal a) -> b
withDef f def ptr | jsIsUndefined ptr
	= def 
	= f ptr
									
									


