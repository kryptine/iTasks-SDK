implementation module iTasks.API.Core.Client.Interface

import StdEnv, StdGeneric, Data.Void, Data.Maybe, Text

:: JSWorld  = JSWorld
:: JSVal a  = JSVal !a

// It describes what is the goal, but the actual wrapping doesn't happen,
// don't try to unwrap it!
:: JSArg = E.a: JSArg (JSVal a)

:: JSWindow     = JSWindow
:: JSDocument   = JSDocument
:: JSFunction a = JSFunction
:: JSArray    a = JSArray
:: JSObject   a = JSObject
:: JSEvent      = JSEvent

jsNull :: JSVal a
jsNull = undef

jsWindow :: JSObj JSWindow
jsWindow = undef

jsDocument :: JSObj JSDocument
jsDocument = undef

jsEmptyObject :: !*JSWorld -> *(!JSObj a, !*JSWorld)
jsEmptyObject world = undef

jsNewObject	:: !String ![JSArg] !*JSWorld -> *(!JSObj b, !*JSWorld)
jsNewObject cons_name args world = undef

jsGetObjectAttr :: !String !(JSObj a) !*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectAttr attr obj world = undef

jsGetObjectEl :: !Int !(JSObj o) !*JSWorld -> *(!JSVal b, !*JSWorld)
jsGetObjectEl index obj world = undef

jsSetObjectAttr	:: !String !(JSVal v) !(JSObj o) !*JSWorld -> *JSWorld
jsSetObjectAttr attr value obj world = undef

jsSetObjectEl :: !Int !(JSVal v) !(JSObj o) !*JSWorld -> *JSWorld
jsSetObjectEl index value obj world = undef

jsDeleteObjectAttr :: !String !(JSObj o) !*JSWorld -> *JSWorld
jsDeleteObjectAttr value obj world = undef

class JSObjAttr a where
  jsSetter :: !a !(JSVal v) !(JSObj o) !*JSWorld -> !*JSWorld
  jsGetter :: !a            !(JSObj o) !*JSWorld -> *(!JSVal b, !*JSWorld)

instance JSObjAttr String where
  jsSetter idx val obj world = jsSetObjectAttr idx val obj world
  jsGetter idx     obj world = jsGetObjectAttr idx obj world

instance JSObjAttr Int where
  jsSetter idx val obj world = jsSetObjectEl idx val obj world
  jsGetter idx     obj world = jsGetObjectEl idx obj world

(.#) infixl 3 :: !(JSObj a) !t -> !(JSObj a, t) | JSObjAttr t
(.#) a b = (a, b)

.? :: !(!JSObj o, !t) !*JSWorld -> !*(!JSVal r, !*JSWorld) | JSObjAttr t
.? (obj, attr) world = jsGetter attr obj world

(.=) infixl 2 :: !(!JSObj o, !t) !v -> !*(!*JSWorld -> !*JSWorld) | JSObjAttr t
(.=) (obj, attr) val = \world -> jsSetter attr (toJSVal val) obj world

jsApply	:: !(JSFun f) !(JSObj scope) ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)
jsApply fun scope args world = undef

jsWrapFun :: !([JSArg] *JSWorld -> *(!JSVal a, !*JSWorld)) !*JSWorld -> *(!JSFun f, !*JSWorld)
jsWrapFun fun world = undef

jsThis :: !*JSWorld -> *(!JSObj a, !*JSWorld)
jsThis world = undef

jsTypeof :: !(JSVal a) -> String
jsTypeof obj = undef

jsAbort :: a -> b
jsAbort _ = undef

newJSArray :: !*JSWorld -> *(!JSArr a, !*JSWorld)
newJSArray world  = undef

toJSVal :: !a -> JSVal b
toJSVal val = undef

toJSArg :: !a -> JSArg
toJSArg val = undef

toJSArgs :: ![a] -> [JSArg]
toJSArgs xs = map toJSArg xs

fromJSValUnsafe :: !(JSVal a) -> Dynamic
fromJSValUnsafe ptr = undef

fromJSVal :: !(JSVal a) !*JSWorld -> *(!Dynamic, !*JSWorld)
fromJSVal ptr world = undef

//UTIL

jsArrayPush :: !(JSVal a) !(JSArr a) !*JSWorld -> *(!JSArr a, !*JSWorld)
jsArrayPush x arr world = callObjectMethod "push" [toJSArg x] arr world

jsArrayPop :: !(JSArr a) !*JSWorld -> *(!JSVal a, !*JSWorld)
jsArrayPop arr world = callObjectMethod "pop" [] arr world

jsArrayReverse :: !(JSArr a) !*JSWorld -> *(!JSArr a, !*JSWorld)
jsArrayReverse arr world = callObjectMethod "reverse" [] arr world

toJSArray :: ![a] !*JSWorld -> *(!JSArr a, !*JSWorld)
toJSArray xs world
  # (arr, world) = newJSArray world
  # world = foldl (op arr) world (zip2 [0..] xs)
  = (arr, world)
  where op arr world (i, arg) = jsSetObjectEl i (toJSVal arg) arr world

fromJSArray :: (JSArr a) ((JSVal b) -> c) !*JSWorld -> *([c], !*JSWorld)
fromJSArray arr f world
  # (l, world) = jsGetObjectAttr "length" arr world
  = fromJSArray` 0 (jsValToInt l) arr world
  where
  fromJSArray` n l arr world
    | n == l         = ([], world)
    | otherwise
      # (x, world)   = jsGetObjectEl n arr world
      # (xs`, world) = fromJSArray` (n + 1) l arr world
      = ([f x : xs`], world)

jsIsUndefined :: !(JSVal a) -> Bool
jsIsUndefined obj = jsTypeof obj == "undefined"
	
getDomElement :: !DomElementId !*JSWorld -> *(!JSObj a, !*JSWorld)
getDomElement elemId world
	= callObjectMethod "getElementById" [toJSArg elemId] jsDocument world

getDomAttr :: !DomElementId !String !*JSWorld -> *(!JSVal a, !*JSWorld)
getDomAttr elemId attr world
	# (elem, world)	= getDomElement elemId world
	= jsGetObjectAttr attr elem world
	
setDomAttr :: !DomElementId !String !(JSVal a) !*JSWorld -> *JSWorld
setDomAttr elemId attr value world
	# (elem, world)	= getDomElement elemId world
	= jsSetObjectAttr attr value elem world

findObject :: !String !*JSWorld -> *(!JSVal a, !*JSWorld)
findObject query world
  # (obj,world) = jsGetObjectAttr attr jsWindow world //deref first attr separate to make the typechecker happy
  = case attrs of
      []  = (obj,world)
          = foldl op (obj,world) attrs
  where
    [attr:attrs] = split "." query
    op (obj, world) attr = ifObj obj
                             (\obj -> jsGetObjectAttr attr obj)
                             obj world

class ToArgs a where
  toArgs :: a -> [JSArg]

instance ToArgs Void where
  toArgs _ = []

instance ToArgs JSArg where
  toArgs x = [x]

instance ToArgs [JSArg] where
  toArgs xs = xs

instance ToArgs (a, b) where
  toArgs (x, y) = [toJSArg x, toJSArg y]

instance ToArgs (a, b, c) where
  toArgs (x, y, z) = [toJSArg x, toJSArg y, toJSArg z]

instance ToArgs (a, b, c, d) where
  toArgs (x, y, z, p) = [toJSArg x, toJSArg y, toJSArg z, toJSArg p]

instance ToArgs (a, b, c, d, e) where
  toArgs (x, y, z, p, q) = [toJSArg x, toJSArg y, toJSArg z, toJSArg p, toJSArg q]

instance ToArgs (a, b, c, d, e, f) where
  toArgs (x, y, z, p, q, r) = [toJSArg x, toJSArg y, toJSArg z, toJSArg p, toJSArg q, toJSArg r]

class JSCall o where
  (.$) infixl 1 :: !o !a -> *(*JSWorld -> *(JSVal r, !*JSWorld)) | ToArgs a

instance JSCall String where
  (.$) fun args = \world -> callFunction fun (toArgs args) world

instance JSCall (JSObj o, String) where
  (.$) (obj, fun) args = \world -> callObjectMethod fun (toArgs args) obj world


callObjectMethod	:: !String ![JSArg] !(JSObj o) !*JSWorld -> *(!JSVal c, !*JSWorld)
callObjectMethod method args obj world
	# (fun, world) = jsGetObjectAttr method obj world
	= jsApply fun obj args world

addJSFromUrl :: !String !(Maybe (JSFun a)) !*JSWorld -> *JSWorld
addJSFromUrl url mbCallback world
	//Create script tag
	# (script,world)	= callObjectMethod "createElement" [toJSArg "script"] jsDocument world
	# world				= jsSetObjectAttr "src" (toJSVal url) script world
	# world				= jsSetObjectAttr "type" (toJSVal "text/javascript") script world
	# world				= jsSetObjectAttr "async" (toJSVal False) script world
	# world				= case mbCallback of
		Nothing			= world
		Just callback	= jsSetObjectAttr "onload" callback script world
	//Inject into the document head
	# (head,world)		= callObjectMethod "getElementsByTagName" [toJSArg "head"] jsDocument world
	# (head,world)		= jsGetObjectEl 0 head world
	# (_,world)			= callObjectMethod "appendChild" [toJSArg script] head world
	= world

addCSSFromUrl :: !String !*JSWorld -> *JSWorld
addCSSFromUrl url world
    # (link,world)      = callObjectMethod "createElement" [toJSArg "link"] jsDocument world
	# world				= jsSetObjectAttr "rel" (toJSVal "stylesheet") link world
	# world				= jsSetObjectAttr "type" (toJSVal "text/css") link world
	# world				= jsSetObjectAttr "href" (toJSVal url) link world
	# world				= jsSetObjectAttr "async" (toJSVal True) link world
	//Inject into the document head
	# (head,world)		= callObjectMethod "getElementsByTagName" [toJSArg "head"] jsDocument world
	# (head,world)		= jsGetObjectEl 0 head world
	# (_,world)			= callObjectMethod "appendChild" [toJSArg link] head world
	= world

jsTrace :: a *JSWorld -> *JSWorld
jsTrace val world
	# (console,world)	= findObject "console" world
	# (_,world)			= callObjectMethod "log" [toJSArg val] console world
	= world

jsValToString :: !(JSVal a) -> String
jsValToString ptr = case fromJSValUnsafe ptr of
					(val :: String) = val
					(val :: Real)   = toString val
					(val :: Int)    = toString val
					val				= jsAbort val
									//= jsAbort "JSVal cannot be converted to String"

jsValToReal :: !(JSVal a) -> Real
jsValToReal ptr = case fromJSValUnsafe ptr of
					(val :: Real)   = val
					(val :: String)	= toReal val
					(val :: Int)    = toReal val
									= abort "Real was expected but something else came"

jsValToInt :: !(JSVal a) -> Int
jsValToInt ptr = case fromJSValUnsafe ptr of
					(val :: Int)	= val
					(val :: String)	= toInt val
					(val :: Real)	= toInt val
								   	= abort "Int was expected but something else came"

jsValToBool :: !(JSVal a) -> Bool
jsValToBool ptr = case fromJSValUnsafe ptr of
					(val :: Bool)	= val
								   	= abort "Bool was expected but something else came"

withDef :: !((JSVal a) -> b) !b !(JSVal a) -> b
withDef f def ptr | jsIsUndefined ptr
	= def 
	= f ptr

callFunction :: !String ![JSArg] !*JSWorld -> *(!JSVal a, !*JSWorld)
callFunction fn args world = callObjectMethod fn args jsWindow world

jsUnsafeCoerce :: !(JSVal a) -> JSVal b
jsUnsafeCoerce x = undef

jsUnsafeObjCoerce :: !(JSVal a) -> JSObj b
jsUnsafeObjCoerce x = undef

jsUnsafeArrCoerce :: !(JSVal a) -> JSArr b
jsUnsafeArrCoerce x = undef

jsUnsafeFunCoerce :: !(JSVal a) -> JSFun b
jsUnsafeFunCoerce x = undef

ifObj :: !(JSVal a) !((JSObj b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)
ifObj val f def world
  | t == "object" || t == "string" = f (jsUnsafeObjCoerce val) world
  | otherwise                      = (def, world)
  where t = jsTypeof val

ifArr :: !(JSVal a) !((JSArr b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)
ifArr val f def world
  # (isArr, world) = jsIsArray val world
  | isArr     = f (jsUnsafeArrCoerce val) world
  | otherwise = (def, world)

ifFun :: !(JSVal a) !((JSFun b) *JSWorld -> *(JSVal c, *JSWorld)) !(JSVal c) !*JSWorld -> *(!JSVal c, !*JSWorld)
ifFun val f def world
  | jsTypeof val == "function" = f (jsUnsafeFunCoerce val) world
  | otherwise                  = (def, world)

jsIsArray :: !(JSVal a) !*JSWorld -> *(!Bool, !*JSWorld)
jsIsArray x world
  # (arr, world) = findObject "Array" world
  # (jb, world)  = callObjectMethod "isArray" [] arr world
  = (jsValToBool jb, world)

jsIsNull :: !(JSVal a) -> Bool
jsIsNull x = undef
