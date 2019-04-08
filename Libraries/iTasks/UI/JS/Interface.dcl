definition module iTasks.UI.JS.Interface

from StdMaybe import :: Maybe

:: *JSWorld

:: JSVal a
:: JSObj a :== JSVal (JSObject a)
:: JSFun a :== JSVal (JSFunction a)

:: JSObject a
:: JSFunction a

class toJS a :: !a -> JSVal b
instance toJS Int, Bool, String, (JSVal b), (Maybe b) | toJS b

jsIsUndefined :: !(JSVal a) -> Bool

/**
 * Access properties of a JavaScript value.
 */
class (.#) infixl 3 attr :: !(JSVal a) !attr -> JSVal b

instance .# String // object access; may contain dots
instance .# Int // array access

(.?) infixl 1 :: !(JSVal a) !*JSWorld -> *(!JSVal r, !*JSWorld)
(.=) infixl 1 :: !(JSObj a) !b !*JSWorld -> *JSWorld | toJS b

class toJSArgs a :: !a -> [JSVal a]
instance toJSArgs Int, Bool, String, (JSVal b), (Maybe b) | toJS b, ()
instance toJSArgs (a,b) | toJS a & toJS b
instance toJSArgs (a,b,c) | toJS a & toJS b & toJS c
instance toJSArgs (a,b,c,d) | toJS a & toJS b & toJS c & toJS d
instance toJSArgs (a,b,c,d,e) | toJS a & toJS b & toJS c & toJS d & toJS e
instance toJSArgs (a,b,c,d,e,f) | toJS a & toJS b & toJS c & toJS d & toJS e & toJS f

(.$) infixl 2 :: !(JSFun a) !b !*JSWorld -> *(!JSVal c, !*JSWorld) | toJSArgs b
(.$!) infixl 2 :: !(JSFun a) !b !*JSWorld -> *JSWorld | toJSArgs b

jsNew :: !String !a !*JSWorld -> *(!JSVal b, !*JSWorld) | toJSArgs a

jsEmptyObject :: !*JSWorld -> *(!JSVal a, !*JSWorld)

jsGlobal :: !String -> JSVal a

jsNull :== jsGlobal "null"
jsThis :== jsGlobal "this"
jsWindow :== jsGlobal "window"
jsDocument :== jsGlobal "document"

/**
 * Should not be used outside this library. The argument is a reference to a
 * DOM element stored somewhere in JavaScript.
 */
referenceToJS :: !Int -> JSVal a

/**
 * @param A function of the type (a b .. z *JSWorld -> *JSWorld)
 */
jsWrapFun :: !f !*JSWorld -> *(!JSFun f, !*JSWorld)

/**
 * Load external CSS stylesheet by its URL.
 *
 * @param The URL.
 */
addCSSFromUrl :: !String !*JSWorld -> *JSWorld

/**
 * Load an external JavaScript file by its URL.
 *
 * @param The URL.
 * @param An optional callback function for when the script has loaded.
 */
addJSFromUrl :: !String !(Maybe (JSFun a)) !*JSWorld -> *JSWorld
