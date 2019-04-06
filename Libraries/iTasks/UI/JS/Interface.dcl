definition module iTasks.UI.JS.Interface

:: *JSWorld

:: JSVal a
:: JSObj a :== JSVal (JSObject a)
:: JSFun a :== JSVal (JSFunction a)

:: JSObject a
:: JSFunction a

class toJS a :: !a -> JSVal b
instance toJS Int, String

/**
 * Access properties of a JavaScript value.
 */
class (.#) infixl 3 attr :: !(JSVal a) !attr -> JSVal b

instance .# String // object access; may contain dots
instance .# Int // array access

(.=) infixl 1 :: !(JSObj a) !(JSVal b) !*JSWorld -> *JSWorld

class toJSArgs a :: !a -> [JSVal a]
instance toJSArgs Int, String, ()
instance toJSArgs (a,b) | toJS a & toJS b
instance toJSArgs (a,b,c) | toJS a & toJS b & toJS c
instance toJSArgs (a,b,c,d) | toJS a & toJS b & toJS c & toJS d
instance toJSArgs (a,b,c,d,e) | toJS a & toJS b & toJS c & toJS d & toJS e
instance toJSArgs (a,b,c,d,e,f) | toJS a & toJS b & toJS c & toJS d & toJS e & toJS f

(.$!) infixl 2 :: !(JSFun a) !b !*JSWorld -> *JSWorld | toJSArgs b

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
