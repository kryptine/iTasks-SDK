definition module iTasks.UI.JS.Interface

:: *JSWorld

:: JSVal a
:: JSObj a :== JSVal (JSObject a)
:: JSFun a :== JSVal (JSFunction a)

:: JSObject a
:: JSFunction a

class toJS a :: !a -> JSVal a
instance toJS Int, String

jsNull :: JSVal a

/**
 * Should not be used outside this library. The argument is a reference to a
 * DOM element stored somewhere in JavaScript.
 */
referenceToJS :: !Int -> JSVal a

/**
 * Access properties of a JavaScript value.
 */
class (.#) infixl 3 attr :: !(JSVal a) !attr -> JSObj b

instance .# String // object access; may contain dots
instance .# Int // array access

(.=) infixl 2 :: !(JSObj a) !(JSVal b) !*JSWorld -> *JSWorld

/**
 * @param A function of the type (a b .. z *JSWorld -> *JSWorld)
 */
jsWrapFun :: !f !*JSWorld -> *(!JSFun f, !*JSWorld)
