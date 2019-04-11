definition module iTasks.UI.JS.Interface

import StdGeneric
from StdMaybe import :: Maybe
from StdOverloaded import class toString
from Text.GenJSON import :: JSONNode

:: *JSWorld

:: JSVal
:: JSFun :== JSVal
:: JSObj :== JSVal

generic gToJS a :: !a -> JSVal
derive gToJS Int, Bool, String, Real, JSVal, Maybe, (), JSONNode
derive gToJS PAIR, FIELD of {gfd_name}, RECORD
toJS x :== gToJS{|*|} x

jsMakeCleanReference :: a -> JSVal
jsGetCleanReference :: !JSVal !*JSWorld -> *(!Maybe b, !*JSWorld)

jsTypeOf :: !JSVal -> JSVal

jsIsUndefined :: !JSVal -> Bool
jsIsNull :: !JSVal -> Bool

jsValToInt :: !JSVal -> Maybe Int
jsValToBool :: !JSVal -> Maybe Bool
jsValToString :: !JSVal -> Maybe String
jsValToReal :: !JSVal -> Maybe Real

jsValToInt` :: !Int !JSVal -> Int
jsValToBool` :: !Bool !JSVal -> Bool
jsValToString` :: !String !JSVal -> String
jsValToReal` :: !Real !JSVal -> Real

/**
 * Access properties of a JavaScript value.
 */
class (.#) infixl 3 attr :: !JSVal !attr -> JSVal

instance .# String // object access; may contain dots
instance .# Int // array access

(.?) infixl 1 :: !JSVal !*JSWorld -> *(!JSVal, !*JSWorld)
(.=) infixl 1 :: !JSVal !b !*JSWorld -> *JSWorld | gToJS{|*|} b

class toJSArgs a :: !a -> [JSVal]
instance toJSArgs Int, Bool, String, JSVal, (Maybe b) | gToJS{|*|} b, ()
instance toJSArgs (a,b) | gToJS{|*|} a & gToJS{|*|} b
instance toJSArgs (a,b,c) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c
instance toJSArgs (a,b,c,d) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d
instance toJSArgs (a,b,c,d,e) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d & gToJS{|*|} e
instance toJSArgs (a,b,c,d,e,f) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d & gToJS{|*|} e & gToJS{|*|} f

(.$) infixl 2 :: !JSFun !b !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs b
(.$!) infixl 2 :: !JSFun !b !*JSWorld -> *JSWorld | toJSArgs b

jsNew :: !String !a !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs a

jsEmptyObject :: !*JSWorld -> *(!JSVal, !*JSWorld)

jsGlobal :: !String -> JSVal

jsNull :== jsGlobal "null"
jsThis :== jsGlobal "this"
jsWindow :== jsGlobal "window"
jsDocument :== jsGlobal "document"

jsWrapFun :: !({!JSVal} *JSWorld -> *JSWorld) !*JSWorld -> *(!JSFun, !*JSWorld)

wrapInitUIFunction :: !(JSVal *JSWorld -> *JSWorld) -> {!JSVal} -> *JSWorld -> *JSWorld

jsDeserializeGraph :: !String !*JSWorld -> *(!.a, !*JSWorld)

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
addJSFromUrl :: !String !(Maybe JSFun) !*JSWorld -> *JSWorld

jsTrace :: !a .b -> .b | toString a
