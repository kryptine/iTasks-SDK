implementation module iTasks.UI.JS.Interface

import StdEnv
import Text

:: *JSWorld = JSWorld

:: JSVal a
	= JSInt !Int
	| JSString !String
	| JSRef !Int // a reference to shared_js_values
	| JSCleanRef !Int // a reference to shared_clean_values
	| JSVar !String

	| E.b c: JSSel !(JSVal b) !(JSVal c) // b[c]
	| E.b: JSSelPath !(JSVal b) !String // b.path1.path2...pathn

:: JSObject a :== ()
:: JSFunction a :== ()

instance toString (JSVal a)
where
	toString v = case v of
		JSInt i -> toString i
		JSString s -> "'"+++s+++"'" // TODO escape
		JSRef i -> "abc_interpreter.shared_js_values["+++toString i+++"]"
		JSCleanRef i -> "abc_interpreter.apply_to_clean_value("+++toString i+++")"
		JSVar v -> v

		JSSel obj attr -> toString obj+++"["+++toString attr+++"]"
		JSSelPath obj path -> toString obj+++"."+++path

js_set :: !(JSVal a) !(JSVal b) !*JSWorld -> *JSWorld
js_set var val w = case eval_js (toString var+++"="+++toString val) of
	True -> w

instance toJS Int where toJS i = JSInt i
instance toJS String where toJS s = JSString s

instance .# String where .# obj path = JSSelPath obj path
instance .# Int where .# arr i = JSSel arr (JSInt i)

(.=) infixl 1 :: !(JSObj a) !(JSVal b) !*JSWorld -> *JSWorld
(.=) sel v w = js_set sel v w

instance toJSArgs Int where toJSArgs i = [toJS i]
instance toJSArgs String where toJSArgs s = [toJS s]
instance toJSArgs () where toJSArgs _ = []

instance toJSArgs (a,b) | toJS a & toJS b
where toJSArgs (a,b) = [toJS a, toJS b]

instance toJSArgs (a,b,c) | toJS a & toJS b & toJS c
where toJSArgs (a,b,c) = [toJS a, toJS b, toJS c]

instance toJSArgs (a,b,c,d) | toJS a & toJS b & toJS c & toJS d
where toJSArgs (a,b,c,d) = [toJS a, toJS b, toJS c, toJS d]

instance toJSArgs (a,b,c,d,e) | toJS a & toJS b & toJS c & toJS d & toJS e
where toJSArgs (a,b,c,d,e) = [toJS a, toJS b, toJS c, toJS d, toJS e]

instance toJSArgs (a,b,c,d,e,f) | toJS a & toJS b & toJS c & toJS d & toJS e & toJS f
where toJSArgs (a,b,c,d,e,f) = [toJS a, toJS b, toJS c, toJS d, toJS e, toJS f]

(.$!) infixl 2 :: !(JSFun a) !b !*JSWorld -> *JSWorld | toJSArgs b
(.$!) f args w = case eval_js call of
	True -> w
where
	call = toString f+++"("+++join "," [toString a \\ a <- toJSArgs args]+++")"

jsGlobal :: !String -> JSVal a
jsGlobal s = JSVar s

jsWrapFun :: !f !*JSWorld -> *(!JSFun f, !*JSWorld)
jsWrapFun f world = (cast (share f), world)

referenceToJS :: !Int -> JSVal a
referenceToJS ref = JSRef ref

eval_js :: !String -> Bool
eval_js s = code {
	instruction 1
	pop_a 1
	pushB TRUE
}

// TODO: this value may live on the heap, so the garbage collector should
// inspect the references in the JavaScript world and not remove shared values.
share :: !a -> JSVal b
share x = JSCleanRef (get_shared_value_index x)
where
	get_shared_value_index :: !a -> Int
	get_shared_value_index _ = code {
		pushI 0 | to return the result
		instruction 2
		pop_a 1
	}

cast :: !.a -> .b
cast _ = code {
	no_op
}
