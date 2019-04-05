implementation module iTasks.UI.JS.Interface

import StdEnv

:: *JSWorld = JSWorld

:: JSVal a
	= JSInt !Int
	| JSString !String
	| JSRef !Int // a reference to shared_js_values
	| JSCleanRef !Int // a reference to shared_clean_values
	| JSNull

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
		JSNull -> "null"

		JSSel obj attr -> toString obj+++"["+++toString attr+++"]"
		JSSelPath obj path -> toString obj+++"."+++path

js_set :: !(JSVal a) !(JSVal b) !*JSWorld -> *JSWorld
js_set var val w = case eval_js (toString var+++"="+++toString val) of
	True -> w

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

instance toJS Int where toJS i = JSInt i
instance toJS String where toJS s = JSString s

jsNull :: JSVal a
jsNull = JSNull

referenceToJS :: !Int -> JSVal a
referenceToJS ref = JSRef ref

instance .# String where .# obj path = JSSelPath obj path
instance .# Int where .# arr i = JSSel arr (JSInt i)

(.=) infixl 2 :: !(JSObj a) !(JSVal b) !*JSWorld -> *JSWorld
(.=) sel v w = js_set sel v w

jsWrapFun :: !f !*JSWorld -> *(!JSFun f, !*JSWorld)
jsWrapFun f world = (cast (share f), world)
