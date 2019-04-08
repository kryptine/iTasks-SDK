implementation module iTasks.UI.JS.Interface

import StdEnv
import StdMaybe
import Text

:: *JSWorld = JSWorld

:: JSVal a
	= JSInt !Int
	| JSBool !Bool
	| JSString !String
	| JSRef !Int // a reference to shared_js_values
	| JSCleanRef !Int // a reference to shared_clean_values
	| JSVar !String
	| JSNull

	| E.b c: JSSel !(JSVal b) !(JSVal c) // b[c]
	| E.b: JSSelPath !(JSVal b) !String // b.path1.path2...pathn

:: JSObject a :== ()
:: JSFunction a :== ()

instance toString (JSVal a)
where
	toString v = case v of
		JSInt i -> toString i
		JSBool b -> if b "true" "false"
		JSString s -> "'"+++s+++"'" // TODO escape
		JSRef i -> "abc_interpreter.shared_js_values["+++toString i+++"]"
		JSCleanRef i -> "abc_interpreter.apply_to_clean_value("+++toString i+++")"
		JSVar v -> v
		JSNull -> "null"

		JSSel obj attr -> toString obj+++"["+++toString attr+++"]"
		JSSelPath obj path -> toString obj+++"."+++path

instance toJS Int where toJS i = JSInt i
instance toJS Bool where toJS b = JSBool b
instance toJS String where toJS s = JSString s
instance toJS (JSVal b) where toJS val = cast val

instance toJS (Maybe b) | toJS b
where
	toJS val = case val of
		Just v  -> toJS v
		Nothing -> JSNull

instance .# String where .# obj path = JSSelPath obj path
instance .# Int where .# arr i = JSSel arr (JSInt i)

(.?) infixl 1 :: !(JSVal a) !*JSWorld -> *(!JSVal r, !*JSWorld)
(.?) sel w = (eval_js_with_return_value (toString sel), w)

(.=) infixl 1 :: !(JSObj a) !b !*JSWorld -> *JSWorld | toJS b
(.=) sel v w = case eval_js (toString sel+++"="+++toString (toJS v)) of
	True -> w

instance toJSArgs Int where toJSArgs i = [toJS i]
instance toJSArgs Bool where toJSArgs b = [toJS b]
instance toJSArgs String where toJSArgs s = [toJS s]
instance toJSArgs (JSVal b) where toJSArgs v = [cast v]
instance toJSArgs (Maybe b) | toJS b
where
	toJSArgs v = case v of
		Just v  -> [toJS v]
		Nothing -> [JSNull]
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

(.$) infixl 2 :: !(JSFun a) !b !*JSWorld -> *(!JSVal c, !*JSWorld) | toJSArgs b
(.$) f args w = (eval_js_with_return_value call, w)
where
	call = toString f+++"("+++join "," [toString a \\ a <- toJSArgs args]+++")"

(.$!) infixl 2 :: !(JSFun a) !b !*JSWorld -> *JSWorld | toJSArgs b
(.$!) f args w = case eval_js call of
	True -> w
where
	call = toString f+++"("+++join "," [toString a \\ a <- toJSArgs args]+++")"

jsNew :: !String !a !*JSWorld -> *(!JSVal b, !*JSWorld) | toJSArgs a
jsNew cons args w = (eval_js_with_return_value call, w)
where
	call = "new "+++cons+++"("+++join "," [toString a \\ a <- toJSArgs args]+++")"

jsEmptyObject :: !*JSWorld -> *(!JSVal a, !*JSWorld)
jsEmptyObject w = (store_js_value "{}", w)

jsGlobal :: !String -> JSVal a
jsGlobal s = JSVar s

jsWrapFun :: !f !*JSWorld -> *(!JSFun f, !*JSWorld)
jsWrapFun f world = (cast (share f), world)

referenceToJS :: !Int -> JSVal a
referenceToJS ref = JSRef ref

addCSSFromUrl :: !String !*JSWorld -> *JSWorld
addCSSFromUrl css w = case add_css css of
	True -> w
where
	add_css :: !String -> Bool
	add_css _ = code {
		instruction 10
		pop_a 1
		pushB TRUE
	}

addJSFromUrl :: !String !(Maybe (JSFun a)) !*JSWorld -> *JSWorld
addJSFromUrl js mbCallback w = case add_js js callback of
	True -> w
where
	callback = case mbCallback of
		Just cb -> toString cb
		Nothing -> ""

	add_js :: !String !String -> Bool
	add_js _ _ = code {
		instruction 11
		pop_a 2
		pushB TRUE
	}

eval_js :: !String -> Bool
eval_js s = code {
	instruction 1
	pop_a 1
	pushB TRUE
}

eval_js_with_return_value :: !String -> JSVal a
eval_js_with_return_value s = code {
	instruction 2
	eq_desc dINT 0 0
	jmp_true return_int
	print "eval_js_with_return_value: return type unknown\n"
	halt
:return_int
	repl_r_args 0 1
	fill_r e_iTasks.UI.JS.Interface_kJSInt 0 1 0 0 0
	jmp return
:return
}

store_js_value :: !String -> JSVal a
store_js_value s = JSRef (eval_and_store s)
where
	eval_and_store :: !String -> Int
	eval_and_store _ = code {
		pushI 0 | to return the result
		instruction 3
		pop_a 1
	}

share :: !a -> JSVal b
share x = JSCleanRef (get_shared_value_index x)
where
	get_shared_value_index :: !a -> Int
	get_shared_value_index _ = code {
		pushI 0 | to return the result
		instruction 4
		pop_a 1
	}

cast :: !.a -> .b
cast _ = code {
	no_op
}
