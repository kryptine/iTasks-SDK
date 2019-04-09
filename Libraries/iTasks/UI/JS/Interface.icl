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
	| JSUndefined

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
		JSUndefined -> "undefined"

		JSSel obj attr -> toString obj+++"["+++toString attr+++"]"
		JSSelPath obj path -> toString obj+++"."+++path

jsIsUndefined :: !(JSVal a) -> Bool
jsIsUndefined v = v=:JSUndefined

jsValToInt :: !(JSVal a) -> Maybe Int
jsValToInt v = case v of
	JSInt i    -> Just i
	JSString s -> case toInt s of
		0 -> if (s=="0") (Just 0) Nothing
		i -> Just i
	_          -> Nothing

jsValToBool :: !(JSVal a) -> Maybe Bool
jsValToBool v = case v of
	JSBool b   -> Just b
	JSInt i    -> Just (i<>0)
	JSString s -> case s of
		"true"  -> Just True
		"false" -> Just False
		_       -> Nothing
	_          -> Nothing

jsValToString :: !(JSVal a) -> Maybe String
jsValToString v = case v of
	JSString s -> Just s
	JSInt i    -> Just (toString i)
	JSBool b   -> Just (if b "true" "false")
	_          -> Nothing

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
jsEmptyObject w = (eval_js_with_return_value "{}", w)

jsGlobal :: !String -> JSVal a
jsGlobal s = JSVar s

jsWrapFun :: !({!JSVal a} *JSWorld -> *JSWorld) !*JSWorld -> *(!JSFun f, !*JSWorld)
jsWrapFun f world = (cast (share casting_f), world)
where
	casting_f :: !{!a} !*JSWorld -> *JSWorld
	casting_f args w = f {cast_value_from_js a \\ a <-: args} w

wrapInitUIFunction :: !((JSObj ()) *JSWorld -> *JSWorld) -> {!JSVal a} -> *JSWorld -> *JSWorld
wrapInitUIFunction f = \args
	| size args<>1
		-> abort ("failed to get iTasks component from JavaScript (args.size="+++toString (size args)+++")\n")
		-> case cast_value_from_js args.[0] of
			r=:(JSRef _)
				-> f r
				-> abort "failed to get iTasks component from JavaScript\n"

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
eval_js_with_return_value s = cast_value_from_js (eval s)
where
	eval :: !String -> a
	eval _ = code {
		instruction 2
	}

cast_value_from_js :: !a -> JSVal b
cast_value_from_js _ = code {
	eq_desc dINT 0 0
	jmp_true return_int
	eq_desc BOOL 0 0
	jmp_true return_bool
	eq_desc _STRING_ 0 0
	jmp_true return_string
	pushD_a 0
	pushI 5290 | 661*8+2; DOMNode (bcprelink.c)
	eqI
	jmp_true return_ref
	print "cast_value_from_js: return type unknown:\n"
	print_symbol_sc 0
	print "\n"
	halt
:return_int
	repl_r_args 0 1
	push_b 0
	pushI 4611686018427387904 | 1<<62; null
	eqI
	jmp_true return_null
	push_b 0
	pushI 4611686018427387905 | 1<<62+1; undefined
	eqI
	jmp_true return_undefined
	fill_r e_iTasks.UI.JS.Interface_kJSInt 0 1 0 0 0
	pop_b 1
	jmp return
:return_null
	fillh e_iTasks.UI.JS.Interface_dJSNull 0 0
	pop_b 1
	jmp return
:return_undefined
	fillh e_iTasks.UI.JS.Interface_dJSUndefined 0 0
	pop_b 1
	jmp return
:return_bool
	repl_r_args 0 1
	fill_r e_iTasks.UI.JS.Interface_kJSBool 0 1 0 0 0
	pop_b 1
	jmp return
:return_string
	fill_r e_iTasks.UI.JS.Interface_kJSString 1 0 1 0 0
	pop_a 1
	jmp return
:return_ref
	repl_r_args 0 1
	fill_r e_iTasks.UI.JS.Interface_kJSRef 0 1 0 0 0
	pop_b 1
	jmp return
:return
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
