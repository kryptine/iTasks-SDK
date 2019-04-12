implementation module iTasks.UI.JS.Interface

import StdEnv
import StdGeneric
import StdOverloadedList

import Data.Maybe
import Text
import Text.GenJSON

:: *JSWorld = JSWorld

:: JSVal
	= JSInt !Int
	| JSBool !Bool
	| JSString !String
	| JSReal !Real

	| JSVar !String
	| JSNull
	| JSUndefined
	| JSTypeOf !JSVal

	| JSObject !{!JSObjectElement}
	| JSArray !{!JSVal}

	| JSSel !JSVal !JSVal // x[y]
	| JSSelPath !JSVal !String // x.path1.path2...pathn

	| JSRef !Int // a reference to js
	| JSCleanRef !Int // a reference to shared_clean_values

	| JSTempPair !JSVal !JSVal
	| JSTempField !String !JSVal

:: JSObjectElement =
	{ key :: !String
	, val :: !JSVal
	}

// TODO optimise this by first computing the size
instance toString JSVal
where
	toString v = case v of
		JSInt i -> toString i
		JSBool b -> if b "true" "false"
		JSString s -> "'"+++s+++"'" // TODO escape
		JSReal r -> toString r

		JSVar v -> v
		JSNull -> "null"
		JSUndefined -> "undefined"
		JSTypeOf v -> "typeof "+++toString v

		JSObject elems -> foldl (+++) "{" [key+++":"+++toString val+++"," \\ {key,val} <-: elems] +++ "}"
		JSArray elems -> foldl (+++) "[" [toString v+++"," \\ v <-: elems] +++ "]"

		JSSel obj attr -> toString obj+++"["+++toString attr+++"]"
		JSSelPath obj path -> toString obj+++"."+++path

		JSRef i -> "ABC.js["+++toString i+++"]"
		JSCleanRef i -> "ABC.ap("+++toString i+++")"

jsMakeCleanReference :: a -> JSVal
jsMakeCleanReference x = share x

jsGetCleanReference :: !JSVal !*JSWorld -> *(!Maybe b, !*JSWorld)
jsGetCleanReference v w = case eval_js_with_return_value (toString v) of
	JSCleanRef i -> (Just (fetch i), w)
	_            -> (Nothing, w)
where
	fetch :: !Int -> a
	fetch _ = code {
		create
		instruction 4
		pop_b 1
	}

jsTypeOf :: !JSVal -> JSVal
jsTypeOf v = JSTypeOf v

jsIsUndefined :: !JSVal -> Bool
jsIsUndefined v = v=:JSUndefined

jsIsNull :: !JSVal -> Bool
jsIsNull v = v=:JSNull

jsValToInt :: !JSVal -> Maybe Int
jsValToInt v = case v of
	JSInt i    -> Just i
	JSReal r   -> Just (toInt r)
	JSString s -> case toInt s of
		0 -> if (s=="0") (Just 0) Nothing
		i -> Just i
	_          -> Nothing

jsValToBool :: !JSVal -> Maybe Bool
jsValToBool v = case v of
	JSBool b   -> Just b
	JSInt i    -> Just (i<>0)
	JSReal r   -> Just (r<>0.0)
	JSString s -> case s of
		"true"  -> Just True
		"false" -> Just False
		_       -> Nothing
	_          -> Nothing

jsValToString :: !JSVal -> Maybe String
jsValToString v = case v of
	JSString s -> Just s
	JSInt i    -> Just (toString i)
	JSReal r   -> Just (toString r)
	JSBool b   -> Just (if b "true" "false")
	_          -> Nothing

jsValToReal :: !JSVal -> Maybe Real
jsValToReal v = case v of
	JSReal r   -> Just r
	JSInt i    -> Just (toReal i)
	JSString s -> Just (toReal s)
	_          -> Nothing

jsValToInt` :: !Int !JSVal -> Int
jsValToInt` i v = fromMaybe i (jsValToInt v)

jsValToBool` :: !Bool !JSVal -> Bool
jsValToBool` b v = fromMaybe b (jsValToBool v)

jsValToString` :: !String !JSVal -> String
jsValToString` s v = fromMaybe s (jsValToString v)

jsValToReal` :: !Real !JSVal -> Real
jsValToReal` r v = fromMaybe r (jsValToReal v)

jsValToList :: !JSVal !(JSVal -> Maybe a) !*JSWorld -> *(!Maybe [a], !*JSWorld)
jsValToList arr get w
# (len,w) = arr .# "length" .? w
= case jsValToInt len of
	Nothing  -> (Nothing,w)
	Just len -> get_elements [] (len-1) w
where
	get_elements xs -1 w = (Just xs,w)
	get_elements xs i w
	# (x,w) = arr .# i .? w
	= case get x of
		Nothing -> (Nothing,w)
		Just x  -> get_elements [x:xs] (i-1) w

jsValToList` :: !JSVal !(JSVal -> a) !*JSWorld -> *(![a], !*JSWorld)
jsValToList` arr get w
# (len,w) = arr .# "length" .? w
= case jsValToInt len of
	Nothing  -> ([],w)
	Just len -> get_elements [] (len-1) w
where
	get_elements xs -1 w = (xs,w)
	get_elements xs i w
	# (x,w) = arr .# i .? w
	= get_elements [get x:xs] (i-1) w

gToJS{|Int|} i = JSInt i
gToJS{|Bool|} b = JSBool b
gToJS{|String|} s = JSString s
gToJS{|Real|} r = JSReal r
gToJS{|JSVal|} v = v
gToJS{|Maybe|} fx v = case v of
	Nothing -> JSNull
	Just x  -> fx x
gToJS{|[]|} fx xs = JSArray {fx x \\ x <- xs}
gToJS{|JSONNode|} n = case n of
	JSONNull -> JSNull
	JSONBool b -> JSBool b
	JSONInt i -> JSInt i
	JSONReal r -> JSReal r
	JSONString s -> JSString s
	JSONArray xs -> JSArray {toJS x \\ x <- xs}
	JSONObject xs -> JSObject {{key=k,val=toJS v} \\ (k,v) <- xs}
	_ -> abort "missing case in gToJS{|JSONNode|}"

gToJS{|PAIR|} fx fy (PAIR x y) = JSTempPair (fx x) (fy y)
gToJS{|FIELD of {gfd_name}|} fx (FIELD x) = JSTempField gfd_name (fx x)
gToJS{|RECORD|} fx (RECORD x) = JSObject {e \\ e <|- collect_elems (fx x)}
where
	collect_elems :: !JSVal -> [!JSObjectElement!]
	collect_elems (JSTempField k v) = [!{key=k,val=v}!]
	collect_elems (JSTempPair a b)  = collect_elems a ++| collect_elems b

instance .# Int
where
	.# arr i = case arr of
		JSArray xs
			| 0<=i && i<size xs -> xs.[i]
			| otherwise         -> JSUndefined
		arr -> JSSel arr (JSInt i)

instance .# String
where
	.# obj path
		| contains_dot (size path-1) path
			= JSSelPath obj path
			= JSSel obj (JSString path)
	where
		contains_dot -1 _ = False
		contains_dot i s = if (s.[i]=='.') True (contains_dot (i-1) s)

(.?) infixl 1 :: !JSVal !*JSWorld -> *(!JSVal, !*JSWorld)
(.?) js w
# (done,js) = try_local_computation js
| done
	= (js,w)
	= (eval_js_with_return_value (toString js), w)
where
	try_local_computation :: !JSVal -> (!Bool, !JSVal)
	try_local_computation v = case v of
		JSInt _      -> (True,v)
		JSBool _     -> (True,v)
		JSString _   -> (True,v)
		JSReal _     -> (True,v)

		JSNull       -> (True,v)
		JSUndefined  -> (True,v)

		JSSel (JSArray xs) (JSInt i)
			| 0<=i && i<size xs -> try_local_computation xs.[i]
			| otherwise         -> (True,JSUndefined)
		// TODO add a case for JSObject and JSString?

		JSRef _      -> (True,v)
		JSCleanRef _ -> (True,v)

		_            -> (False,v)

(.=) infixl 1 :: !JSVal !b !*JSWorld -> *JSWorld | gToJS{|*|} b
(.=) sel v w = case eval_js (toString sel+++"="+++toString (toJS v)) of
	True -> w

instance toJSArgs Int where toJSArgs i = [toJS i]
instance toJSArgs Bool where toJSArgs b = [toJS b]
instance toJSArgs String where toJSArgs s = [toJS s]
instance toJSArgs JSVal where toJSArgs v = [cast v]
instance toJSArgs (Maybe b) | gToJS{|*|} b
where
	toJSArgs v = case v of
		Just v  -> [toJS v]
		Nothing -> [JSNull]
instance toJSArgs () where toJSArgs _ = []

instance toJSArgs (a,b) | gToJS{|*|} a & gToJS{|*|} b
where toJSArgs (a,b) = [toJS a, toJS b]

instance toJSArgs (a,b,c) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c
where toJSArgs (a,b,c) = [toJS a, toJS b, toJS c]

instance toJSArgs (a,b,c,d) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d
where toJSArgs (a,b,c,d) = [toJS a, toJS b, toJS c, toJS d]

instance toJSArgs (a,b,c,d,e) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d & gToJS{|*|} e
where toJSArgs (a,b,c,d,e) = [toJS a, toJS b, toJS c, toJS d, toJS e]

instance toJSArgs (a,b,c,d,e,f) | gToJS{|*|} a & gToJS{|*|} b & gToJS{|*|} c & gToJS{|*|} d & gToJS{|*|} e & gToJS{|*|} f
where toJSArgs (a,b,c,d,e,f) = [toJS a, toJS b, toJS c, toJS d, toJS e, toJS f]

(.$) infixl 2 :: !JSFun !b !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs b
(.$) f args w = (eval_js_with_return_value call, w)
where
	call = toString f+++"("+++join "," [toString a \\ a <- toJSArgs args]+++")"

(.$!) infixl 2 :: !JSFun !b !*JSWorld -> *JSWorld | toJSArgs b
(.$!) f args w = case eval_js call of
	True -> w
where
	call = toString f+++"("+++join "," [toString a \\ a <- toJSArgs args]+++")"

jsNew :: !String !a !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs a
jsNew cons args w = (eval_js_with_return_value call, w)
where
	call = "new "+++cons+++"("+++join "," [toString a \\ a <- toJSArgs args]+++")"

jsDelete :: !JSVal !*JSWorld -> *JSWorld
jsDelete v w = case eval_js ("delete "+++toString v) of
	True -> w

jsEmptyObject :: !*JSWorld -> *(!JSVal, !*JSWorld)
jsEmptyObject w = (eval_js_with_return_value "{}", w)

jsGlobal :: !String -> JSVal
jsGlobal s = JSVar s

jsWrapFun :: !({!JSVal} *JSWorld -> *JSWorld) !*JSWorld -> *(!JSFun, !*JSWorld)
jsWrapFun f world = (cast (share casting_f), world)
where
	casting_f :: !{!a} !*JSWorld -> *JSWorld
	casting_f args w = f {cast_value_from_js a \\ a <-: args} w

wrapInitUIFunction :: !(JSVal *JSWorld -> *JSWorld) -> {!JSVal} -> *JSWorld -> *JSWorld
wrapInitUIFunction f = \args
	| size args<>1
		-> abort ("failed to get iTasks component from JavaScript (args.size="+++toString (size args)+++")\n")
		-> case cast_value_from_js args.[0] of
			r=:(JSRef _)
				-> f r
				-> abort "failed to get iTasks component from JavaScript\n"

jsDeserializeGraph :: !String !*JSWorld -> *(!.a, !*JSWorld)
jsDeserializeGraph s w = (deserialize s, w)
where
	deserialize :: !String -> .a
	deserialize _ = code {
		instruction 5
	}

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

addJSFromUrl :: !String !(Maybe JSFun) !*JSWorld -> *JSWorld
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

jsTrace :: !a .b -> .b | toString a
jsTrace s x = case eval_js ("console.log('"+++toString s+++"')") of
	True -> x

eval_js :: !String -> Bool
eval_js s = code {
	instruction 1
	pop_a 1
	pushB TRUE
}

eval_js_with_return_value :: !String -> JSVal
eval_js_with_return_value s = cast_value_from_js (eval s)
where
	eval :: !String -> a
	eval _ = code {
		instruction 2
	}

cast_value_from_js :: !a -> JSVal
cast_value_from_js x = case cast_value_from_js` x of
	JSArray arr -> JSArray {cast_value_from_js x \\ x <-: arr} // TODO exploit uniqueness
	v           -> v

cast_value_from_js` :: !a -> JSVal
cast_value_from_js` _ = code {
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
	eq_desc REAL 0 0
	jmp_true return_real
	eq_desc _ARRAY_ 0 0
	jmp_true return_array
	print "cast_value_from_js: return type unknown:\n"
	.d 1 0
	jsr _print_graph
	.o 0 0
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
:return_real
	repl_r_args 0 1
	fill_r e_iTasks.UI.JS.Interface_kJSReal 0 1 0 0 0
	pop_b 1
	jmp return
:return_string
	fill_r e_iTasks.UI.JS.Interface_kJSString 1 0 1 0 0
	pop_a 1
	jmp return
:return_array
	fill_r e_iTasks.UI.JS.Interface_kJSArray 1 0 1 0 0
	pop_a 1
	jmp return
:return_ref
	pushI 1 | for shiftr%
	repl_r_args 0 1
	push_b 0
	pushI 1
	and%
	pushI 1
	eqI
	jmp_true return_clean_ref
	shiftr%
	fill_r e_iTasks.UI.JS.Interface_kJSRef 0 1 0 0 0
	pop_b 1
	jmp return
:return_clean_ref
	shiftr%
	fill_r e_iTasks.UI.JS.Interface_kJSCleanRef 0 1 0 0 0
	pop_b 1
	jmp return
:return
}

share :: !a -> JSVal
share x = JSCleanRef (get_shared_value_index x)
where
	get_shared_value_index :: !a -> Int
	get_shared_value_index _ = code {
		pushI 0 | to return the result
		instruction 3
		pop_a 1
	}

cast :: !.a -> .b
cast _ = code {
	no_op
}
