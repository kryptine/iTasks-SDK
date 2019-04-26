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

	| JSUnused // used as always-False pattern match; see comments on abort_with_node.

:: JSObjectElement =
	{ key :: !String
	, val :: !JSVal
	}

// TODO optimise this by first computing the size
instance toString JSVal
where
	toString v = let s = toS v in if (size s<0) (abort_with_node v) s
	where
		toS :: !JSVal -> String
		toS v = case v of
			JSInt i -> toString i
			JSBool b -> if b "true" "false"
			JSString s -> "'"+++escape_js_string s+++"'"
			JSReal r -> toString r

			JSVar v -> v
			JSNull -> "null"
			JSUndefined -> "undefined"
			JSTypeOf v -> "typeof "+++toS v

			JSObject elems -> foldl (+++) "{" [key+++":"+++toS val+++"," \\ {key,val} <-: elems] +++ "}"
			JSArray elems -> foldl (+++) "[" [toS v+++"," \\ v <-: elems] +++ "]"

			JSSel obj attr -> toS obj+++"["+++toS attr+++"]"
			JSSelPath obj path -> toS obj+++"."+++path

			JSRef i -> "ABC.js["+++toString i+++"]"
			JSCleanRef i -> "ABC.ap("+++toString i+++")"

escape_js_string :: !String -> String
escape_js_string s
# escaped_len = escaped_size (size s-1) s 0
| escaped_len == size s
	= s
	= copy_chars s 0 (createArray escaped_len '\0') 0
where
	escaped_size :: !Int !String !Int -> Int
	escaped_size -1 s n = n
	escaped_size i s n
	| s.[i] < '\x20' = escaped_size (i-1) s (n+4)
	| s.[i] == '\''  = escaped_size (i-1) s (n+2)
	| otherwise      = escaped_size (i-1) s (n+1)

	copy_chars :: !{#Char} !Int !*{#Char} !Int -> .{#Char}
	copy_chars src si dst di
	| si >= size src = dst
	# c = src.[si]
	| c < '\x20'
		# c = toInt c
		# dst = {dst & [di]='\\', [di+1]='x', [di+2]=hex (c>>4), [di+3]=hex (c bitand 0x0f)}
		= copy_chars src (si+1) dst (di+4)
	| c == '\''
		# dst = {dst & [di]='\\', [di+1]='\''}
		= copy_chars src (si+1) dst (di+2)
	| otherwise
		# dst = {dst & [di]=c}
		= copy_chars src (si+1) dst (di+1)
	where
		hex i = hex_chars.[i]

hex_chars :: {#Char}
hex_chars =: "0123456789abcdef"

jsMakeCleanReference :: a !JSVal -> JSVal
jsMakeCleanReference x attach_to = share attach_to x

jsGetCleanReference :: !JSVal !*JSWorld -> *(!Maybe b, !*JSWorld)
jsGetCleanReference v w = case eval_js_with_return_value (toString v) of
	JSCleanRef i -> (Just (fetch i), w)
	_            -> if (1==1) (Nothing, w) (abort_with_node v)
where
	fetch :: !Int -> a
	fetch _ = code {
		create
		instruction 5
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
gToJS{|(,)|} fa fb (a,b) = JSArray {fa a,fb b}
gToJS{|(,,)|} fa fb fc (a,b,c) = JSArray {fa a,fb b,fc c}
gToJS{|(,,,)|} fa fb fc fd (a,b,c,d) = JSArray {fa a,fb b,fc c,fd d}
gToJS{|(,,,,)|} fa fb fc fd fe (a,b,c,d,e) = JSArray {fa a,fb b,fc c,fd d,fe e}
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
	= case eval_js_with_return_value (toString js) of
		JSUnused -> abort_with_node js
		result   -> (result, w)
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

		JSRef _      -> (True,v)
		JSCleanRef _ -> (True,v)

		_            -> (False,v)

(.=) infixl 1 :: !JSVal !b !*JSWorld -> *JSWorld | gToJS{|*|} b
(.=) sel v w
# v = toJS v
= case set_js (toString sel) (toString v) of
	True  -> w
	False -> abort_with_node (sel,v)

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
(.$) f args w
# args = toJSArgs args
= case eval_js_with_return_value (call args) of
	JSUnused -> abort_with_node (f,args)
	result   -> (result, w)
where
	call args = toString f+++"("+++join "," [toString a \\ a <- args]+++")"

(.$!) infixl 2 :: !JSFun !b !*JSWorld -> *JSWorld | toJSArgs b
(.$!) f args w
# args = toJSArgs args
= case eval_js (call args) of
	True  -> w
	False -> abort_with_node (f,args)
where
	call args = toString f+++"("+++join "," [toString a \\ a <- args]+++")"

jsNew :: !String !a !*JSWorld -> *(!JSVal, !*JSWorld) | toJSArgs a
jsNew cons args w
# args = toJSArgs args
= case eval_js_with_return_value (call args) of
	JSUnused -> abort_with_node args
	result   -> (result, w)
where
	call args = "new "+++cons+++"("+++join "," [toString a \\ a <- args]+++")"

jsDelete :: !JSVal !*JSWorld -> *JSWorld
jsDelete v w = case eval_js ("delete "+++toString v) of
	True  -> w
	False -> abort_with_node v

jsEmptyObject :: !*JSWorld -> *(!JSVal, !*JSWorld)
jsEmptyObject w = (eval_js_with_return_value "{}", w)

jsGlobal :: !String -> JSVal
jsGlobal s = JSVar s

jsWrapFun :: !({!JSVal} *JSWorld -> *JSWorld) !JSVal !*JSWorld -> *(!JSFun, !*JSWorld)
jsWrapFun f attach_to world = (cast (share attach_to \(JSArray args) w -> f args w), world)

wrapInitUIFunction :: !(JSVal *JSWorld -> *JSWorld) -> {!JSVal} -> *JSWorld -> *JSWorld
wrapInitUIFunction f = cast init
where
	init :: !{!JSVal} !*JSWorld -> *JSWorld
	init args w
	# is_initialized = get_arg args.[1] <> 1
	| not is_initialized && init val = abort "internal error in wrapInitUIFunction\n"
	# js_ref = JSRef (get_arg args.[0])
	= f js_ref w
	where
		// This function ensures that the client knows the addresses of some
		// of the constructors which it needs to know.
		init :: !{!JSVal} -> Bool
		init _ = code {
			instruction 7
			pop_a 1
			pushB FALSE
		}

		val :: {!JSVal}
		val =
			{ JSInt 0
			, JSBool True
			, JSString ""
			, JSReal 0.0
			, JSNull
			, JSUndefined
			, JSArray {}
			, JSRef 0
			, JSCleanRef 0
			}

		get_arg :: !a -> Int
		get_arg _ = code {
			repl_r_args 0 1
		}

jsDeserializeGraph :: !String !*JSWorld -> *(!.a, !*JSWorld)
jsDeserializeGraph s w = (deserialize s, w)
where
	deserialize :: !String -> .a
	deserialize _ = code {
		instruction 6
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
	True  -> w
	False -> abort_with_node mbCallback
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
	True  -> x
	False -> abort_with_node s // just in case it is a JSVal

set_js :: !String !String -> Bool
set_js var val = code {
	instruction 1
	pop_a 2
	pushB TRUE
}

eval_js :: !String -> Bool
eval_js s = code {
	instruction 2
	pop_a 1
	pushB TRUE
}

eval_js_with_return_value :: !String -> JSVal
eval_js_with_return_value s = code {
	instruction 3
	fill_a 0 1
	pop_a 1
}

share :: !JSVal !a -> JSVal
share attach_to x = case attach_to of
	JSRef r -> JSCleanRef (get_shared_value_index x r)
	_       -> abort "when sharing a value from Clean to JS it must be linked to an iTasks component\n"
where
	get_shared_value_index :: !a !Int -> Int
	get_shared_value_index _ _ = code {
		instruction 4
		pop_a 1
	}

cast :: !.a -> .b
cast _ = code {
	no_op
}

// This function is meant to be called with a (node containing) JSVal(s) as its
// argument, and then ensures that a references to that value(s) remain
// reachable by the garbage collector. This is needed when a JSVal is converted
// to a string and then sent to JavaScript. If it contains a JSRef, the
// reference must not be garbage collected, but computing the string to send to
// JavaScript may trigger a garbage collection after the JSRef has been
// visited. This function, when used properly, makes sure that the JSRef stays
// in memory until after the call to JavaScript.
abort_with_node :: !a -> .b
abort_with_node _ = abort "abort_with_node\n"
