implementation module iTasks.UI.JS.Encoding

import iTasks.UI.JS.Interface
import Text.JSON
import Text.Encodings.Base64
import StdMisc, StdArray
import dynamic_string

/*
When we encode values on the server we directly encode to the representation used by the Sapl run-time such that
additional decoding on the client is not longer necessary.
*/
/*
* Format of sapl representation:
ADTs:
[<index of cons>,<name of cons>, <args ...>]
Records (same as ADT, record type with an underscore prepended is used as cons name):
[0, '_' + <name of type>, <args ...>]
Primitives:
[<boxed primitive>]
Thunks:
[<function ref>,[<args ...>]]
*/

encodeOnServer :: !a -> JSONNode | JSEncode{|*|} a
encodeOnServer x = case JSEncode{|*|} x of 
	[node] = node
	_      = JSONError

decodeOnClient :: !(JSVal a) !*JSWorld -> *(!a, !*JSWorld)
decodeOnClient val world = undef //Implemented in iTasks/Sapl FFI

generic JSEncode t :: !t -> [JSONNode]
JSEncode{|Int|} x = [JSONArray [JSONInt x]]
JSEncode{|Real|} x = [JSONArray [JSONReal x]]
JSEncode{|Char|} x = [JSONArray [JSONString {x}]]
JSEncode{|Bool|} x = [JSONArray [JSONBool x]]
JSEncode{|String|} x = [JSONArray [JSONString x]]
JSEncode{|UNIT|} (UNIT) = []
JSEncode{|PAIR|} fx fy (PAIR x y) = fx x ++ fy y
where
	(++) infixr 5::![.a] !u:[.a] -> u:[.a]
	(++) [hd:tl]	list	= [hd:tl ++ list]
	(++) nil 		list	= list

JSEncode{|EITHER|} fx fy (LEFT x) = fx x
JSEncode{|EITHER|} fx fy (RIGHT y) = fy y
JSEncode{|OBJECT|} fx (OBJECT x) = fx x
JSEncode{|CONS of {gcd_name,gcd_index}|} fx (CONS x) = [JSONArray [JSONInt gcd_index, JSONString gcd_name : fx x]]
JSEncode{|RECORD of {grd_name}|} fx (RECORD x) = [JSONArray [JSONInt 0, JSONString ("_" +++ grd_name) : fx x]]
JSEncode{|FIELD|} fx (FIELD x) = fx x
JSEncode{|{}|} fx x = [JSONArray (flatten [fx e \\ e <-: x])]
JSEncode{|{!}|} fx x = [JSONArray (flatten [fx e \\ e <-: x])]
JSEncode{|(->)|} fx fy x = [JSONString "error"]

derive JSEncode [],(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,)
