definition module iTasks.UI.JS.Encoding
/**
* This module provides encoding/decoding functions for communicating values efficiently
* between an itasks server application and its client (webbrowser).
* It uses an encoding of Clean values as JSON that can be decoded natively in javascript
*/
import iTasks.UI.JS.Interface
import StdGeneric
from Text.JSON import :: JSONNode

//Sending values server -> client
encodeOnServer :: !a -> JSONNode | JSEncode{|*|} a
decodeOnClient :: !(JSVal a) !*JSWorld -> *(!a, !*JSWorld)

//Sending value client -> server
//encodeOnClient :: !a *JSWorld -> (!JSVal a, !*JSWorld)
//decodeOnServer :: !JSONNode -> (Maybe a)

generic JSEncode t :: !t -> [JSONNode]
derive  JSEncode Int, Real, Char, Bool, String, UNIT, [],
	(,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), {}, {!}, (->),
    EITHER, OBJECT

JSEncode{|CONS of {gcd_name,gcd_index}|} fx (CONS x) = [JSONArray [JSONInt gcd_index, JSONString gcd_name : fx x]]
JSEncode{|RECORD of {grd_name}|} fx (RECORD x) = [JSONArray [JSONInt 0, JSONString ("_" +++ grd_name) : fx x]]

JSEncode{|FIELD|} fx (FIELD x) = fx x

JSEncode{|PAIR|} fx fy (PAIR x y) = fx x ++ fy y
where
    (++) infixr 5::![.a] !u:[.a] -> u:[.a]
    (++) [hd:tl]    list    = [hd:tl ++ list]
    (++) nil        list    = list
