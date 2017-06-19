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

generic JSEncode t :: !Bool !t -> [JSONNode]
derive  JSEncode Int, Real, Char, Bool, String, UNIT, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), {}, {!}, (->), Maybe, JSONNode,
    EITHER, CONS of {gcd_name}, OBJECT

JSEncode{|RECORD of {grd_fields}|} fx _ (RECORD x)
  = [JSONObject [(name, o) \\ o <- fx False x & name <- grd_fields | isNotNull o]]
  where
  isNotNull :: !JSONNode -> Bool
  isNotNull JSONNull = False
  isNotNull _ = True

JSEncode{|FIELD|} fx _ (FIELD x) = fx True x

JSEncode{|PAIR|} fx fy _ (PAIR x y) = fx False x ++ fy False y
where
    (++) infixr 5::![.a] !u:[.a] -> u:[.a]
    (++) [hd:tl]    list    = [hd:tl ++ list]
    (++) nil        list    = list
