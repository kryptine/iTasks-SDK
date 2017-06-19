implementation module iTasks.UI.JS.Encoding

import iTasks.UI.JS.Interface
import Text.JSON
import Text.Encodings.Base64
import StdMisc, StdArray
import dynamic_string

encodeOnServer :: !a -> JSONNode | JSEncode{|*|} a
encodeOnServer x = case JSEncode{|*|} True x of 
	[node] = node
	_      = JSONError

decodeOnClient :: !(JSVal a) !*JSWorld -> *(!a, !*JSWorld)
decodeOnClient _ _ = undef //This implementation is hardcoded in the client framework

generic JSEncode t :: !Bool !t -> [JSONNode]
JSEncode{|Int|} _ x = [JSONInt x]
JSEncode{|Real|} _ x = [JSONReal x]
JSEncode{|Char|} _ x = [JSONString {x}]
JSEncode{|Bool|} _ x = [JSONBool x]
JSEncode{|String|} _ x = [JSONString x]
JSEncode{|UNIT|} _ (UNIT) = []
JSEncode{|PAIR|} fx fy _ (PAIR x y) = fx False x ++ fy False y
where
	(++) infixr 5::![.a] !u:[.a] -> u:[.a]
	(++) [hd:tl]	list	= [hd:tl ++ list]
	(++) nil 		list	= list
JSEncode{|EITHER|} fx fy _ (LEFT x) = fx False x
JSEncode{|EITHER|} fx fy _ (RIGHT y) = fy False y
JSEncode{|OBJECT|} fx _ (OBJECT x) = fx False x
JSEncode{|CONS of {gcd_name}|} fx _ (CONS x)
  = [JSONArray [JSONString gcd_name : fx False x]]
JSEncode{|RECORD of {grd_fields}|} fx _ (RECORD x)
  = [JSONObject [(name, o) \\ o <- fx False x & name <- grd_fields | isNotNull o]]
  where
  isNotNull :: !JSONNode -> Bool
  isNotNull JSONNull = False
  isNotNull _ = True

JSEncode{|FIELD|} fx _ (FIELD x) = fx True x
JSEncode{|[]|} fx _ x = [JSONArray (flatten [fx False e \\ e <- x])]
JSEncode{|(,)|} fx fy _ (x,y) = [JSONArray (fx False x ++ fy False y)]
JSEncode{|(,,)|} fx fy fz _ (x,y,z) = [JSONArray (fx False x ++ fy False y ++ fz False z)]
JSEncode{|(,,,)|} fx fy fz fi _ (x,y,z,i) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i)]
JSEncode{|(,,,,)|} fx fy fz fi fj _ (x,y,z,i,j) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j)]
JSEncode{|(,,,,,)|} fx fy fz fi fj fk _ (x,y,z,i,j,k) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k)]
JSEncode{|(,,,,,,)|} fx fy fz fi fj fk fl _ (x,y,z,i,j,k,l) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k ++ fl False l)]
JSEncode{|(,,,,,,,)|} fx fy fz fi fj fk fl fm _ (x,y,z,i,j,k,l,m) = [JSONArray (fx False x ++ fy False y ++ fz False z ++ fi False i ++ fj False j ++ fk False k ++ fl False l ++ fm False m)]
JSEncode{|{}|} fx _ x = [JSONArray (flatten [fx False e \\ e <-: x])]
JSEncode{|{!}|} fx _ x = [JSONArray (flatten [fx False e \\ e <-: x])]
JSEncode{|Maybe|} fx inField (Just x) = if inField (fx False x) [JSONArray [JSONString "Just" : fx False x]]
JSEncode{|Maybe|} fx inField Nothing = if inField [JSONNull] [JSONArray [JSONString "Nothing"]]
JSEncode{|JSONNode|} _ node = [node]

JSEncode{|(->)|} _ _ _ f = [JSONArray [JSONString "_FUNCTION_", JSONString (base64URLEncode (copy_to_string f))]]

