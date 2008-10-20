definition module JSON

import StdGeneric, StdMaybe

toJSON		:: a		-> String	| JSONEncode{|*|} a
//fromJSON	:: String	-> Maybe a	| JSONDecode{|*|} a

generic JSONEncode t :: t [String] -> [String]
derive  JSONEncode Int, Char, Bool, String, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, [], {}, {!}

//generic JSONDecode t :: String -> Maybe t
//derive  JSONDecode Int, Char, Bool, String, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, [], {}, {!}
