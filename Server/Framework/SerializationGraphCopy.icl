implementation module SerializationGraphCopy

import StdEnv
import dynamic_string

import Base64
import JSON

serialize :: !a -> *String
serialize value = copy_to_string value

deserialize	:: !*String -> a
deserialize str = let (a,_) = (copy_from_string str) in a
				
serializeDynamic :: !Dynamic -> *String
serializeDynamic dyn = dynamic_to_string dyn

deserializeDynamic :: !*String -> Dynamic
deserializeDynamic str = string_to_dynamic str
/*
JSONEncode{|Dynamic|} dyn = [JSONString (base64Encode (serializeDynamic dyn))]
JSONEncode{|(->)|} _ _ f = [JSONString (base64Encode (serialize f))]

JSONDecode{|Dynamic|} [JSONString string:c]	= (Just (deserializeDynamic {s` \\ s` <-: base64Decode string}), c)
JSONDecode{|Dynamic|} c						= (Nothing, c)

JSONDecode{|(->)|} _ _ [JSONString string:c]	= (Just (fst(deserialize {s` \\ s` <-: base64Decode string})) ,c) 
JSONDecode{|(->)|} _ _ c						= (Nothing,c)
*/

JSONEncode{|Dynamic|} dyn = [JSONString (base64Encode (serializeDynamic dyn))]
JSONEncode{|(->)|} _ _ f = [JSONString (base64Encode (serialize f))]

JSONDecode{|Dynamic|} [JSONString string:c]	= (Just (deserializeDynamic {s` \\ s` <-: base64Decode string}), c)
JSONDecode{|Dynamic|} c						= (Nothing, c)

JSONDecode{|(->)|} _ _ [JSONString string:c]	= (Just (fst(copy_from_string {s` \\ s` <-: base64Decode string})) ,c) 
JSONDecode{|(->)|} _ _ c						= (Nothing,c)
