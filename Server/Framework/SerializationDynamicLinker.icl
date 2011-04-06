implementation module SerializationDynamicLinker

import StdEnv

import Base64
import JSON
import StdMisc
import Maybe
from Serialization import qualified serialize, deserialize, serializeDynamic, deserializeDynamic

serialize :: !a -> String | TC a
serialize value = 'Serialization'.serialize value

deserialize	:: !String -> a | TC a
deserialize str = 
	case 'Serialization'.deserialize str of
		Just value = value
		Nothing = abort "SerializationDynamicLink module, deserializeDynamic: deserialization failed"

serializeDynamic :: !Dynamic -> String
serializeDynamic dyn = 'Serialization'.serializeDynamic dyn

deserializeDynamic :: !String -> Dynamic
deserializeDynamic str = 
	case 'Serialization'.deserialize str of
		Just dyn = dyn
		Nothing  = abort "SerializationDynamicLink module, deserializeDynamic: deserialization failed"

JSONEncode{|Dynamic|} dyn = [JSONString (base64Encode (serializeDynamic dyn))]
JSONEncode{|(->)|} _ _ f = [JSONString (base64Encode (serialize f))]

JSONDecode{|Dynamic|} [JSONString string:c]	= (Just (deserializeDynamic {s` \\ s` <-: base64Decode string}), c)
JSONDecode{|Dynamic|} c						= (Nothing, c)

//JSONDecode{|(->)|} _ _ [JSONString string:c]	= (Just (fst(deserialize {s` \\ s` <-: base64Decode string})) ,c) 
//JSONDecode{|(->)|} _ _ c						= (Nothing,c)
