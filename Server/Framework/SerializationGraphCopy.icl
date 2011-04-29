implementation module SerializationGraphCopy

import StdEnv
import dynamic_string

import Base64
import Error
import JSON
import Maybe

from Store import ::StoreFormat(..)

serialize :: !a -> *String
serialize value = copy_to_string value

deserialize	:: !*String -> MaybeErrorString a
deserialize str = let (a,_) = (copy_from_string str) in (Ok a)
				
serializeDynamic :: !Dynamic -> *String
serializeDynamic dyn = dynamic_to_string dyn

deserializeDynamic :: !*String -> Dynamic
deserializeDynamic str = string_to_dynamic str

JSONEncode{|Dynamic|} dyn = [JSONString (base64Encode (serializeDynamic dyn))]
JSONEncode{|(->)|} _ _ f = [JSONString (base64Encode (serialize f))]

JSONDecode{|Dynamic|} [JSONString string:c]	= (Just (deserializeDynamic (base64Decode string)), c)
JSONDecode{|Dynamic|} c						= (Nothing, c)

JSONDecode{|(->)|} _ _ [JSONString string:c]	= (Just (fst(copy_from_string {s` \\ s` <-: base64Decode string})) ,c) 
JSONDecode{|(->)|} _ _ c						= (Nothing,c)

dynamicJSONEncode :: !a -> [JSONNode]
dynamicJSONEncode f = [JSONString (base64Encode (copy_to_string f))]

dynamicJSONDecode :: !JSONNode -> Maybe a
dynamicJSONDecode (JSONString str)	= Just (fst (copy_from_string (base64Decode str)))
dynamicJSONDecode _					= Nothing

defaultStoreFormat :: StoreFormat
defaultStoreFormat = SFPlain

serializationModule :: String
serializationModule = "SerializationGraphCopy"
