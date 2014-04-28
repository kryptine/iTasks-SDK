definition module iTasks.Framework.Serialization

from Text.JSON import generic JSONEncode, generic JSONDecode, ::JSONNode
from Data.Error import ::MaybeError, ::MaybeErrorString
from Data.Maybe import ::Maybe

serialize :: !a -> *String
deserialize	:: !*String -> MaybeErrorString a
serializeDynamic :: !Dynamic -> *String
deserializeDynamic :: !*String -> Dynamic

derive JSONEncode Dynamic, (->)
derive JSONDecode Dynamic, (->)

//Check if a JSON serialization contains encoded functions or dynamics
functionFree		:: !JSONNode -> Bool

dynamicJSONEncode	:: !a -> JSONNode
dynamicJSONDecode	:: !JSONNode -> Maybe a
