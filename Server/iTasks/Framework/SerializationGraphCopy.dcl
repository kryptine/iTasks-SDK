definition module iTasks.Framework.SerializationGraphCopy

from Text.JSON import generic JSONEncode, generic JSONDecode, ::JSONNode
from Data.Error import ::MaybeError, ::MaybeErrorString
from Data.Maybe import ::Maybe
from iTasks.Framework.Store import :: StoreFormat

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

serializationModule	:: String
defaultStoreFormat	:: StoreFormat
