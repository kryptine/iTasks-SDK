definition module SerializationGraphCopy

from JSON import generic JSONEncode, generic JSONDecode, ::JSONNode
from Error import ::MaybeError, ::MaybeErrorString
from Store import ::StoreFormat
from Maybe import ::Maybe

serialize :: !a -> *String
deserialize	:: !*String -> MaybeErrorString a
serializeDynamic :: !Dynamic -> *String
deserializeDynamic :: !*String -> Dynamic

derive JSONEncode Dynamic, (->)
derive JSONDecode Dynamic, (->)

dynamicJSONEncode :: !a -> [JSONNode]
dynamicJSONDecode :: !JSONNode -> Maybe a

defaultStoreFormat :: StoreFormat

serializationModule :: String