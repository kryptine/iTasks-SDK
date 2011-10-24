definition module SerializationGraphCopy

from JSON import generic JSONEncode, generic JSONDecode, ::JSONNode
from Error import ::MaybeError, ::MaybeErrorString
from Maybe import ::Maybe
from Store import :: StoreFormat

serialize :: !a -> *String
deserialize	:: !*String -> MaybeErrorString a
serializeDynamic :: !Dynamic -> *String
deserializeDynamic :: !*String -> Dynamic

derive JSONEncode Dynamic, (->)
derive JSONDecode Dynamic, (->)

dynamicJSONEncode :: !a -> [JSONNode]
dynamicJSONDecode :: !JSONNode -> Maybe a

serializationModule :: String
defaultStoreFormat :: StoreFormat