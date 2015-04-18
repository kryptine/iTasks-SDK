definition module iTasks._Framework.SerializationDynamicLinker

from JSON import :: JSONNode
from Error import ::MaybeError, ::MaybeErrorString
from iTasks._Framework.Store import ::StoreFormat

serialize :: !a -> String | TC a
deserialize	:: !String -> MaybeErrorString a | TC a
serializeDynamic :: !Dynamic -> String
deserializeDynamic :: !String -> MaybeErrorString Dynamic

derive JSONEncode Dynamic, (->)
derive JSONDecode Dynamic, (->)

dynamicJSONEncode :: !a -> [JSONNode]
dynamicJSONDecode :: !JSONNode -> Maybe a

defaultStoreFormat :: StoreFormat

serializationModule :: String
