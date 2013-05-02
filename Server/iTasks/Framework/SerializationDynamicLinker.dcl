definition module iTasks.Framework.SerializationDynamicLinker

import JSON
from Error import ::MaybeError, ::MaybeErrorString
from iTasks.Framework.Store import ::StoreFormat

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
