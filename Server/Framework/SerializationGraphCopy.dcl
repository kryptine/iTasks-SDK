definition module SerializationGraphCopy

from JSON import generic JSONEncode, generic JSONDecode, ::JSONNode
from Maybe import ::Maybe

serialize :: !a -> *String
deserialize	:: !*String -> a
serializeDynamic :: !Dynamic -> *String
deserializeDynamic :: !*String -> Dynamic

derive JSONEncode Dynamic, (->)
derive JSONDecode Dynamic, (->)
