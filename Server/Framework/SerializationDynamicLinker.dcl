definition module SerializationDynamicLinker

import JSON

serialize :: !a -> String | TC a
deserialize	:: !String -> a | TC a
serializeDynamic :: !Dynamic -> String
deserializeDynamic :: !String -> Dynamic

derive JSONEncode Dynamic, (->)
derive JSONDecode Dynamic, (->)
