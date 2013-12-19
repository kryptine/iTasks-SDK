definition module iTasks.Framework.DynamicUtil

import Text.JSON

//derive JSONEncode TypeCode
//derive JSONDecode TypeCode

unpackType 		:: !Dynamic -> TypeCode
//typeCodeName 	:: !TypeCodeConstructor -> String
//unsafeCreateDynamic :: !a !TypeCode -> Dynamic