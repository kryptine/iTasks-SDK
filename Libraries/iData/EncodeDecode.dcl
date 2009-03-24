definition module EncodeDecode

// provides the encoding and decoding of information between browser and the executable

encodeString 				:: !String  -> String
decodeString 				:: !String -> *String

urlEncode 					:: !String ->  String
urlDecode 					:: !String -> *String