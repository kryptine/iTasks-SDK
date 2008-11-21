definition module EncodeDecode

// provides the encoding and decoding of information between browser and the executable
// (c) 2005 - MJP

import NWorld

encodeString 				:: !String  -> String
decodeString 				:: !String -> *String

urlEncode 					:: !String ->  String
urlDecode 					:: !String -> *String