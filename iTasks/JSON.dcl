definition module JSON
/*
* This module provides functions to encode and decode any Clean data type
* to JSON format. It provides two generic functions JSONEncode and JSONDecode
* which must be derived for concrete types. Then toJSON and fromJSON may be
* used to convert any value to and from JSON.
*
* For more info about JSON see: http://www.json.org/
*/

import StdGeneric, StdMaybe

//Abstract token type which is the intermediary representation during JSON parsing
:: Token
/**
* Encodes any value to JSON format.
* @param The value to encode
* @return The JSON encoded value
*/
toJSON		:: a		-> String	| JSONEncode{|*|} a
/**
* Tries to parse a JSON encoded string.
* When parsing fails, the result is Nothing.
*
* @param The JSON encoded input
* @return Just the result, when parsing succeeds
*/
fromJSON	:: String	-> Maybe a	| JSONDecode{|*|} a
/**
* Generic encoding function. This function should not be used
* directly but always through the toJSON function. It must be derived
* for each type you want to encode in JSON format.
*/
generic JSONEncode t :: t [String] -> [String]
derive  JSONEncode Int, Real, Char, Bool, String, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, [], {}, {!}, Maybe
/**
* Generic decoding function. This function should not be used
* directly, but always through the fromJSON function. It must be derived
* for each type you want to parse from JSON format.
*/
generic JSONDecode t :: [Token] -> (Maybe t,[Token])
derive  JSONDecode Int, Real, Char, Bool, String, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, [], {}, {!}, Maybe