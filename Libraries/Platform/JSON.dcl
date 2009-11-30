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

//Token type which is the intermediary representation during JSON parsing
:: Token	= TokenInt Int
			| TokenReal	Real
			| TokenString String
			| TokenBool	Bool
			| TokenNull
			| TokenBracketOpen
			| TokenBracketClose
			| TokenBraceOpen
			| TokenBraceClose
			| TokenName	String
			| TokenColon
			| TokenComma
			| TokenWhitespace String
			| TokenFail	

:: JSON = JSON String	//String which is already in JSON encoding

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
* Escapes a string for manual JSON construction
*
* @param The unescaped string
* @return A properly escaped string
*/
jsonEscape	:: String	-> String

/**
* Lexer for Json-Strings. This function is used by the JSONTree-module.
**/
lex :: String Int [Token] -> (Int, [Token])

/**
* Generic encoding function. This function should not be used
* directly but always through the toJSON function. It must be derived
* for each type you want to encode in JSON format.
*/
generic JSONEncode t :: t [String] -> [String]
derive  JSONEncode Int, Real, Char, Bool, String, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, [], (,), {}, {!}, Maybe, JSON
/**
* Generic decoding function. This function should not be used
* directly, but always through the fromJSON function. It must be derived
* for each type you want to parse from JSON format.
*/
generic JSONDecode t :: [Token] -> (Maybe t,[Token])
derive  JSONDecode Int, Real, Char, Bool, String, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, [], (,), {}, {!}, Maybe