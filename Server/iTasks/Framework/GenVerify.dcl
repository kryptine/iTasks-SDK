definition module iTasks.Framework.GenVerify

import iTasks.Framework.GenUpdate

:: VerifyOptions =
	{ optional		:: !Bool
	, disabled		:: !Bool
	}

generic gVerify a :: !VerifyOptions (MaskedValue a) -> Verification

derive gVerify UNIT, PAIR, EITHER, OBJECT, CONS of {gcd_arity}, RECORD of {grd_arity}, FIELD
derive gVerify Int, Real, Char, Bool, String, [], (,), (,,),(,,,),(->), Dynamic
derive gVerify Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

/**
* Verify a value.
*/
verifyValue :: !a -> Verification | gVerify{|*|} a
/**
* Verify a form based on the value and its update mask.
*/
verifyMaskedValue :: !(MaskedValue a) -> Verification | gVerify{|*|} a
/**
* Based on the verification of a value, determine if it is valid.
* A compound value is valid if the verification contains no invalid parts.
*/
isValid :: !Verification -> Bool

/**
* Verifies a value which is always valid.
* No hint message is shown.
*
*/
alwaysValid :: !(MaskedValue a) -> Verification
/**
* Verifies a value which is always valid if filled in (e.g. a basic value).
*
*/
simpleVerify :: !VerifyOptions !(MaskedValue a) -> Verification
/**
* Verifies a custom ADT.
* For this ADT also a custom visualization has to be implemented.
* There is only one verify mask for the entire value.
* 
* @param	The predicate function
* @param	A function for error message generation, in case the predicate fails
* @param	The actual value
*/
customVerify :: !(a -> Bool) !(a -> String) !VerifyOptions (MaskedValue a) -> Verification
