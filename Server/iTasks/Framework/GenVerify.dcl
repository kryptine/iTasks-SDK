definition module iTasks.Framework.GenVerify

import iTasks.Framework.GenUpdate

instance GenMask VerifyMask //TODO: Remove once no longer needed

:: ErrorMessage = BlankError			//A required field is missing
				| FormatError !String	//The entered value does not have the right format
				
:: HintMessage :== String
:: Optional :== Bool

:: VerifyMask = VMUntouched !(Maybe HintMessage) !Optional ![VerifyMask] 
			  | VMValid !(Maybe HintMessage) ![VerifyMask]
			  | VMValidWithState !(Maybe HintMessage) ![VerifyMask] !JSONNode
			  | VMInvalid !ErrorMessage ![VerifyMask]
			  | VMInvalidWithState !ErrorMessage ![VerifyMask] !JSONNode
			  
:: VerifyOptions =
	{ optional		:: !Bool
	, disabled		:: !Bool
	}

generic gVerify a :: !(Maybe a) ![InteractionMask] !VerifyOptions -> ([VerifyMask],[InteractionMask])

derive gVerify UNIT, PAIR, EITHER, OBJECT of {gtd_num_conses}, CONS of {gcd_arity}, RECORD of {grd_arity}, FIELD
derive gVerify Int, Real, Char, Bool, String, [], (,), (,,),(,,,),(->), Dynamic

derive gVerify Maybe, Either, Void, Map, JSONNode, HtmlTag, Timestamp

/**
* Verify a form based on the value and its update mask.
*/
verifyMaskedValue :: !a !InteractionMask -> VerifyMask | gVerify{|*|} a

/**
* Verify a value (a form which is filled in completely).
*/
verifyValue :: !a -> Bool | gVerify{|*|} a

/**
* Based on the verify mask of a value, determine if it is valid.
* A value is valid if the verify mask contains no invalid fields and all untouched fields are optional
*/
isValidMask :: !VerifyMask -> Bool

/**
* Verifies a value which is always valid.
* No hint message is shown.
*
*/
alwaysValid :: ![InteractionMask] -> (![VerifyMask],![InteractionMask])

/**
* Verifies a value which is always valid if filled in (e.g. a basic value).
* So only a hint message is needed.
*
* @param	A hint message
*/
simpleVerify :: !String ![InteractionMask] !VerifyOptions -> (![VerifyMask],![InteractionMask])

/**
* Verifies a custom ADT.
* For this ADT also a custom visualization has to be implemented.
* There is only one verify mask for the entire value.
* 
* @param 	An optional hint message
* @param	The predicate function
* @param	A function for error message generation, in case the predicate fails
* @param	The actual value (if present)

*/
customVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) ![InteractionMask] !VerifyOptions -> (![VerifyMask],![InteractionMask])

/**
* Sets a number of fields identified by data-paths in a verify mask to invalid.
*
* @ param	A list of data-paths of field to be set to invalid.
*			Additionally an error message is given for each field.
* @ param	The verify mask to be modified
*
* @ return	The modified verify mask
*/
setInvalid :: ![(!DataPath,!ErrorMessage)] !VerifyMask -> VerifyMask
