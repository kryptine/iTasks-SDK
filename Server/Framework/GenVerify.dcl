definition module GenVerify

import GenUpdate

:: ErrorMessage = ErrorMessage !String | IsBlankError
:: HintMessage :== String
:: Optional :== Bool

:: VerifyMask = VMUntouched !(Maybe HintMessage) !Optional ![VerifyMask] 
			  | VMValid !(Maybe HintMessage) ![VerifyMask]
			  | VMInvalid !ErrorMessage ![VerifyMask]
			  
:: *VerSt =
	{ updateMask	:: ![UpdateMask]
	, verifyMask	:: ![VerifyMask]
	, optional		:: !Bool
	, iworld        :: !*IWorld
	}

generic gVerify a :: (Maybe a) *VerSt -> *VerSt

instance GenMask VerifyMask
instance toString ErrorMessage

derive gVerify UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, Int, Real, Char, Bool, String, (,), (,,),(,,,),(->), []
derive gVerify Maybe, Dynamic, Void, Document, Either, Editable, Hidden, Display, VisualizationHint, Timestamp
derive gVerify Password, Date, Time, FormButton, Currency, User, UserDetails, Note, DateTime, Choice, MultipleChoice, Shared, SharedReadOnly, Map, Tree, TreeNode
derive gVerify EmailAddress, Action, ProcessRef, Table

/**
* Verify a value based on the value and its update mask.
*/
verifyValue :: !a !UpdateMask !*IWorld -> (!VerifyMask, !*IWorld) | gVerify{|*|} a

/**
* Based on the verify mask of a value, determine if it is valid.
* A value is valid if the verify mask contains no invalid fields and all untouched fields are optional
*/
isValidValue :: !VerifyMask -> Bool

/**
* Verifies a value which is always valid.
* No hint message is shown.
*
* @param	The verify-state
*
* @return	The modified verify-state
*/
alwaysValid :: !*VerSt -> *VerSt

/**
* Verifies a value which is always valid if filled in (e.g. a basic value).
* So only a hint message is needed.
*
* @param	A hint message
* @param	The verify-state
*
* @return	The modified verify-state
*/
simpleVerify :: !String !*VerSt -> *VerSt

/**
* Verifies an ADT wrapping an already existing type to put extra restrictions on it.
* The ADT must have the form :: ADTName = ConsName x.
* The instances for gVisualize and gUpdate can be generically derived,
* which means that just a value of type x is visualized.
*
* For example the following type can be used to represent positive integers:
* :: PositiveInt = PositiveInt Int
* A predicate checking if the integer is positive has to be given to this function then.
* 
* @param 	An optional hint message
* @param	A predicate function
* @param	A function for error message generation, in case the predicate fails
* @param	The actual value (if present)
* @param	The verify-state
*
* @return	The modified verify-state
*/
wrapperVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) !*VerSt -> *VerSt

/**
* Verifies a custom ADT.
* For this ADT also a custom visualization has to be implemented.
* There is only one verify mask for the entire value.
* 
* @param 	An optional hint message
* @param	The predicate function
* @param	A function for error message generation, in case the predicate fails
* @param	The actual value (if present)
* @param	The verify-state
*
* @return	The modified verify-state
*/
customVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) !*VerSt -> *VerSt

/**
* Verifies a custom ADT.
* For this ADT also a custom visualization has to be implemented.
* There is only one verify mask for the entire value.
* For determining if a value is valid and generating a hint or error message
* also world can be used (e.g. for calling a compiler).
* 
* @param	An optional default hint message used if no valid value is filled in
* @param	A function for determining if a value is valid
*			and generating an optional hint or an error message.
* @param	The actual value (if present)
* @param	The verify-state
*
* @return	The modified verify-state
*/
customWorldVerify :: !(Maybe String) !(a *IWorld -> (!WorldPredicateResult,!*IWorld)) !(Maybe a) !*VerSt -> *VerSt

:: WorldPredicateResult = WPRValid !(Maybe String) | WPRInvalid !String

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