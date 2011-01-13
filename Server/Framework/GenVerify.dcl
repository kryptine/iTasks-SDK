definition module GenVerify
import GenUpdate

:: ErrorMessage = ErrorMessage String | IsBlankError
:: HintMessage :== String
:: FieldLabel :== String
:: Optional :== Bool

:: VerifyMask = VMUntouched (Maybe HintMessage) (Maybe FieldLabel) !Optional [VerifyMask] 
			  | VMValid (Maybe HintMessage) (Maybe FieldLabel) [VerifyMask]
			  | VMInvalid ErrorMessage (Maybe FieldLabel) [VerifyMask]
			  

:: *VerSt =
	{ updateMask	:: [UpdateMask]
	, verifyMask	:: [VerifyMask]
	, optional		:: Bool
	, iworld        :: *IWorld
	}


generic gVerify a :: (Maybe a) *VerSt -> *VerSt

instance GenMask VerifyMask
instance toString ErrorMessage

derive gVerify UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, Int, Real, Char, Bool, String, (,), (,,),(,,,),(->), []
derive gVerify Maybe, Dynamic, Void, Document, Either, Editable, Hidden, Display, VisualizationHint
derive gVerify Password, Date, Time, FormButton, Currency, User, UserDetails, Task, Note, DateTime, Choice, MultipleChoice
derive JSONEncode VerifyMask

/**
* Verify a value based on the value and its update mask.
*/
verifyValue :: !a !UpdateMask *IWorld -> (VerifyMask, *IWorld) | gVerify{|*|} a

/**
* Based on the verify mask of a value, determine if it is valid.
* A value is valid if the verify mask contains no invalid fields and all untouched fields are optional
*/
isValidValue :: !VerifyMask -> Bool

basicVerify :: String !*VerSt -> *VerSt

/**
* Verifies a custom ADT
* 
* @param 	An optional hint message
* @param	The predicate function
* @param	A function for error message generation, in case the predicate fails
* @param	The actual value (if present)
* @param	The verify-state
*
* @return	The modified verify-state
*/
verifyConstructor :: (Maybe String) (a -> Bool) (a -> String) (Maybe a) !*VerSt -> *VerSt


/**
* Verifies a custom ADT using the world
* 
* @param 	An optional hint message
* @param	A function for error message generation, in case the predicate fails
* @param	The actual value (if present)
* @param	The verify-state
*
* @return	The modified verify-state
*/
worldVerify :: (Maybe String) (a *IWorld -> (Maybe String, *IWorld)) (Maybe a) !*VerSt -> *VerSt