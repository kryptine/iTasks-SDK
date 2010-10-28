definition module GenVerify
import GenUpdate

:: ErrorMessage = ErrorMessage String | IsBlankError
:: HintMessage :== String
:: FieldLabel :== String

:: VerifyMask = VMValid (Maybe HintMessage) (Maybe FieldLabel) [VerifyMask]
			  | VMInvalid ErrorMessage (Maybe FieldLabel) [VerifyMask]
			  | VMUntouched (Maybe HintMessage) (Maybe FieldLabel) [VerifyMask]

:: *VerSt =
	{ updateMask	:: UpdateMask
	, verifyMask	:: VerifyMask
	, optional		:: Bool
	}


generic gVerify a :: (Maybe a) *VerSt -> *VerSt

instance GenMask VerifyMask
instance toString ErrorMessage

derive gVerify UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, Int, Real, Char, Bool, String, (,), (,,), (,,,),(->), []
derive gVerify Maybe, Dynamic, Void, Document, Either, Editable, Hidden, Display, VisualizationHint
derive gVerify Password, Date, Time, FormButton, Currency, User, UserDetails, Task, Note, DateTime
derive JSONEncode VerifyMask

verifyValue :: !a !UpdateMask -> VerifyMask | gVerify{|*|} a
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
verifyConstructor :: (Maybe String) (a -> Bool) (a -> String) (Maybe a) *VerSt -> *VerSt