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

derive gVerify UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, Int, Real, Char, Bool, String, (,), (,,), (,,,), [], Maybe, Dynamic, Void, Document, Either, Editable, Hidden, Display, VisualizationHint
derive JSONEncode VerifyMask

verifyValue :: !a !UpdateMask -> VerifyMask | gVerify{|*|} a
basicVerify :: String !*VerSt -> *VerSt

