definition module GenVerify
import GenUpdate

:: LabeledDataPath :== [LabelOrNumber]
:: LabelOrNumber = Unlabeled Int | Label String

:: MessagePredicate = MPIfMasked | MPAlways
:: ErrorMessage :== String
:: HintMessage :== String

:: ErrorMask :== [(LabeledDataPath, MessagePredicate, ErrorMessage)]
:: HintMask  :== [(LabeledDataPath, MessagePredicate, HintMessage)]

:: *ESt =
	{ dataMask		:: DataMask
	, errorMask		:: ErrorMask
	, currentPath	:: LabeledDataPath
	}
	
:: *HSt =
	{ dataMask		:: DataMask
	, hintMask		:: HintMask
	, currentPath	:: LabeledDataPath
	}

generic gError a :: a *ESt -> *ESt
generic gHint  a :: (Maybe a) *HSt -> *HSt

derive gError UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, Int, Real, Char, Bool, String, (,), (,,), (,,,), [], Maybe, Dynamic, Void, Either
derive gHint  UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, Int, Real, Char, Bool, String, (,), (,,), (,,,), [], Maybe, Dynamic, Void, Either
	
determineErrors			:: a DataMask -> ErrorMask 	| gError{|*|} a
determineHints 			:: a DataMask -> HintMask 	| gHint{|*|} a

stepLabeledDataPath 	:: LabeledDataPath -> LabeledDataPath
shiftLabeledDataPath 	:: LabeledDataPath -> LabeledDataPath

appendError 			:: !String !MessagePredicate *ESt -> *ESt
appendHint				:: !String !MessagePredicate *HSt -> *HSt

class ChildLookup st
where
	firstChild		:: (*st -> *st) *st -> *st
	nthChild		:: !Int (*st -> *st) *st -> *st
	labeledChild	:: !String (*st -> *st) *st -> *st

instance ChildLookup HSt
instance ChildLookup ESt

getErrorMessage 		:: DataPath DataMask ErrorMask -> String
getHintMessage 			:: DataPath DataMask HintMask  -> String

getErrorCount			:: DataPath DataMask ErrorMask -> Int