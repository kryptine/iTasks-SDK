definition module GenVerify
import GenUpdate

:: LabeledDataPath :== [LabelOrNumber]
:: LabelOrNumber = Unlabeled Int | Label String

:: MessagePredicate = MPIfMasked | MPAlways
:: ErrorMessage = ErrorMessage String | IsBlankError
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

instance == LabelOrNumber

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

class VerifyState st
where
	firstChild		:: (*st -> *st) *st -> *st
	nthChild		:: !Int (*st -> *st) *st -> *st
	labeledChild	:: !String (*st -> *st) *st -> *st

	stepOut			:: *st -> *st //continuation utility-function which steps the datapath, hiding this step from the end user 

instance VerifyState HSt
instance VerifyState ESt

/*
* Checks if the value is masked and adds an appropriate error if this is not the case
*
* @param The path in the mask which needs to be checked for the value
* @param The path to the interface component to which the error needs to be applied
* @param The Error State
*
* @return The modified Error State
*/
verifyIfBlank 			:: !LabeledDataPath !LabeledDataPath !*ESt -> *ESt

getErrorMessage 		:: DataPath DataMask ErrorMask -> String
getHintMessage 			:: DataPath DataMask HintMask  -> String

getErrorCount			:: DataPath DataMask ErrorMask -> Int

toLabeledMask :: !DataMask -> [LabeledDataPath]