implementation module VerifiedTest

import iTasks
import StdMisc
import CommonDomain, Text

derive bimap (,),Maybe

// Verified Types

:: PositiveNum = Positive Int
:: EqString = Eq String String
:: FString = FString String
:: TestDoc = Test Document

:: TestStruct = 
	{ value1 :: String
	, value2 :: String
	}

derive gPrint 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc
derive gParse 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc
derive gUpdate 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc
derive gVisualize	TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc
derive gError		TestDoc

instance == ButtonState
where
	(==) Pressed Pressed = True
	(==) NotPressed NotPressed = True
	(==) _ _ = False

gHint{|TestDoc|} _ hst=:{HSt | currentPath}
	# hst = firstChild (appendHint "Test hint") hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gHint{|TestRec|} Nothing hst=:{HSt | currentPath}
	# hst = appendHint "Hi! Make sure to specify the string length in the second field" hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
	
gHint{|TestRec|} (Just rec) hst=:{HSt | currentPath}
	# hst = appendHint "Hi! Make sure to specify the string length in the second field." hst
	# hst = firstChild (appendHint ("Pssst.. the lenght of the string is "+++ toString (size rec.string))) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
	
gError{|TestRec|} r est=:{ESt | currentPath}
	#est = case r.int <> (size r.string) of
		True
			# est = nthChild 1 (appendError "This does not match the length of the string") est
			= appendError("You don't get it, do you?") est
		_	= est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
	
gError{|PositiveNum|} (Positive x) est=:{ESt | currentPath}
	# est = if (x < 0) (firstChild (appendError ((toString x)+++" is number is not greater than 0")) est) est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}

gHint{|PositiveNum|} _ hst=:{HSt | currentPath}
	# hst = firstChild (appendHint "Enter a positive number") hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
	
:: TestRec =
	{ string :: String
	, int	 :: Int
	}
	
verifiedTest :: Task TestRec
verifiedTest = enterInformation "Enter info"

Start :: *World -> *World
Start world = startEngine [workflow "Verification Test" verifiedTest] world