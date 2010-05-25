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

derive gPrint 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc, NestedTest, NestedRec
derive gParse 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc, NestedTest, NestedRec
derive gUpdate 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc, NestedTest, NestedRec
derive gVisualize	TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc, NestedTest, NestedRec
derive gError		TestDoc, NestedTest, NestedRec

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
	# hst = labeledChild "string" (appendHint ("Pssst.. the lenght of the string is "+++ toString (size rec.TestRec.string))) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
	
gError{|TestRec|} r est=:{ESt | currentPath}
	#est = case r.int <> (size r.TestRec.string) of
		True
			# est = labeledChild "int" (appendError "This does not match the length of the string") est
			= appendError("You don't get it, do you?") est
		_	= est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
	
gError{|PositiveNum|} (Positive x) est=:{ESt | currentPath}
	# est = if (x < 0) (firstChild (appendError ((toString x)+++" is number is not greater than 0")) est) est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}

gHint{|PositiveNum|} _ hst=:{HSt | currentPath}
	# hst = firstChild (appendHint "Enter a positive number") hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gHint{|NestedTest|} _ Nothing hst = hst	
gHint{|NestedTest|} fx (Just x) hst=:{HSt | currentPath}
	# hst = appendHint("Both strings need to be equal") hst
	# hst = labeledChild "rec" (labeledChild "string" (appendHint "<- string")) hst
	# hst = firstChild (appendHint "<- string") hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
	
:: TestRec =
	{ string :: String
	, int	 :: Int
	}

:: NestedRec a =
	{ string 	:: String
	, positive 	:: a
	}
	
:: NestedTest a =
	{ string :: String
	, rec	 :: PositiveNum
	}
	
verifiedTest :: Task TestRec
verifiedTest = enterInformation "Enter info"

Start :: *World -> *World
Start world = startEngine [workflow "Verification Test" verifiedTest] world