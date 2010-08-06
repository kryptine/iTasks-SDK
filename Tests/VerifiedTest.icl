implementation module VerifiedTest

import iTasks
import StdMisc
import CommonDomain, Text

derive bimap (,),Maybe

// Verified Types

:: TestCons = Test1 String | Test2 Int | Test3

:: TestCons2 = Lala1 TestCons3 | Lala2 Int
:: TestCons3 = One Int | Two String

:: PositiveNum = Positive Int
:: EqString = Eq String String
:: FString = FString String
:: TestDoc = Test Document
:: PercentList = PercentList [Int]

:: TestStruct = 
	{ value1 :: String
	, value2 :: String
	}

derive gPrint 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc, NestedTest, NestedRec, TestCons, TestCons2, TestCons3,PercentList
derive gParse 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc, NestedTest, NestedRec, TestCons, TestCons2, TestCons3,PercentList
derive gUpdate 		TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc, NestedTest, NestedRec, TestCons, TestCons2, TestCons3,PercentList
derive gVisualize	TestRec, PositiveNum, EqString, FString, TestStruct, TestDoc, NestedTest, NestedRec, TestCons, TestCons2, TestCons3,PercentList
derive gError		TestDoc, NestedTest, NestedRec, TestCons2, TestCons3, TestCons
derive gHint		TestCons2, TestCons3

instance == ButtonState
where
	(==) Pressed Pressed = True
	(==) NotPressed NotPressed = True
	(==) _ _ = False

gHint{|TestCons|} _  hst=:{HSt | currentPath}
	# hst = appendHint "Test Hint!" MPIfMasked hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gHint{|TestDoc|} _ hst=:{HSt | currentPath}
	# hst = firstChild (appendHint "Test hint" MPAlways) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gHint{|TestRec|} Nothing hst=:{HSt | currentPath}
	# hst = appendHint "Hi! Make sure to specify the string length in the second field" MPAlways hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
	
gHint{|TestRec|} (Just rec) hst=:{HSt | currentPath}
	# hst = appendHint "Hi! Make sure to specify the string length in the second field." MPAlways hst
	# hst = labeledChild "string" (appendHint ("Pssst.. the lenght of the string is "+++ toString (size rec.TestRec.string)) MPIfMasked) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
/*
gError{|TestCons|} (Test1 s) est=:{ESt | currentPath}
	# est = appendError "You cannot select the first option." MPIfMasked est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|TestCons|} (Test2 i) est=:{ESt | currentPath}
	# est = verifyIfBlank (shiftLabeledDataPath currentPath) (shiftLabeledDataPath currentPath) est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|TestCons|} _ est=:{ESt | currentPath}
	
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
*/

gError{|TestRec|} r est=:{ESt | currentPath}
	#est = case r.int <> (size r.TestRec.string) of
		True
			# est = labeledChild "int" (appendError "This does not match the length of the string" MPIfMasked) est
			= appendError("You don't get it, do you?") MPAlways est
		_	= est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
	
gError{|PositiveNum|} (Positive x) est=:{ESt | currentPath}
	# est = verifyIfBlank (shiftLabeledDataPath currentPath) (shiftLabeledDataPath currentPath) est
	# est = if (x < 0) (firstChild (appendError ((toString x)+++" is not greater than 0") MPIfMasked) est) est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}

gHint{|PositiveNum|} _ hst=:{HSt | currentPath}
	# hst = firstChild (appendHint "Enter a positive number" MPAlways) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gHint{|NestedTest|} _ Nothing hst = hst	
gHint{|NestedTest|} fx (Just x) hst=:{HSt | currentPath}
	# hst = appendHint "Both strings need to be equal" MPAlways hst
	# hst = labeledChild "rec" (labeledChild "string" (appendHint "<- string" MPAlways)) hst
	# hst = firstChild (appendHint "<- string" MPAlways) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gError{|PercentList|} (PercentList []) est=:{ESt | currentPath} 
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|PercentList|} (PercentList x) est=:{ESt | currentPath}
	# sum = sum x;
	# est = if (sum <> 100) (firstChild (appendError ("The sum is "+++toString sum+++". It should be 100") MPAlways) est) est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}

gHint{|PercentList|} _ hst=:{HSt | currentPath}
	# hst = firstChild (appendHint "The sum of all values should be 100" MPAlways) hst
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
	
verifiedTest :: Task TestCons
verifiedTest = enterInformation "Enter info"

Start :: *World -> *World
Start world = startEngine [workflow "Verification Test" verifiedTest] world