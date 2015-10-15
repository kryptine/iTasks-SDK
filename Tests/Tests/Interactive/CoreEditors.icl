implementation module Tests.Interactive.CoreEditors

import iTasks, TestFramework

testCoreEditors :: TestSuite
testCoreEditors = testsuite "Core editors" "These tests check if editors for the core data types work"
	[testString,testChar,testInt,testReal,testBool
	,testADTSingleConsOne,testADTSingleConsMulti,testADTMultiCons
	,testSingleRecord,testNestedRecord
	]

testString = interactive "String" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task String
	tut = testEditors "String"

testChar = interactive "Char" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task Char
	tut = testEditors "Char"

testInt = interactive "Int" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task Int
	tut = testEditors "Int"

testReal = interactive "Real" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task Real
	tut = testEditors "Real"

testBool = interactive "Bool" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task Bool
	tut = testEditors "Bool"

:: ADTSingleCons = ADTSingleCons Int
derive class iTask ADTSingleCons

testADTSingleConsOne = interactive "Single Constructor" "Check the behavior of the editors" "The editors should behave as a normal Int editor" tut
where
	tut :: Task ADTSingleCons
	tut = testEditors "ADTSingleCons"

:: ADTSingleConsMulti = ADTSingleConsMulti Int String
derive class iTask ADTSingleConsMulti

testADTSingleConsMulti = interactive "Single Constructor Multi" "Check the behavior of the editors" "You should have an Int editor and a String editor" tut
where
	tut :: Task ADTSingleConsMulti
	tut = testEditors "ADTSingleConsMulti"

:: ADTMultiCons
	= ADTMultiConsNone
	| ADTMultiConsSingle Int
	| ADTMultiConsMulti Int String

derive class iTask ADTMultiCons

testADTMultiCons = interactive "Multiple Constructor" "Check the behavior of the editors"
	("You should have a dropdown to select the constructor."
	+++ "The first cons has no editors, the second a single Int and the third has an Int editor and a String editor"
	) tut
where
	tut :: Task ADTMultiCons
	tut = testEditors "ADTMultiCons"

:: TwoFieldRecord =
	{ first 	:: Int
	, second 	:: String
	}
derive class iTask TwoFieldRecord

testSingleRecord = interactive "Record" "Check the behavior of the editors" "You should see editors for a record with an Int and a String field" tut
where
	tut :: Task TwoFieldRecord
	tut = testEditors "TwoFieldRecord"

:: NestedRecord =
	{ firstTwo 	:: TwoFieldRecord
	, third 	:: Bool
	}
derive class iTask NestedRecord

testNestedRecord = interactive "Nested Record" "Check the behavior of the editors"
	("You should see editors for a record with as first field a nested record of an Int and a String field.\n"
	+++ "And as second field a Bool") tut
where
	tut :: Task NestedRecord
	tut = testEditors "NestedRecord"

