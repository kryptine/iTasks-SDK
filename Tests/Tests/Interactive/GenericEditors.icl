implementation module Tests.Interactive.GenericEditors

import iTasks, TestFramework

testGenericEditors :: TestSuite
testGenericEditors = testsuite "Generic editors" "These tests check if generic editors for standard types work"
	[testString,testChar,testInt,testReal,testBool
	,testADTSingleConsOne,testADTSingleConsMulti,testADTMultiCons
	,testSingleRecord,testNestedRecord,testOptionalRecord,testIntList
	]

testString = itest "String" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task String
	tut = testCommonInteractions "String"

testChar = itest "Char" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task Char
	tut = testCommonInteractions "Char"

testInt = itest "Int" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task Int
	tut = testCommonInteractions "Int"

testReal = itest "Real" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task Real
	tut = testCommonInteractions "Real"

testBool = itest "Bool" "Check if the editors behave 'normally'" "You should be able to edit in all editors" tut
where
	tut :: Task Bool
	tut = testCommonInteractions "Bool"

:: ADTSingleCons = ADTSingleCons Int
derive class iTask ADTSingleCons

testADTSingleConsOne = itest "Single Constructor" "Check the behavior of the editors" "The editors should behave as a normal Int editor" tut
where
	tut :: Task ADTSingleCons
	tut = testCommonInteractions "ADTSingleCons"

:: ADTSingleConsMulti = ADTSingleConsMulti Int String
derive class iTask ADTSingleConsMulti

testADTSingleConsMulti = itest "Single Constructor Multi" "Check the behavior of the editors" "You should have an Int editor and a String editor" tut
where
	tut :: Task ADTSingleConsMulti
	tut = testCommonInteractions "ADTSingleConsMulti"

:: ADTMultiCons
	= ADTMultiConsNone
	| ADTMultiConsSingle Int
	| ADTMultiConsMulti Int String

derive class iTask ADTMultiCons

testADTMultiCons = itest "Multiple Constructor" "Check the behavior of the editors"
	("You should have a dropdown to select the constructor."
	+++ "The first cons has no editors, the second a single Int and the third has an Int editor and a String editor"
	) tut
where
	tut :: Task ADTMultiCons
	tut = testCommonInteractions "ADTMultiCons"

:: TwoFieldRecord =
	{ first 	:: Int
	, second 	:: String
	}
derive class iTask TwoFieldRecord

testSingleRecord = itest "Record" "Check the behavior of the editors" "You should see editors for a record with an Int and a String field" tut
where
	tut :: Task TwoFieldRecord
	tut = testCommonInteractions "TwoFieldRecord"

:: NestedRecord =
	{ firstTwo 	:: TwoFieldRecord
	, third 	:: Bool
	}
derive class iTask NestedRecord

testNestedRecord = itest "Nested Record" "Check the behavior of the editors"
	("You should see editors for a record with as first field a nested record of an Int and a String field.\n"
	+++ "And as second field a Bool") tut
where
	tut :: Task NestedRecord
	tut = testCommonInteractions "NestedRecord"

testOptionalRecord = itest "Optional Record" "Check the behavior of the editors"
	("You should see an editor for a record with two fields, an Int and a String field.\n"
	+++ "The record should start with a checkbox that allows you to enable the record") tut
where
	tut :: Task (Maybe TwoFieldRecord)
	tut = testCommonInteractions "Optional record"


testIntList = itest "Standard Int list" "Check the behavior of the editors"
	"You should see an editor for a list of integers" tut
where
	tut :: Task [Int]
	tut = testCommonInteractions "Int list"

