implementation module Tests.Unit.CoreEditors
import TestFramework
import Tests.Unit.FrameworkStubs

import iTasks.UI.Editor, iTasks.UI.Diff, iTasks.UI.Layout
import iTasks._Framework.Generic.Interaction
import iTasks._Framework.IWorld
import qualified Data.Map as DM
import StdMisc

derive gEq EditMask
derive gText EditMask

//COMPLEX TYPES FOR TESTING

:: TestConsFields = TestConsFields Int Int Int Int Int Int
derive class iTask TestConsFields

:: TestRecordFields =
	{ a :: Int
	, b :: String
	, c :: Bool
	}
derive class iTask TestRecordFields

:: TestCons = ConsA | ConsB
derive class iTask TestCons

:: TestConsWithField = ConsWithFieldA | ConsWithFieldB String
derive class iTask TestConsWithField

testGenericEditorGenUI :: TestSuite
testGenericEditorGenUI = testsuite "Generic UI generation" "Tests for the core generic UI generation"
	[testIntUntouched
	,testIntTouched
	,testIntBlanked
	,testRealTouched
	,testConsFieldsTouched 
	,testMultipleConsesTouched
	,testConsesWithFieldTouched
	,testRecordTouched 
	,testMaybeIntUntouched
	]

testGenUI :: String UIDef a EditMask -> Test | iTask a
testGenUI name exp x mask = assertEqualWorld name exp sut
where
	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,vst) = gEditor{|*|}.Editor.genUI [] x mask vst
		# world = fromStubIWorld (fromStubVSt vst)
		= (res,world)

testIntUntouched = testGenUI "Untouched Int" 
	(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","info"),("hint","Please enter a whole number (this value is required)")]}
		(UI (UIControl (UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v", value = Nothing})))))
	42 Untouched

testIntTouched = testGenUI "Touched Int"
	(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a whole number")]}
		(UI (UIControl (UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v", value = Just (JSONInt 42)})))))
	42 Touched

testIntBlanked = testGenUI "Blanked Int"
	(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","invalid"),("hint","You need to enter a whole number (this value is required)")]}
		(UI (UIControl (UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v", value = Nothing})))))
	42 Blanked

testRealTouched = testGenUI "Touched Real"
	(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a decimal number")]}
		(UI (UIControl (UIEditDecimal {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				       {UIEditOpts|taskId="STUB", editorId="v", value = Just (JSONReal 3.14)})))))
	3.14 Touched

testConsFieldsTouched = testGenUI "Touched cons fields"
	(UI (UICompoundEditor {UIEditor|optional=False,attributes='DM'.newMap}
		[fieldExp "v0" 1, fieldExp "v1" 2, fieldExp "v2" 3, fieldExp "v3" 4,fieldExp "v4" 5,fieldExp "v5" 6]))
	(TestConsFields 1 2 3 4 5 6) Touched
where
	fieldExp editorId val = 
		(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a whole number")]}
			(UI (UIControl (UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
					   {UIEditOpts|taskId="STUB", editorId=editorId, value = Just (JSONInt val)})))))

testMultipleConsesTouched = testGenUI "Touched constructor selection"
	(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly selected an option")]}
		(UI (UIControl (UIDropdown  {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
					   {UIChoiceOpts|taskId="STUB", editorId="v", value = [0], options = ["ConsA","ConsB"]})))))
	ConsA Touched

testConsesWithFieldTouched = testGenUI "Touched constructor with field"
	(UI (UICompoundEditor {UIEditor|optional=False,attributes='DM'.newMap} [consExp,fieldsExp]))
	ConsWithFieldA Touched	
where
	consExp = (UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly selected an option")]}
		(UI (UIControl (UIDropdown  {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
					   {UIChoiceOpts|taskId="STUB", editorId="v", value = [0], options = ["ConsWithFieldA","ConsWithFieldB"]})))))

	fieldsExp = UI UIEmpty //Placeholder

testRecordTouched = testGenUI "Touched record"
	(UI (UICompoundEditor {UIEditor|optional=False,attributes='DM'.newMap}
		[intField,stringField,boolField]))
	{ a = 42, b = "Foo", c = True} Touched
where
	intField =
		(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a whole number"),("label","a")]}
			(UI (UIControl (UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v0", value = Just (JSONInt 42)})))))

	stringField =
		(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a single line of text"),("label","b")]}
			(UI (UIControl (UIEditString {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
					{UIEditOpts|taskId="STUB", editorId="v1", value = Just (JSONString "Foo")})))))
	boolField =
		(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("label","c")]}
		    (UI (UIControl (UIEditCheckbox {UIFSizeOpts|margins=Nothing}
				        {UIEditOpts|taskId="STUB", editorId="v2", value = Just (JSONBool True)})))))

testMaybeIntUntouched = testGenUI "Untouched optional Int"
		(UI (UIEditor {UIEditor|optional=True,attributes='DM'.fromList[("hint-type","info"),("hint","Please enter a whole number")]}
			(UI (UIControl (UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v", value = Nothing})))))
		test Untouched
where
	test :: Maybe Int
	test = Nothing

testGenericEditorEdits :: TestSuite
testGenericEditorEdits = testsuite "Generic edits" "Tests for processing edits by editors"
	[testEditConsChange
	]

testGenEdit :: String (Masked a) (Masked a) DataPath JSONNode -> Test | iTask a
testGenEdit name exp (ov,om) dp edit = assertEqualWorld name exp sut
where
	sut world 
		# ust = toStubUSt (toStubIWorld world)
		# (nv,nm,ust) = gEditor{|*|}.Editor.appDiff dp edit ov om ust
		# world = fromStubIWorld (fromStubUSt ust)
		= ((nv,nm),world)

testEditConsChange = skip "Change constructor" 
/*
	= testGenEdit "Change constructor"
	(ConsB, Touched)
	(ConsA, Touched)
	[]
	(JSONInt 1)
*/

testGenericEditorDiffs :: TestSuite
testGenericEditorDiffs = testsuite "Generic diffs" "Tests for the generic diffs"
	[testSameInt
	,testDifferentInt1
	,testDifferentInt2
	,testDiffConsFields1
	,testDiffConsFields2
	,testDiffRecordFields
	,testDiffConsChange
	,testDiffConsWithFieldChange
	,testMaybeIntChangeToJust
	,testMaybeIntChangeToNothing
	]

//General pattern for diff tests

testGenDiff :: String UIChangeDef (Masked a) (Masked a) -> Test | iTask a
testGenDiff name exp (x,mx) (y,my) = assertEqualWorld name exp sut
where
	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,vst) = gEditor{|*|}.genDiff [] x mx y my vst
		# world = fromStubIWorld (fromStubVSt vst)
		= (compactChangeDef res,world)

//Integers
testSameInt :: Test
testSameInt = testGenDiff "Same Int" NoChange (42,Touched) (42,Touched)

testDifferentInt1 :: Test
testDifferentInt1 = testGenDiff "Different Int 1" (ChangeUI [("setEditorValue",[JSONInt 23])] []) (42,Touched) (23,Touched)

testDifferentInt2 :: Test
testDifferentInt2
	= testGenDiff "Different Int 2"
		(ChangeUI [("setEditorValue",[JSONInt 3])
				  ,("setAttribute",[JSONString HINT_ATTRIBUTE,JSONString "You need to enter a whole number (this value is required)"])
				  ,("setAttribute",[JSONString HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INVALID])
				  ] [])
			(42,Touched) (3,Blanked)

testDiffConsFields1 :: Test
testDiffConsFields1 
	= testGenDiff "Diff constructor fields 1" 
		(ChangeUI [] [ChangeChild 3 (ChangeUI [("setEditorValue",[JSONInt 44])] [])])
		(TestConsFields 1 2 3 4 5 6,Touched)
		(TestConsFields 1 2 3 44 5 6,Touched)

testDiffConsFields2 :: Test
testDiffConsFields2 
	= testGenDiff "Diff constructor fields 2" 
		(ChangeUI [] [ChangeChild 3 (ChangeUI [("setEditorValue",[JSONInt 44])
											  ,("setAttribute",[JSONString HINT_ATTRIBUTE,JSONString "You have correctly entered a whole number"])
											  ,("setAttribute",[JSONString HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_VALID])
											  ] [])])

		(TestConsFields 1 2 3 4 5 6, Untouched)
		(TestConsFields 1 2 3 44 5 6, CompoundMask [Untouched,Untouched,Untouched,Touched,Untouched,Untouched])

testDiffRecordFields :: Test
testDiffRecordFields 
	= testGenDiff "Diff record fields"
		(ChangeUI [] [ChangeChild 0 (ChangeUI [("setEditorValue",[JSONInt 23])] []),ChangeChild 1 (ChangeUI [("setEditorValue",[JSONString "bar"])] [])])
		({TestRecordFields|a=42,b="foo",c=True},Touched)
		({TestRecordFields|a=23,b="bar",c=True},Touched)

testDiffConsChange :: Test
testDiffConsChange 
	= testGenDiff "Changing a single constructor"
		(ChangeUI [("setValue",[JSONInt 1,JSONBool True])] [])
		(ConsA,Touched)
		(ConsB,Touched)

testDiffConsWithFieldChange :: Test
testDiffConsWithFieldChange 
	= testGenDiff "Changing a constructor with a data field"
		(ChangeUI [] [ChangeChild 0 (ChangeUI [("setValue",[JSONInt 1,JSONBool True])] []), ChangeChild 1 (ReplaceUI expField)])
		(ConsWithFieldA,Touched)
		(ConsWithFieldB "Foo",Touched)
where
	expField =
		(UI (UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a single line of text")]}
			(UI (UIControl (UIEditString {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
					   {UIEditOpts|taskId="STUB", editorId="v0", value = Just (JSONString "Foo")})))))
	
testMaybeIntChangeToJust :: Test
testMaybeIntChangeToJust
	= testGenDiff "Switch Maybe Int Nothing to Just"
		(ChangeUI [("setEditorValue", [JSONInt 42])
				  ,("setAttribute",[JSONString HINT_ATTRIBUTE,JSONString "You have correctly entered a whole number"])
				  ,("setAttribute",[JSONString HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_VALID])
				  ] [])
		(Nothing,Touched)
		(Just 42,Touched)

testMaybeIntChangeToNothing :: Test
testMaybeIntChangeToNothing = skip "Switch Maybe Int Just to Nothing"
/*
	= testGenDiff "Switch Maybe Int Just to Nothing"
		(ChangeUI [("setEditorValue", [JSONNull])] [])
		(Just 42)
		Nothing
*/

