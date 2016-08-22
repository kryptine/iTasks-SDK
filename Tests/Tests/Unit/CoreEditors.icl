implementation module Tests.Unit.CoreEditors
import TestFramework
import Tests.Unit.FrameworkStubs

import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Layout
import iTasks._Framework.IWorld
import qualified Data.Map as DM
import StdMisc

derive gText EditMask, FieldMask

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
	[testIntEnter
	,testIntUpdate
	,testRealUpdate
	,testConsFieldsUpdate
	,testMultipleConsesUpdate
	,testConsesWithFieldTouched
	,testRecordTouched 
	,testMaybeIntEnter
	]

testGenUI :: String (UI,EditMask) a EditMode-> Test | iTask a
testGenUI name exp x mode = assertEqualWorld name (Ok exp) sut
where
	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,vst) = gEditor{|*|}.Editor.genUI [] x {VSt|vst & mode = mode}
		# world = fromStubIWorld (fromStubVSt vst)
		= (res,world)

testIntEnter = testGenUI "Enter Int" 
	(uia UIIntegerField
		('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "info")
						,("hint",JSONString "Please enter a whole number (this value is required)")
						,("taskId",JSONString "STUB")
						,("editorId",JSONString "v")
						,("value",JSONNull)
						]),FieldMask {touched=False,valid=False,state=JSONNull})
	0 Enter

testIntUpdate = testGenUI "Update Int"
	(uia UIIntegerField
		('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid")
						,("hint",JSONString "You have correctly entered a whole number")
						,("taskId",JSONString "STUB")
						,("editorId",JSONString "v")
						,("value",JSONInt 42)
						]),FieldMask {touched=False,valid=True,state=JSONInt 42})
	42 Update

testRealUpdate = testGenUI "Update Real"
	(uia UIDecimalField
		('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid")
						,("hint",JSONString "You have correctly entered a decimal number")
						,("taskId",JSONString "STUB")
						,("editorId",JSONString "v")
						,("value",JSONReal 3.14)
						]),FieldMask {touched=False,valid=True,state=JSONReal 3.14})
	3.14 Update

testConsFieldsUpdate = testGenUI "Update constructor fields"
	(uic UICons [fieldExp "v0" 1, fieldExp "v1" 2, fieldExp "v2" 3, fieldExp "v3" 4,fieldExp "v4" 5,fieldExp "v5" 6]
		,CompoundMask [maskExp n \\ n <- [1..6]])
	(TestConsFields 1 2 3 4 5 6) Update
where
	fieldExp editorId val = 
		uia UIIntegerField
			('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid"),("hint",JSONString "You have correctly entered a whole number")
							,("taskId",JSONString "STUB"),("editorId",JSONString editorId),("value",JSONInt val)
							])
	maskExp n = FieldMask {touched = False, valid = True, state = JSONInt n}

testMultipleConsesUpdate = skip "Touched constructor selection"
/* testGenUI "Touched constructor selection"
	(uia UIDropdown
		('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid"),("hint",JSONString "You have correctly selected an option")
					,("taskId",JSONString "STUB"),("editorId",JSONString "v")
					,("value",JSONArray [JSONInt 0]),("options",JSONArray [JSONString "ConsA",JSONString "ConsB"]) ]))
	ConsA Touched
*/

testConsesWithFieldTouched = skip "Touched constructor with field"
/* testGenUI "Touched constructor with field"
	(uiac UICompoundContent ('DM'.fromList [("optional",JSONBool False)]) [consExp,fieldsExp])
	ConsWithFieldA Touched	
where
	consExp = (uia UIDropdown 
		('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid"),("hint",JSONString "You have correctly selected an option")
			,("taskId",JSONString "STUB"),("editorId",JSONString "v")
			,("value",JSONArray [JSONInt 0]),("options",JSONArray [JSONString "ConsWithFieldA",JSONString "ConsWithFieldB"])]))

	fieldsExp = ui UIEmpty//Placeholder
*/

testRecordTouched = skip "Touched record"
/* testGenUI "Touched record"
	(uiac UICompoundContent ('DM'.fromList [("optional",JSONBool False)]) 
		[intField,stringField,boolField])
	{ a = 42, b = "Foo", c = True} Touched
where
	intField =
		(uia UIEditInt 
			('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid")
							,("hint",JSONString "You have correctly entered a whole number"),("label",JSONString "a")
							,("taskId",JSONString "STUB")
							,("editorId",JSONString "v0")
							,("value",JSONInt 42)]))
	stringField =
		(uia UIEditString
			('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid")
							,("hint",JSONString "You have correctly entered a single line of text"),("label",JSONString "b")
							,("taskId",JSONString "STUB")
							,("editorId",JSONString "v1")
							,("value",JSONString "Foo")
							]))
	boolField =
		(uia UIEditCheckbox 
			('DM'.fromList[("optional",JSONBool False),("label",JSONString "c"),("taskId",JSONString "STUB"),("editorId",JSONString "v2")]))
*/

testMaybeIntEnter = testGenUI "Enter optional Int"
   (uia UIIntegerField ('DM'.fromList[("optional",JSONBool True)
                                     ,("hint-type",JSONString "info")
                                     ,("hint",JSONString "Please enter a whole number")
                                     ,("taskId",JSONString "STUB")
                                     ,("editorId",JSONString "v")
                                     ,("value",JSONNull)]), FieldMask {touched=False,valid=True,state=JSONNull})
		test Enter
where
	test :: Maybe Int
	test = Nothing

testGenericEditorEdits :: TestSuite
testGenericEditorEdits = testsuite "Generic edits" "Tests for processing edits by editors"
	[testEditConsChange
	,testEditListElement
	,testAddListElement
	]

testGenEdit :: String (a,EditMask,UIChange) (a,EditMask) (DataPath,JSONNode) -> Test | iTask a
testGenEdit name exp (ov,om) (tp,edit) = assertEqualWorld name (Ok exp) sut
where
	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,nv,vst) = gEditor{|*|}.Editor.onEdit [] (tp,edit) ov om vst
		# world = fromStubIWorld (fromStubVSt vst)
		= (fmap (\(nc,nm) -> (nv,nm,nc)) res,world)

testEditConsChange = skip "Change constructor" 
/*
	= testGenEdit "Change constructor"
	(ConsB, Touched)
	(ConsA, Touched)
	[]
	(JSONInt 1)
*/

testEditListElement = testGenEdit "List element edit" 
	([42],CompoundMask [FieldMask {touched=True,valid=True,state=JSONInt 42}]
		,ChangeUI [] [(0, ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 42), SetAttribute "hint" (JSONString "You have correctly entered a whole number"), SetAttribute "hint-type" (JSONString "valid")] []))])
	([0],CompoundMask [FieldMask {touched=True,valid=True,state=JSONNull}])
	([0],JSONInt 42)

testAddListElement = testGenEdit "List element add"
	([42,0],CompoundMask [FieldMask {touched=True,valid=True,state=JSONInt 42},FieldMask {touched=False,valid=False,state=JSONNull}]
		,ChangeUI [] [(1,InsertChild elui)

					 ,(2,ChangeChild (ChangeUI [] [(0, ChangeChild (ChangeUI [SetAttribute "value" (JSONString "2 items")] []))]))
					 ])
	([42],CompoundMask [FieldMask {touched=True,valid=True,state=JSONInt 42}])
	([],JSONString "add")
where
	elui = uiac UIContainer ('DM'.fromList [("direction",JSONString "horizontal"),("halign",JSONString "right"),("height",JSONString "wrap")]) [inui:buis]
	inui = uia UIIntegerField ('DM'.fromList[("optional",JSONBool False)
                                     ,("hint-type",JSONString "info")
                                     ,("hint",JSONString "Please enter a whole number (this value is required)")
                                     ,("taskId",JSONString "STUB")
                                     ,("editorId",JSONString "v1")
                                     ,("value",JSONNull)])
	buis = [uia UIButton ('DM'.fromList [("iconCls",JSONString "icon-up"),("value",JSONString "mup_1"),("enabled",JSONBool True),("taskId",JSONString "STUB"),("editorId",JSONString "v")])
	       ,uia UIButton ('DM'.fromList [("iconCls",JSONString "icon-down"),("value",JSONString "mdn_1"),("enabled",JSONBool False),("taskId",JSONString "STUB"),("editorId",JSONString "v")])
	       ,uia UIButton ('DM'.fromList [("iconCls",JSONString "icon-remove"),("value",JSONString "rem_1"),("taskId",JSONString "STUB"),("editorId",JSONString "v")])
	       ]

testGenericEditorRefreshes :: TestSuite
testGenericEditorRefreshes = testsuite "Generic diffs" "Tests for the generic diffs"
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

testOnRefresh :: String UIChange a a EditMask -> Test | iTask a
testOnRefresh name exp n o m = assertEqualWorld name (Ok exp) sut
where
	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,val,vst) = gEditor{|*|}.Editor.onRefresh [] n o m vst
		# world = fromStubIWorld (fromStubVSt vst)
		= case res of
			(Ok (change,mask))  = (Ok (compactUIChange change),world)
			(Error e) 			= (Error e, world)

//Integers
testSameInt :: Test
testSameInt = testOnRefresh "Same Int" NoChange 42 42 newFieldMask

testDifferentInt1 :: Test
testDifferentInt1 = testOnRefresh "Different Int 1" (ChangeUI [SetAttribute "value" (JSONInt 23)] []) 23 42 newFieldMask

testDifferentInt2 :: Test
testDifferentInt2
	= testOnRefresh "Different Int 2"
		(ChangeUI [SetAttribute "value" (JSONInt 3)
				  ,SetAttribute HINT_ATTRIBUTE (JSONString "You need to enter a whole number (this value is required)")
				  ,SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_INVALID)
				  ] [])
			3 42 newFieldMask

testDiffConsFields1 :: Test
testDiffConsFields1 
	= testOnRefresh "Diff constructor fields 1" 
		(ChangeUI [] [(3,ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 44)] []))])
		(TestConsFields 1 2 3 44 5 6) 
		(TestConsFields 1 2 3 4 5 6)
		newFieldMask

testDiffConsFields2 :: Test
testDiffConsFields2 
	= testOnRefresh "Diff constructor fields 2" 
		(ChangeUI [] [(3,ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 44)
											  ,SetAttribute HINT_ATTRIBUTE (JSONString "You have correctly entered a whole number")
											  ,SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_VALID)
											  ] []))])
		(TestConsFields 1 2 3 44 5 6)
		(TestConsFields 1 2 3 4 5 6) 
		(CompoundMask [newFieldMask,newFieldMask,newFieldMask,newFieldMask,newFieldMask,newFieldMask])

testDiffRecordFields :: Test
testDiffRecordFields 
	= testOnRefresh "Diff record fields"
		(ChangeUI [] [(0, ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 23)] [])),(1,ChangeChild (ChangeUI [SetAttribute "value" (JSONString "bar")] []))])
		{TestRecordFields|a=23,b="bar",c=True} {TestRecordFields|a=42,b="foo",c=True} newFieldMask

testDiffConsChange :: Test
testDiffConsChange 
	= testOnRefresh "Changing a single constructor"
		(ChangeUI [SetAttribute "value" (JSONArray [JSONInt 1,JSONBool True])] [])
		ConsB ConsA newFieldMask

testDiffConsWithFieldChange :: Test
testDiffConsWithFieldChange 
	= testOnRefresh "Changing a constructor with a data field"
		(ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "value" (JSONArray [JSONInt 1,JSONBool True])] [])), (1,ChangeChild (ReplaceUI expField))])
		(ConsWithFieldB "Foo") ConsWithFieldA newFieldMask
where
	expField = uia UITextField
		('DM'.fromList[("optional",JSONBool False)
			 		  ,("hint-type",JSONString "valid")
					  ,("hint",JSONString "You have correctly entered a single line of text")
					  ,("taskId",JSONString "STUB")
					  ,("editorId",JSONString "v0")
					  ,("value",JSONString "Foo")
					])
						
testMaybeIntChangeToJust :: Test
testMaybeIntChangeToJust
	= testOnRefresh "Switch Maybe Int Nothing to Just"
		(ChangeUI [SetAttribute "value" (JSONInt 42)
				  ,SetAttribute HINT_ATTRIBUTE (JSONString "You have correctly entered a whole number")
				  ,SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_VALID)
				  ] [])
		(Just 42)
		Nothing
		newFieldMask

testMaybeIntChangeToNothing :: Test
testMaybeIntChangeToNothing = skip "Switch Maybe Int Just to Nothing"
/*
	= testOnRefresh "Switch Maybe Int Just to Nothing"
		(ChangeUI [("setEditorValue", [JSONNull])] [])
		(Just 42)
		Nothing
*/

testGenericHelperFunctions :: TestSuite
testGenericHelperFunctions = testsuite "Generic helper functions" "Tests for the helper functions used by the generic editors"
	[
	]


