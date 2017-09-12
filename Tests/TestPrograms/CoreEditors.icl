module CoreEditors
import iTasks.Internal.Test.Definition
import iTasks.Internal.Test.Stubs

import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Layout
import iTasks.Internal.IWorld
import qualified Data.Map as DM
import Data.Either, Data.Error, Data.Functor
import StdMisc

derive gText EditMask, FieldMask, CompoundMask
derive gPrettyTrace MaybeError, EditMask, FieldMask, CompoundMask
derive gPrettyTrace UIChange, UIChildChange, UIAttributeChange, UI, UIType, JSONNode

//COMPLEX TYPES FOR TESTING

:: TestConsFields = TestConsFields Int Int Int Int Int Int
derive class iTask TestConsFields
derive gPrettyTrace TestConsFields

:: TestRecordFields =
	{ a :: Int
	, b :: String
	, c :: Bool
	}
derive class iTask TestRecordFields
derive gPrettyTrace TestRecordFields

:: TestCons = ConsA | ConsB
derive class iTask TestCons
derive gPrettyTrace TestCons

:: TestConsWithField = ConsWithFieldA | ConsWithFieldB String
derive class iTask TestConsWithField
derive gPrettyTrace TestConsWithField

:: TestRecursiveCons = RNil | RCons Int TestRecursiveCons
derive class iTask TestRecursiveCons
derive gPrettyTrace TestRecursiveCons

Start world = execTestSuite (testsuite "Core editors" "Tests for core editors" tests) world
where
	tests = 
        [testIntEnter
        ,testIntUpdate
	    ,testRealUpdate
	    ,testConsFieldsUpdate
        ,testMultipleConsesUpdate
        ,testConsesWithFieldTouched
        ,testRecordTouched 
        ,testMaybeIntEnter
        ,testEditConsChange
        ,skip testEditRecursiveConsChange
        ,testEditRecursiveConsChange2
        ,testEditListElement
        ,testAddListElement
        ,testMoveListElementUp
        ,testMoveListElementDown
        ,testMoveFirstListElementDown
        ,testRemoveListElement
        ,testSameInt
        ,testDifferentInt1
        ,testDifferentInt2
        ,testDiffConsFields1
        ,testDiffConsFields2
        ,testDiffRecordFields
        ,skip testDiffConsChange
        ,testDiffConsWithFieldChange
        ,testMaybeIntChangeToJust
        ,testMaybeIntChangeToNothing
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
						,("mode",JSONString "enter")
						]),FieldMask {touched=False,valid=False,state=JSONNull})
	0 Enter

testIntUpdate = testGenUI "Update Int"
	(uia UIIntegerField
		('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid")
						,("hint",JSONString "You have correctly entered a whole number")
						,("taskId",JSONString "STUB")
						,("editorId",JSONString "v")
						,("value",JSONInt 42)
						,("mode",JSONString "update")
						]),FieldMask {touched=False,valid=True,state=JSONInt 42})
	42 Update

testRealUpdate = testGenUI "Update Real"
	(uia UIDecimalField
		('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid")
						,("hint",JSONString "You have correctly entered a decimal number")
						,("taskId",JSONString "STUB")
						,("editorId",JSONString "v")
						,("value",JSONReal 3.14)
						,("mode",JSONString "update")
						]),FieldMask {touched=False,valid=True,state=JSONReal 3.14})
	3.14 Update

testConsFieldsUpdate = testGenUI "Update constructor fields"
	(uiac UICons ('DM'.fromList [("mode",JSONString "update")]) [fieldExp "v0" 1, fieldExp "v1" 2, fieldExp "v2" 3, fieldExp "v3" 4,fieldExp "v4" 5,fieldExp "v5" 6]
		,CompoundMask {fields=[maskExp n \\ n <- [1..6]],state=JSONNull})
	(TestConsFields 1 2 3 4 5 6) Update
where
	fieldExp editorId val = 
		uia UIIntegerField
			('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid"),("hint",JSONString "You have correctly entered a whole number")
							,("taskId",JSONString "STUB"),("editorId",JSONString editorId),("value",JSONInt val),("mode",JSONString "update")
							])
	maskExp n = FieldMask {touched = False, valid = True, state = JSONInt n}

testMultipleConsesUpdate = testGenUI "Update constructor selection"
	(uiac UIVarCons ('DM'.fromList [("mode",JSONString "update")]) [uia UIDropdown
		('DM'.fromList[("taskId",JSONString "STUB"),("editorId",JSONString "v")
					,("value",JSONArray [JSONInt 0])
                    ,("options",JSONArray [JSONObject [("id",JSONInt 0),("text",JSONString "ConsA")],JSONObject [("id",JSONInt 1),("text",JSONString "ConsB")]]) ])]

		,CompoundMask {fields=[FieldMask {touched=False,valid=True,state=JSONInt 0}],state=JSONNull})
	ConsA Update

testConsesWithFieldTouched = skip (testGenUI "Touched constructor with field"
	(uic UIVarCons [consExp,fieldsExp],expMask)
	ConsWithFieldA Update)
where
	consExp = (uia UIDropdown 
		('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid"),("hint",JSONString "You have correctly selected an option")
			,("taskId",JSONString "STUB"),("editorId",JSONString "v")
			,("value",JSONArray [JSONInt 0])
            ,("options",JSONArray [JSONObject [("id",JSONInt 0),("text",JSONString "ConsWithFieldA")],JSONObject [("id",JSONInt 1),("text",JSONString "ConsWithFieldB")]])])
		)
	expMask = CompoundMask {fields=[FieldMask {touched=False,valid=True,state=JSONInt 0}],state=JSONNull}
	fieldsExp = ui UIEmpty//Placeholder

testRecordTouched = skip (
	testGenUI "Touched record"
	(uiac UIRecord ('DM'.fromList [("optional",JSONBool False)]) 
		[intField,stringField,boolField],expMask)
	{ a = 42, b = "Foo", c = True} Update)
where
	expMask = newFieldMask

	intField =
		(uia UIIntegerField
			('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid")
							,("hint",JSONString "You have correctly entered a whole number"),("label",JSONString "a")
							,("taskId",JSONString "STUB")
							,("editorId",JSONString "v0")
							,("value",JSONInt 42)]))
	stringField =
		(uia UITextField
			('DM'.fromList[("optional",JSONBool False),("hint-type",JSONString "valid")
							,("hint",JSONString "You have correctly entered a single line of text"),("label",JSONString "b")
							,("taskId",JSONString "STUB")
							,("editorId",JSONString "v1")
							,("value",JSONString "Foo")
							]))
	boolField =
		(uia UICheckbox 
			('DM'.fromList[("optional",JSONBool False),("label",JSONString "c"),("taskId",JSONString "STUB"),("editorId",JSONString "v2")]))

testMaybeIntEnter = testGenUI "Enter optional Int"
   (uia UIIntegerField ('DM'.fromList[("optional",JSONBool True)
                                     ,("hint-type",JSONString "info")
                                     ,("hint",JSONString "Please enter a whole number")
                                     ,("taskId",JSONString "STUB")
                                     ,("editorId",JSONString "v")
                                     ,("mode",JSONString "enter")
                                     ,("value",JSONNull)]), FieldMask {touched=False,valid=True,state=JSONNull})
		test Enter
where
	test :: Maybe Int
	test = Nothing
testGenEdit :: String (a,EditMask,UIChange) (a,EditMask) (DataPath,JSONNode) -> Test | iTask a & gPrettyTrace{|*|} a
testGenEdit name exp (ov,om) (tp,edit) = assertEqualWorld name (Ok exp) sut
where
	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,nv,vst) = gEditor{|*|}.Editor.onEdit [] (tp,edit) ov om vst
		# world = fromStubIWorld (fromStubVSt vst)
		= (fmap (\(nc,nm) -> (nv,nm,nc)) res,world)

testEditConsChange
	= testGenEdit "Change constructor"
	(ConsB, CompoundMask {CompoundMask|fields=[FieldMask {touched=True,valid=True,state=JSONInt 1}],state=JSONNull},ChangeUI [] [])
	(ConsA, CompoundMask {CompoundMask|fields=[FieldMask {touched=False,valid=False,state=JSONNull}],state=JSONNull})
	([],JSONArray [JSONInt 1])

testEditRecursiveConsChange
	= testGenEdit "Change recursive constructor"
	(RCons 0 RNil, CompoundMask {CompoundMask|fields=[FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=False,valid=False,state=JSONNull},CompoundMask {CompoundMask|fields=[FieldMask {touched=False,valid=False,state=JSONNull}],state=JSONNull}],state=JSONNull},ChangeUI [] [(1,InsertChild intUI),(2,InsertChild newConsUI)])
	(RNil, CompoundMask {CompoundMask|fields=[FieldMask {touched=False,valid=False,state=JSONNull}],state=JSONNull})
	([],JSONArray [JSONInt 1])
where
	intUI = uia UIIntegerField ('DM'.unions [editAttrs "STUB" "v0" Nothing,'DM'.fromList [("hint-type",JSONString "info"),("hint",JSONString "Please enter a whole number (this value is required)"),("optional",JSONBool False),("mode",JSONString "enter")]])
	newConsUI = uic UIVarCons [uia UIDropdown (choiceAttrs "STUB" "v1" [] [JSONObject [("id",JSONInt 0),("text",JSONString "RNil")],JSONObject [("id",JSONInt 1),("text",JSONString "RCons")]])]

testEditRecursiveConsChange2
	= testGenEdit "Change changed recursive constructor"
	(RCons 0 RNil, CompoundMask {CompoundMask|fields=[FieldMask {touched=True,valid=True,state=JSONInt 1}
								                     ,FieldMask {touched=False,valid=False,state=JSONNull}
													 ,CompoundMask {CompoundMask|fields=[FieldMask {touched=True,valid=True,state=JSONInt 0}],state=JSONNull}],state=JSONNull},ChangeUI [] [(2,ChangeChild (ChangeUI [] []))])
	(RCons 0 RNil, CompoundMask {CompoundMask|fields=[FieldMask {touched=True,valid=True,state=JSONInt 1}
                                ,FieldMask {touched=False,valid=False,state=JSONNull}
                                ,CompoundMask {CompoundMask|fields=[FieldMask {touched=False,valid=False,state=JSONNull}],state=JSONNull}],state=JSONNull})
	([1],JSONArray [JSONInt 0])

testEditListElement = testGenEdit "List element edit" 
	([42],CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 42}],state=JSONArray [JSONInt 0]}
		,ChangeUI [] [(0,ChangeChild (ChangeUI [] [(0, ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 42), SetAttribute "hint" (JSONString "You have correctly entered a whole number"), SetAttribute "hint-type" (JSONString "valid")] []))]))])
	([0],CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONNull}],state=JSONArray [JSONInt 0]})
	([0],JSONInt 42)

testAddListElement = testGenEdit "List element add"
	([42,0],CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 42},FieldMask {touched=False,valid=False,state=JSONNull}],state=JSONArray[JSONInt 0,JSONInt 1]}
		,ChangeUI [] [(1,InsertChild elui)

					 ,(2,ChangeChild (ChangeUI [] [(0, ChangeChild (ChangeUI [SetAttribute "value" (JSONString "2 items")] []))]))
					 ,(0,ChangeChild (ChangeUI [] [(2, ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool True)] []))]))
					 ])
	([42],CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 42}],state=JSONArray [JSONInt 0]})
	([],JSONString "add")
where
	elui = uiac UIListItem ('DM'.fromList [("direction",JSONString "horizontal"),("halign",JSONString "right"),("height",JSONString "wrap")]) [inui:buis]
	inui = uia UIIntegerField ('DM'.fromList[("optional",JSONBool False)
                                     ,("hint-type",JSONString "info")
                                     ,("hint",JSONString "Please enter a whole number (this value is required)")
                                     ,("taskId",JSONString "STUB")
                                     ,("editorId",JSONString "v1")
                                     ,("mode",JSONString "enter")
                                     ,("width",JSONString "flex")
                                     ,("value",JSONNull)])
	buis = [uia UIButton ('DM'.fromList [("iconCls",JSONString "icon-up"),("value",JSONString "mup_1"),("enabled",JSONBool True),("taskId",JSONString "STUB"),("editorId",JSONString "v")])
	       ,uia UIButton ('DM'.fromList [("iconCls",JSONString "icon-down"),("value",JSONString "mdn_1"),("enabled",JSONBool False),("taskId",JSONString "STUB"),("editorId",JSONString "v")])
	       ,uia UIButton ('DM'.fromList [("iconCls",JSONString "icon-remove"),("value",JSONString "rem_1"),("taskId",JSONString "STUB"),("editorId",JSONString "v")])
	       ]

testMoveListElementUp = testGenEdit "Move list element up"
	([2,1,3],CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 2},FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=True,valid=True,state=JSONInt 3}],state=JSONArray [JSONInt 1,JSONInt 0,JSONInt 2]}
			,ChangeUI [] [(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool False)] []))]))
                         ,(0,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool True)] []))]))
						 ,(1,MoveChild 0)])
	([1,2,3], CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=True,valid=True,state=JSONInt 2},FieldMask {touched=True,valid=True,state=JSONInt 3}],state=JSONArray [JSONInt 0,JSONInt 1,JSONInt 2]})
	([],JSONString "mup_1")

testMoveListElementDown = testGenEdit "Move list element down"
	([1,3,2],CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=True,valid=True,state=JSONInt 3},FieldMask {touched=True,valid=True,state=JSONInt 2}],state=JSONArray [JSONInt 0,JSONInt 2,JSONInt 1]}
            ,ChangeUI [] [(1,ChangeChild (ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool False)] []))]))
                         ,(2,ChangeChild (ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool True)] []))]))
                         ,(1,MoveChild 2)])

	([1,2,3], CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=True,valid=True,state=JSONInt 2},FieldMask {touched=True,valid=True,state=JSONInt 3}],state=JSONArray [JSONInt 0,JSONInt 1,JSONInt 2]})
	([],JSONString "mdn_1")

testMoveFirstListElementDown = testGenEdit "Move first list element down"
	([2,1,3],CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 2},FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=True,valid=True,state=JSONInt 3}],state=JSONArray [JSONInt 1,JSONInt 0,JSONInt 2]}
            ,ChangeUI [] [(0,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool True)] []))]))
                         ,(1,ChangeChild (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool False)] []))]))
                         ,(0,MoveChild 1)])

	([1,2,3], CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=True,valid=True,state=JSONInt 2},FieldMask {touched=True,valid=True,state=JSONInt 3}],state=JSONArray [JSONInt 0,JSONInt 1,JSONInt 2]})
	([],JSONString "mdn_0")


testRemoveListElement = skip (testGenEdit "Remove list element"
	([1,2],CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=True,valid=True,state=JSONInt 2}],state=JSONArray [JSONInt 0,JSONInt 1]},ChangeUI [] [(1,ChangeChild (ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool False)] []))]))
                             ,(2,RemoveChild)])
	([1,2,3], CompoundMask {fields=[FieldMask {touched=True,valid=True,state=JSONInt 1},FieldMask {touched=True,valid=True,state=JSONInt 2},FieldMask {touched=True,valid=True,state=JSONInt 3}],state=JSONArray [JSONInt 0,JSONInt 1,JSONInt 2]})
	([],JSONString "rem_2"))

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
testDifferentInt1 = skip (testOnRefresh "Different Int 1" (ChangeUI [SetAttribute "value" (JSONInt 23)] []) 23 42 newFieldMask)

testDifferentInt2 :: Test
testDifferentInt2
	= skip (testOnRefresh "Different Int 2"
		(ChangeUI [SetAttribute "value" (JSONInt 3)
				  ,SetAttribute HINT_ATTRIBUTE (JSONString "You need to enter a whole number (this value is required)")
				  ,SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_INVALID)
				  ] [])
			3 42 newFieldMask)

testDiffConsFields1 :: Test
testDiffConsFields1 
	= testOnRefresh "Diff constructor fields 1" 
		(ChangeUI [] [(3,ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 44)] []))])
		(TestConsFields 1 2 3 44 5 6) 
		(TestConsFields 1 2 3 4 5 6)
		(CompoundMask {fields=[newFieldMask,newFieldMask,newFieldMask,newFieldMask,newFieldMask,newFieldMask],state=JSONNull})

testDiffConsFields2 :: Test
testDiffConsFields2 
	= skip (testOnRefresh "Diff constructor fields 2" 
		(ChangeUI [] [(3,ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 44)
											  ,SetAttribute HINT_ATTRIBUTE (JSONString "You have correctly entered a whole number")
											  ,SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_VALID)
											  ] []))])
		(TestConsFields 1 2 3 44 5 6)
		(TestConsFields 1 2 3 4 5 6) 
		(CompoundMask {fields=[newFieldMask,newFieldMask,newFieldMask,newFieldMask,newFieldMask,newFieldMask],state=JSONNull})
	)

testDiffRecordFields :: Test
testDiffRecordFields 
	= testOnRefresh "Diff record fields"
		(ChangeUI [] [(0, ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 23)] [])),(1,ChangeChild (ChangeUI [SetAttribute "value" (JSONString "bar")] []))])
		{TestRecordFields|a=23,b="bar",c=True}
		{TestRecordFields|a=42,b="foo",c=True} 
		(CompoundMask {fields=[newFieldMask,newFieldMask,newFieldMask],state=JSONNull})

testDiffConsChange :: Test
testDiffConsChange 
	= skip (testOnRefresh "Changing a single constructor"
		(ChangeUI [SetAttribute "value" (JSONArray [JSONInt 1,JSONBool True])] [])
		ConsB ConsA newCompoundMask)

testDiffConsWithFieldChange :: Test
testDiffConsWithFieldChange 
	= skip (testOnRefresh "Changing a constructor with a data field"
		(ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "value" (JSONArray [JSONInt 1,JSONBool True])] [])), (1,ChangeChild (ReplaceUI expField))])
		(ConsWithFieldB "Foo") ConsWithFieldA newFieldMask)
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
	= skip (testOnRefresh "Switch Maybe Int Nothing to Just"
		(ChangeUI [SetAttribute "value" (JSONInt 42)
				  ,SetAttribute HINT_ATTRIBUTE (JSONString "You have correctly entered a whole number")
				  ,SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_VALID)
				  ] [])
		(Just 42)
		Nothing
		newFieldMask)

testMaybeIntChangeToNothing :: Test
testMaybeIntChangeToNothing = skip (fail "Switch Maybe Int Just to Nothing")
/*
	= testOnRefresh "Switch Maybe Int Just to Nothing"
		(ChangeUI [("setEditorValue", [JSONNull])] [])
		(Just 42)
		Nothing
*/
