implementation module Tests.Unit.CoreEditors
import TestFramework

import iTasks.UI.Editor, iTasks.UI.Diff, iTasks.UI.Layout
import iTasks._Framework.Generic.Interaction
import iTasks._Framework.IWorld
import qualified Data.Map as DM
import StdMisc

//TEST STUBS
toStubIWorld :: *World -> *IWorld
toStubIWorld world
  = {IWorld
  |server = {serverName = "STUB",serverURL = "//127.0.0.1:80",buildID = "STUB"
        	,paths = {appDirectory = "./STUB/",dataDirectory = "./STUB/",publicWebDirectories = []},customCSS  = False}
  ,config = {sessionTime = 3600, smtpServer = "localhost"}
  ,clocks = {SystemClocks |localDate=defaultValue,localTime=defaultValue,utcDate=defaultValue,utcTime=defaultValue}
  ,current ={TaskEvalState|taskTime= 0,taskInstance= 0,sessionInstance = Nothing,attachmentChain = []
            ,nextTaskNo = 0,eventRoute	= 'DM'.newMap,editletDiffs = 'DM'.newMap}
  ,sdsNotifyRequests = [], memoryShares = 'DM'.newMap, cachedShares = 'DM'.newMap, exposedShares = 'DM'.newMap
  ,jsCompilerState = abort "STUB js compiler state" ,shutdown = False,ioTasks = {done = [], todo = []},ioStates = 'DM'.newMap
  ,world = world
  ,resources = Nothing,random = [],onClient = False }
	
fromStubIWorld :: *IWorld -> *World
fromStubIWorld iworld=:{IWorld|world} = world

toStubVSt :: *IWorld -> *VSt
toStubVSt iworld = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = "STUB", iworld = iworld}

fromStubVSt :: *VSt -> *IWorld
fromStubVSt vst=:{VSt|iworld} = iworld

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

testGenericEditorGenUI :: TestSuite
testGenericEditorGenUI = testsuite "Generic UI generation" "Tests for the cor generic UI generation"
	[testIntUntouched
	,testIntTouched
	,testIntBlanked
	,testRealTouched
	,testConsFieldsTouched 
	,testMultipleConsesTouched
	,testRecordTouched 
	,testMaybeIntUntouched
	]

testGenUI :: String UIContent a InteractionMask -> Test | iTask a
testGenUI name exp x mask = assertEqualWorld name exp sut
where
	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,vst) = gEditor{|*|}.Editor.genUI [] x mask (verifyMaskedValue (x,mask)) [] vst
		# world = fromStubIWorld (fromStubVSt vst)
		= (res,world)

testIntUntouched = testGenUI "Untouched Int" 
	(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","info"),("hint","Please enter a whole number")]}
		(UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v", value = Nothing}))
	42 Untouched

testIntTouched = testGenUI "Touched Int"
	(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a whole number")]}
		(UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v", value = Just (JSONInt 42)}))
	42 Touched

testIntBlanked = testGenUI "Blanked Int"
	(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","invalid"),("hint","You need to enter a whole number (this value is required)")]}
		(UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v", value = Nothing}))
	42 Blanked

testRealTouched = testGenUI "Touched Real"
	(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a decimal number")]}
		(UIEditDecimal {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				       {UIEditOpts|taskId="STUB", editorId="v", value = Just (JSONReal 3.14)}))
	3.14 Touched

testConsFieldsTouched = testGenUI "Touched cons fields"
	(UICompoundEditor {UIEditor|optional=False,attributes='DM'.newMap}
		[fieldExp "v0" 1, fieldExp "v1" 2, fieldExp "v2" 3, fieldExp "v3" 4,fieldExp "v4" 5,fieldExp "v5" 6])
	(TestConsFields 1 2 3 4 5 6) Touched
where
	fieldExp editorId val = 
		(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a whole number")]}
			(UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
					   {UIEditOpts|taskId="STUB", editorId=editorId, value = Just (JSONInt val)}))

testMultipleConsesTouched = testGenUI "Touched constructor selection"
	(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly selected an option")]}
		(UIDropdown  {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
					   {UIChoiceOpts|taskId="STUB", editorId="v", value = [0], options = ["ConsA","ConsB"]}))
	ConsA Touched

testRecordTouched = testGenUI "Touched record"
	(UICompoundEditor {UIEditor|optional=False,attributes='DM'.newMap}
		[intField,stringField,boolField])
	{ a = 42, b = "Foo", c = True} Touched
where
	intField =
		(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a whole number"),("label","A*:")]}
			(UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v0", value = Just (JSONInt 42)}))

	stringField =
		(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("hint-type","valid"),("hint","You have correctly entered a single line of text"),("label","B*:")]}
			(UIEditString {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
					{UIEditOpts|taskId="STUB", editorId="v1", value = Just (JSONString "Foo")}))
	boolField =
		(UIEditor {UIEditor|optional=False,attributes='DM'.fromList[("label","C*:")]}
		(UIEditCheckbox {UIFSizeOpts|margins=Nothing}
				        {UIEditOpts|taskId="STUB", editorId="v2", value = Just (JSONBool True)}))

testMaybeIntUntouched = testGenUI "Untouched optional Int"
		(UIEditor {UIEditor|optional=True,attributes='DM'.fromList[("hint-type","info"),("hint","Please enter a whole number")]}
			(UIEditInt {UIHSizeOpts|width=Nothing,minWidth=Nothing,maxWidth=Nothing,margins=Nothing}
				   {UIEditOpts|taskId="STUB", editorId="v", value = Nothing}))
		test Untouched
where
	test :: Maybe Int
	test = Nothing

testGenericEditorDiffs :: TestSuite
testGenericEditorDiffs = testsuite "Generic diffs" "Tests for the generic diffs"
	[testSameInt
	,testDifferentInt
	,testDiffConsFields
	,testDiffRecordFields
	,testDiffConsChange
	,testMaybeIntChangeToJust
	,testMaybeIntChangeToNothing
	]

//General pattern for diff tests
derive class iTask UIChangeDef, UIStep

testGenDiff :: String UIChangeDef a a -> Test | iTask a
testGenDiff name exp x y = assertEqualWorld name exp sut
where
	sut world 
		# vst = toStubVSt (toStubIWorld world)
		# (res,vst) = gEditor{|*|}.genDiff [] x y vst
		# world = fromStubIWorld (fromStubVSt vst)
		= (res,world)

//Integers
testSameInt :: Test
testSameInt = testGenDiff "Same Int" NoChange 42 42

testDifferentInt :: Test
testDifferentInt = testGenDiff "Different Int" (ChangeUI [("setEditorValue",[JSONInt 23])] []) 42 23

testDiffConsFields :: Test
testDiffConsFields 
	= testGenDiff "Diff constructor fields" 
		(ChangeUI [] [(ItemStep 3,ChangeUI [("setEditorValue",[JSONInt 44])] [])])
		(TestConsFields 1 2 3 4 5 6) (TestConsFields 1 2 3 44 5 6)

testDiffRecordFields :: Test
testDiffRecordFields 
	= testGenDiff "Diff record fields"
		(ChangeUI [] [(ItemStep 0,ChangeUI [("setEditorValue",[JSONInt 23])] []),(ItemStep 1,ChangeUI [("setEditorValue",[JSONString "bar"])] [])])
		{TestRecordFields|a=42,b="foo",c=True}
		{TestRecordFields|a=23,b="bar",c=True}

testDiffConsChange :: Test
testDiffConsChange 
	= testGenDiff "Changing a single constructor"
		(ChangeUI [("setValue",[JSONInt 1,JSONBool True])] [])
		ConsA
		ConsB

testMaybeIntChangeToJust :: Test
testMaybeIntChangeToJust
	= testGenDiff "Switch Maybe Int Nothing to Just"
		(ChangeUI [("setEditorValue", [JSONInt 42])] [])
		Nothing
		(Just 42)

testMaybeIntChangeToNothing :: Test
testMaybeIntChangeToNothing
	= testGenDiff "Switch Maybe Int Just to Nothing"
		(ChangeUI [("setEditorValue", [JSONNull])] [])
		(Just 42)
		Nothing

