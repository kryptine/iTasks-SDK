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
toStubVSt iworld = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = "STUB-taskID", layout = autoLayoutRules, iworld = iworld}

fromStubVSt :: *VSt -> *IWorld
fromStubVSt vst=:{VSt|iworld} = iworld

testGenericDiffs :: TestSuite
testGenericDiffs = testsuite "Generic diffs" "Tests for the generic diffs"
	[testSameInt
	,testDifferentInt
	,testDiffConsFields
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

:: TestConsFields = TestConsFields Int Int Int Int Int Int
derive class iTask TestConsFields

testDiffConsFields :: Test
testDiffConsFields 
	= testGenDiff "Constructor fields" 
		(ChangeUI [] [(ItemStep 3,ChangeUI [("setEditorValue",[JSONInt 44])] [])])
		(TestConsFields 1 2 3 4 5 6) (TestConsFields 1 2 3 44 5 6)

