implementation module TestFramework
import iTasks, StdFile
import iTasks.UI.Editor, iTasks.UI.Diff

// TEST FRAMEWORK
derive class iTask TestResult, TestSuite, Test, InteractiveTest

gText{|UnitTest|} _ _			            = []
gEditor{|UnitTest|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ _ vst			    = (UIEmpty {UIEmpty|actions=[]},vst)
	genDiff _ _ _ vst 				= (NoChange, vst)
	appDiff _ _ val mask ust 		= (val,mask,ust)

gEditMeta{|UnitTest|} _ 	   = [{label=Just "Unit test",hint=Nothing,unit=Nothing}]
gVerify{|UnitTest|} _ mv 	   = alwaysValid mv
JSONEncode{|UnitTest|} _ c	   = [dynamicJSONEncode c]
JSONDecode{|UnitTest|} _ [c:r] = (dynamicJSONDecode c,r)
JSONDecode{|UnitTest|} _ r	   = (Nothing,r)
gEq{|UnitTest|} _ _			   = True
gDefault{|UnitTest|}		   = {UnitTest|name="Default unit test",test=pass}
where
	pass :: *World -> *(TestResult,*World)
	pass w = (Passed,w)
//DEFINING TESTS

interactive :: String String String (Task a) -> Test | iTask a
interactive name instructions expectation tut
  = InteractiveTest {name=name,instructions = Note instructions, expectation = Note expectation, taskUnderTest = tut @! ()}

assertEqual :: String a a -> Test | gEq{|*|} a & gText{|*|} a 
assertEqual name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w = (if (exp === sut) Passed (Failed (Just (Note ("Expected: " <+++ exp <+++ "\nActual:   " <+++ sut)))),w)

assertEqualWorld :: String a (*World -> *(a,*World)) -> Test | gEq{|*|} a & gText{|*|} a
assertEqualWorld name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w
		# (res,w) = sut w
		= (if (exp === res) Passed (Failed (Just (Note ("Expected: " <+++ exp <+++ "\nActual:   " <+++ res)))),w)

skip :: String -> Test
skip name = UnitTest {UnitTest|name=name,test=test}
where
	test w = (Skipped,w)

testsuite :: String String [Test] -> TestSuite
testsuite name description tests
  = {name=name,description=Note description,tests=tests}

//RUNNING TESTS
testInteractive :: InteractiveTest -> Task TestResult
testInteractive {name,instructions,expectation,taskUnderTest}
	= 	viewInformation () [] (H1Tag [] [Text name]) 
	||-	((viewInformation (Title "Instructions") [] instructions <<@ ForceLayout)
		  -&&- (viewInformation (Title "Expected result") [] expectation <<@ ForceLayout ) <<@ ArrangeHorizontal )
	||- taskUnderTest
	||- enterInformation (Title "Result") []

testFullSuite :: TestSuite -> Task [TestResult]
testFullSuite suite=:{TestSuite|tests=tests}
	= allTasks [(testInteractive t <<@ ForceLayout) <<@ Title t.InteractiveTest.name \\ InteractiveTest t <- tests] <<@ ArrangeWithTabs

//UTILITY TASKS
testEditors :: String -> Task a | iTask a
testEditors typeName
	= 	 enterInformation ("Enter","Enter information of type " +++ typeName) []
	-||- updateInformation ("Update","Update default value of type " +++ typeName) [] defaultValue
	-||- (withShared defaultValue
			\s -> (updateSharedInformation ("Update shared","Update shared value of type " +++ typeName) [] s
				   -||
				   viewSharedInformation ("View shared","View shared value of type " +++ typeName) [] s
				  )
		 )

runTests :: [TestSuite] -> Task [TestResult]
runTests suites =
		enterChoice ("Suite selection","Which tests do you want to run?") [ChooseWith (ChooseFromRadioButtons (\{TestSuite|name} -> name))] suites
	>>= testFullSuite
where
	testFullSuite :: TestSuite -> Task [TestResult]
	testFullSuite suite=:{TestSuite|tests=tests}
		= allTasks [(testInteractive t <<@ ForceLayout) <<@ Title t.InteractiveTest.name \\ InteractiveTest t <- tests] <<@ ArrangeWithTabs


runUnitTestsCLI :: [TestSuite] *World -> *World
runUnitTestsCLI suites world
	# (console,world)	= stdio world
	# (console,world) 	= foldl runSuite (console,world) suites
	# (_,world)			= fclose console world
	= world
where	
	runSuite (console,world) {TestSuite|name,tests}
		# console = fwrites ("===[ "+++ name +++ " ]===\n") console
		= foldl runTest (console,world) [t \\ UnitTest t <- tests]
		
	runTest (console,world) {UnitTest|name,test}
		# console = fwrites (name +++ "... ") console
		= case (test world) of
			(Passed,world)
				# console = fwrites (green "PASSED\n") console
				= (console,world)
			(Failed Nothing,world)
				# console = fwrites (red "FAILED\n") console
				= (console,world)
			(Failed (Just (Note msg)),world)
				# console = fwrites (red ("FAILED\n" +++msg+++"\n")) console
				= (console,world)
			(Skipped,world)
				# console = fwrites (yellow "SKIPPED\n") console
				= (console,world)

	//ANSI COLOR CODES -> TODO: Create a library in clean-platform for ANSI colored output
	red s = toString [toChar 27,'[','3','1','m'] +++ s +++ toString [toChar 27,'[','0','m']
	green s = toString [toChar 27,'[','3','2','m'] +++ s +++ toString [toChar 27,'[','0','m']
	yellow s = toString [toChar 27,'[','3','3','m'] +++ s +++ toString [toChar 27,'[','0','m']


