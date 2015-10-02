implementation module TestFramework
import iTasks

// TEST FRAMEWORK
derive class iTask TestResult, TestSuite, InteractiveTest

//DEFINING TESTS

interactive :: String String String (Task a) -> InteractiveTest | iTask a
interactive name instructions expectation tut
  = {name=name,instructions = Note instructions, expectation = Note expectation, taskUnderTest = tut @! ()}

testsuite :: String String [InteractiveTest] -> TestSuite
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
	= allTasks [(testInteractive t <<@ ForceLayout) <<@ Title t.InteractiveTest.name \\ t <- tests] <<@ ArrangeWithTabs

runTests :: [TestSuite] -> Task [TestResult]
runTests suites =
		enterChoice ("Suite selection","Which tests do you want to run?") [ChooseWith (ChooseFromRadioButtons (\{TestSuite|name} -> name))] suites
	>>= testFullSuite

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
