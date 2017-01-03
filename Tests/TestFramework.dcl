definition module TestFramework
import iTasks

:: InteractiveTest 
	= { name :: String
      , instructions  	:: String
      , expectation 	:: String
	  , taskUnderTest 	:: Task ()
	  }

:: Test = UnitTest UnitTest
		| InteractiveTest InteractiveTest

:: UnitTest
	= { name :: String
	  , test :: *World -> *(TestResult,*World)
	  }

:: TestSuite =
	{ name :: String
	, description :: String
	, tests :: [Test]
	}

:: TestResult
	= Passed 
	| Failed !(Maybe String) 	//Observed behavior
	| Skipped 				//The test was skipped

:: SuiteResult =
	{ suiteName :: String
	, testResults :: [(String,TestResult)]
	}
	
:: TestReport :== [SuiteResult]
	
derive class iTask TestSuite, Test, InteractiveTest, TestResult, SuiteResult

derive JSONEncode UnitTest
derive JSONDecode UnitTest
derive gEq UnitTest
derive gEditor UnitTest
derive gText UnitTest
derive gDefault UnitTest

/**
* Convenient wrapper for defining interactive tests
*
* @param The name of the test
* @param Instructions on how to execute the test
* @param A description of the expected results
* @param The task to test
*/
itest :: String String String (Task a) -> Test | iTask a

/**
* Convenient wrapper for defining unit tests
*
* @param The name of the test
* @param The task to test
*/
utest :: String (*World -> *(TestResult,*World)) -> Test

assert :: String (a -> Bool) a -> Test | JSONEncode{|*|} a

assertEqual :: String a a -> Test | gEq{|*|} a & JSONEncode{|*|} a

assertWorld :: String (a -> Bool) (*World -> *(a,*World)) -> Test | JSONEncode{|*|} a

assertEqualWorld :: String a (*World -> *(a,*World)) -> Test | gEq{|*|} a & JSONEncode{|*|} a

checkEqual :: a a -> TestResult | gEq{|*|} a & JSONEncode{|*|} a 
checkEqualWith :: (a a -> Bool) a a -> TestResult |JSONEncode{|*|} a 

pass :: String -> Test

fail :: String -> Test

skip :: Test -> Test

/**
* Convenient wrapper for defining test suites
*
* @param The name of the test suite
* @param A short description of the test suite
* @param The list of tests that make up the suite
*/
testsuite :: String String [Test] -> TestSuite

/**
* Filter test suites based on the name of a test
*/

filterSuitesByTestName ::String [TestSuite] -> [TestSuite]
filterTestsByName :: String [Test] -> [Test]

/**
* Test a specific editor
*
* @param The editor to test
* @param The model value that the editor edits
* @param Edit mode to test (View,Enter,Update)
*/
testEditor :: (Editor a) a EditMode -> Task a | iTask a

testEditorWithShare :: (Editor a) a EditMode -> Task a | iTask a

/**
* A generic test rig for testing the different editor variants for a type
*
* @param The name of the type to test (e.g. "Int" or "MyADT"
*/
testCommonInteractions :: String -> Task a | iTask a


testTaskOutput :: String (Task a) [Either Event Int] [UIChange] ([UIChange] [UIChange] -> TestResult) -> Test | iTask a

/**
* Test if all tests have passed
*/
allPassed :: TestReport -> Bool
/**
* Check if no tests have failed (skipped and passed)
*/
noneFailed :: TestReport -> Bool

/**
* Run all tests interactively and run all tests
*
* @param the list of test suites to choose from
*/
runTests :: [TestSuite] -> Task ()

/**
* Runs the unit tests from the test suites and shows test
* results on stdout
*/
runUnitTestsCLI :: [TestSuite] *World -> *World
/**
* Run all unit tests from the test suites and dump
*/
runUnitTestsJSON :: [TestSuite] *World -> *World

