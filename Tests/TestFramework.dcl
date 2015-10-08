definition module TestFramework
import iTasks

:: TestResult = Pass | Fail !(Maybe Note) //Observed behavior
:: InteractiveTest 
	= { name :: String
      , instructions  	:: Note
      , expectation 	:: Note
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
	, description :: Note
	, tests :: [Test]
	}

derive class iTask TestResult, TestSuite, Test, InteractiveTest

derive JSONEncode UnitTest
derive JSONDecode UnitTest
derive gEq UnitTest
derive gEditor UnitTest
derive gEditMeta UnitTest
derive gVerify UnitTest
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
interactive :: String String String (Task a) -> Test | iTask a


assertEqual :: String a a -> Test | gEq{|*|} a & gText{|*|} a

assertEqualWorld :: String a (*World -> *(a,*World)) -> Test | gEq{|*|} a & gText{|*|} a

/**
* Convenient wrapper for defining test suites
*
* @param The name of the test suite
* @param A short description of the test suite
* @param The list of tests that make up the suite
*/
testsuite :: String String [Test] -> TestSuite

/**
* A generic test rig for testing the different editor variants for a type
*
* @param The name of the type to test (e.g. "Int" or "MyADT"
*/
testEditors :: String -> Task a | iTask a

/**
* Choose a suite test suite and run all tests
*
* @param the list of test suites to choose from
*/
runTests :: [TestSuite] -> Task [TestResult]

/**
* Runs the unit tests from the test suites and shows test
* results on stdout
*/
runUnitTestsCLI :: [TestSuite] *World -> *World
