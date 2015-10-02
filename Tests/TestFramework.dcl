definition module TestFramework
import iTasks

:: TestResult = Pass | Fail !(Maybe Note) //Observed behavior
:: InteractiveTest 
	= { name :: String
      , instructions  	:: Note
      , expectation 	:: Note
	  , taskUnderTest 	:: Task ()
	  }

:: TestSuite =
	{ name :: String
	, description :: Note
	, tests :: [InteractiveTest]
	}

derive class iTask TestResult, TestSuite, InteractiveTest

/**
* Convenient wrapper for defining interactive tests
*
* @param The name of the test
* @param Instructions on how to execute the test
* @param A description of the expected results
* @param The task to test
*/
interactive :: String String String (Task a) -> InteractiveTest | iTask a

/**
* Convenient wrapper for defining test suites
*
* @param The name of the test suite
* @param A short description of the test suite
* @param The list of tests that make up the suite
*/
testsuite :: String String [InteractiveTest] -> TestSuite

/**
* Choose a suite test suite and run all tests
*
* @param the list of test suites to choose from
*/
runTests :: [TestSuite] -> Task [TestResult]

/**
* A generic test rig for testing the different editor variants for a type
*
* @param The name of the type to test (e.g. "Int" or "MyADT"
*/
testEditors :: String -> Task a | iTask a
