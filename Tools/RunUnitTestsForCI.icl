module RunUnitTestsForCI
/* 
*  This program runs all unit tests and outputs their results to stdout
*  This way all testing code specified as tasks can be easily tested in our continous integration pipeline.
*/
import iTasks
import iTasks.API.Extensions.Development.Testing

TESTS_PATH :== "../Tests/TestPrograms"

runAllTests
	=   get (mapRead (filter ((==) "icl" o takeExtension)) (sdsFocus TESTS_PATH externalDirectory))
	>>- \modules ->
		sequence "Running all tests" [runTestModule (TESTS_PATH </> m) >>- traceValue \\ m <- modules]

Start world = runTasks runAllTests world
