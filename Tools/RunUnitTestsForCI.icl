module RunUnitTestsForCI
/* 
*  This program runs all unit tests and outputs their results to stdout
*  This way all testing code specified as tasks can be easily tested in our continous integration pipeline.
*/
import iTasks
import iTasks.Extensions.Development.Testing
import iTasks.Internal.Test.Definition
import System.FilePath

TESTS_PATH :== "../Tests/TestPrograms"

runAllTests
	=   get (mapRead (filter ((==) "icl" o takeExtension)) (sdsFocus TESTS_PATH directoryListing))
	>>- \modules ->
		sequence "Running all tests" [runTestModule (TESTS_PATH </> m) >>- traceValue \\ m <- modules]
	>>- \results ->
		shutDown (if (noneFailed results) 0 1)

Start world = runTasks runAllTests world
