module RunUnitTestsForCI
/* 
*  This program runs all unit tests and outputs their results to stdout
*  This way all testing code specified as tasks can be easily tested in our continous integration pipeline.
*/
import iTasks
import iTasks.Extensions.Development.Testing
import iTasks.Internal.Test.Definition
import System.FilePath
import System.CommandLine

DEFAULT_TESTS_PATH :== "../Tests/TestPrograms"

runAllTests
	=   determineTestDir
	>>- \testDir ->
        get (mapRead (filter ((==) "icl" o takeExtension)) (sdsFocus testDir directoryListing))
	>>- \modules ->
		sequence "Running all test programs"
			[runTestModule (testDir </> m) >>- traceValue \\ m <- modules]
	>>- \results ->
		shutDown (if (noneFailed results) 0 1)

determineTestDir
	= accWorld getCommandLine
	@ \args -> case args of
		[_,path] = path
		_      = DEFAULT_TESTS_PATH

Start world = runTasks runAllTests world
