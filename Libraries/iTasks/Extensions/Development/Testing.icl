implementation module iTasks.Extensions.Development.Testing
import iTasks
import System.Time
import iTasks.Extensions.Development.Tools
import iTasks.Internal.Test.Definition
import Text, Data.Tuple, Data.Error, System.FilePath, System.OS

TESTS_PATH :== "../Tests/TestPrograms"

//:: CompileError = CompileError !Int

compileTestModule :: FilePath -> Task TestResult
compileTestModule path
	= 	traceValue path >>| get cpmExecutable
	>>- \cpm ->runWithOutput cpm ["project", base, "create"] (Just baseDir)
	>>- \_   ->runWithOutput cpm ["project", prj, "target", "iTasks"] (Just baseDir)
	>>- \_   ->runWithOutput cpm ["project", prj, "exec", base +++ ".exe"] (Just baseDir)
	>>- \_   ->runWithOutput cpm ["project", prj, "set", "-h", "200M", "-s", "2M", "-dynamics"] (Just baseDir)
	>>- \_   ->runWithOutput cpm [prj] (Just baseDir) //Build the test
		@ \(c,o) -> if (passed c o) Passed (Failed (Just ("Failed to build " +++ prj +++ "\n" +++ join "" o)))
where
    //Cpm still returns exitcode 0 on failure, so we have to check the output
	passed 0 o = let lines = split OS_NEWLINE (join "" o) in not (any isErrorLine lines) 
	passed _ _ = False

 	isErrorLine l = startsWith "Error" l || startsWith "Type error" l || startsWith "Parse error" l

	baseDir = takeDirectory path
	base = takeFileName (dropExtension path)
	prj = takeFileName (addExtension base "prj")

//Copy-paste.. should be in library
runTestModule :: FilePath -> Task SuiteResult
runTestModule path
	= compileTestModule path
	>>- \res -> case res of
		Passed = runWithOutput exe [] Nothing @ (parseSuiteResult o appSnd (join "")) //Run the test
	    _      = return {SuiteResult|suiteName=path,testResults=[("build",res)]}
where
	baseDir = takeDirectory path
	base = dropExtension path
	exe = addExtension base "exe"

	parseSuiteResult :: (Int,String) -> SuiteResult //QUICK AND DIRTY PARSER
	parseSuiteResult (ecode,output)
		# lines = split "\n" output
		| length lines < 2 = fallback ecode output
		# suiteName = trim ((split ":" (lines !! 0)) !! 1)
		# results = [parseRes resLines \\ resLines <- splitLines (drop 3 lines) | length resLines >= 2]
		= {SuiteResult|suiteName=suiteName,testResults=results}
	where
		splitLines lines = split` lines [[]]
		where
			split` ["":lines] acc = split` lines [[]:acc]
			split` [l:lines] [h:acc] = split` lines [[l:h]:acc]
			split` [] acc = reverse (map reverse acc)

		parseRes [nameLine,resultLine:descLines]
			# name = trim ((split ":" nameLine) !! 1)
			# result = case resultLine of
				"Result: Passed" = Passed
				"Result: Skipped" = Skipped
				_ = Failed (if (descLines =: []) Nothing (Just (join "\n" descLines)))
			= (name,result)
		parseRes _ = ("oops",Failed Nothing)

		//If we can't parse the output, We'll treat it as a single simple test executable
		fallback 0 _ = {SuiteResult|suiteName="Unknown",testResults=[("executable",Passed)]}
		fallback _ output = {SuiteResult|suiteName="Unknown",testResults=[("executable",Failed (Just output))]}

runWithOutput :: FilePath [String] (Maybe FilePath) -> Task (Int,[String])
runWithOutput prog args dir = withShared ([], []) \out->withShared [] \stdin->
	externalProcess {tv_sec=0,tv_nsec=100000000} prog args dir Nothing stdin out
	>>- \c->get out @ tuple c o fst
