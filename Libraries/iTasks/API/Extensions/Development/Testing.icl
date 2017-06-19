implementation module iTasks.API.Extensions.Development.Testing
import iTasks
import iTasks.API.Extensions.Development.Tools
import iTasks._Framework.Test.Definition
import Text

derive class iTask ExitCode

TESTS_PATH :== "../Tests/TestPrograms"

//Copy-paste.. should be in library
runTestModule :: FilePath -> Task SuiteResult
runTestModule path
	= 	get cpmExecutable 
	>>- \cpm ->
	    withShared [] ( \io -> (
			 runWithOutput cpm [prj] (Just baseDir) io //Build the test
		>>- \res -> case res of
			(ExitCode 0,_) = runWithOutput exe [] Nothing io @ parseSuiteResult //Run the test
			(_,output)     = return {SuiteResult|suiteName=path,testResults=[("build",Failed (Just ("Failed to build " +++ prj +++ "\n" +++ output)))]}
	   )
	  )
where
	baseDir = takeDirectory path
	base = dropExtension path
	exe = addExtension base "exe"
	prj = addExtension base "prj"

	runWithOutput prog args dir out 
		= externalProcess prog args dir out {onStartup=onStartup,onOutData=onOutData,onErrData=onErrData,onShareChange=onShareChange,onExit=onExit}	
	where
		onStartup r = (Ok (ExitCode 0,""), Nothing, [], False) 
		onOutData data (e,o) r = (Ok (e,o +++ data), Just (r ++ [data]), [], False)
		onErrData data l r = (Ok l, Nothing, [], False)
		onShareChange  l r = (Ok l, Nothing, [], False)
		onExit ecode (_,o) r =  (Ok (ecode,o), Nothing)

	parseSuiteResult :: (ExitCode,String) -> SuiteResult //QUICK AND DIRTY PARSER
	parseSuiteResult (ExitCode ecode,output)
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
