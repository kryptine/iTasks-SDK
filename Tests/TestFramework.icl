implementation module TestFramework
import iTasks, StdFile, StdMisc
import iTasks.API.Extensions.Image
import iTasks.UI.Editor, iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Common, iTasks.UI.Definition
import iTasks._Framework.Serialization
import Text, Text.HTML, System.CommandLine
import qualified Data.Map as DM
import iTasks.API.Extensions.Development.Codebase

from iTasks._Framework.IWorld import createIWorld, destroyIWorld, initJSCompilerState, ::IWorld{server}, :: ServerInfo(..), :: SystemPaths(..)
from iTasks._Framework.TaskStore import createTaskInstance, taskInstanceUIChanges
from iTasks._Framework.TaskEval import evalTaskInstance
from iTasks._Framework.Store import flushShareCache, emptyStore
from iTasks._Framework.Util import toCanonicalPath
import iTasks._Framework.Serialization
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import qualified iTasks._Framework.SDS as SDS
from Data.Queue import :: Queue(..)
import System.OS
import iTasks.Util.Trace

// TEST FRAMEWORK
derive class iTask TestSuite, Test, InteractiveTest, TestResult, SuiteResult

gText{|UnitTest|} _ _			            = []
gEditor{|UnitTest|} = emptyEditor 
JSONEncode{|UnitTest|} _ c	   = [dynamicJSONEncode c]
JSONDecode{|UnitTest|} _ [c:r] = (dynamicJSONDecode c,r)
JSONDecode{|UnitTest|} _ r	   = (Nothing,r)
gEq{|UnitTest|} _ _			   = True
gDefault{|UnitTest|}		   = {UnitTest|name="Default unit test",test=pass}
where
	pass :: *World -> *(TestResult,*World)
	pass w = (Passed,w)

//DEFINING TESTS
itest :: String String String (Task a) -> Test | iTask a
itest name instructions expectation tut
  = InteractiveTest {name=name,instructions = instructions, expectation = expectation, taskUnderTest = tut @! ()}

utest :: String (*World -> *(TestResult,*World)) -> Test
utest name test = UnitTest {UnitTest|name=name,test=test}

assert :: String (a -> Bool) a -> Test | gPrettyTrace{|*|} a
assert name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w = (if (exp sut) Passed (Failed (Just ("Actual:\n" +++ (prettyTrace sut)))),w)

assertEqual :: String a a -> Test | gEq{|*|} a & gPrettyTrace{|*|} a
assertEqual name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w = (checkEqual exp sut,w)

assertWorld :: String (a -> Bool) (*World -> *(a,*World)) -> Test | gPrettyTrace{|*|} a
assertWorld name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w 
		# (res,w) = sut w
		= (if (exp res) Passed (Failed (Just ("Actual:\n" +++ (prettyTrace res)))),w)

assertEqualWorld :: String a (*World -> *(a,*World)) -> Test | gEq{|*|} a & gPrettyTrace{|*|} a
assertEqualWorld name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w
		# (res,w) = sut w
		= (if (exp === res) Passed (Failed (Just (sideBySideTrace ("Expected:",exp) ("Actual:",res)))),w)

checkEqual :: a a -> TestResult | gEq{|*|} a & gPrettyTrace{|*|} a
checkEqual exp sut = checkEqualWith (===) exp sut

checkEqualWith :: (a a -> Bool) a a -> TestResult | gPrettyTrace{|*|} a
checkEqualWith pred exp sut = if (pred exp sut) Passed (Failed (Just (sideBySideTrace ("Expected:",exp) ("Actual:", sut))))

pass :: String -> Test
pass name = UnitTest {UnitTest|name=name,test = \w -> (Passed,w)}

fail :: String -> Test
fail name = UnitTest {UnitTest|name=name,test = \w -> (Failed Nothing, w)}

skip :: Test -> Test
skip skipped = UnitTest {UnitTest|name=nameOf skipped,test= \w -> (Skipped,w)}
where
	nameOf (UnitTest {UnitTest|name}) = name
	nameOf (InteractiveTest {InteractiveTest|name}) = name

testsuite :: String String [Test] -> TestSuite
testsuite name description tests
  = {name=name,description=description,tests=tests}

filterSuitesByTestName ::String [TestSuite] -> [TestSuite]
filterSuitesByTestName pattern suites = [{TestSuite|s & tests =filterTestsByName pattern tests} \\ s=:{TestSuite|tests} <- suites]

filterTestsByName :: String [Test] -> [Test]
filterTestsByName pattern tests = filter match tests
where
	match (UnitTest {UnitTest|name}) = indexOf pattern name >= 0
	match (InteractiveTest {InteractiveTest|name}) = indexOf pattern name >= 0

//RUNNING TESTS
testInteractive :: InteractiveTest -> Task TestResult
testInteractive {name,instructions,expectation,taskUnderTest}
	= 	(viewInformation () [] (H1Tag [] [Text name]) <<@ ApplyLayout (setUIAttributes (heightAttr WrapSize)))
	||-	((viewInformation (Title "Instructions") [] instructions)
		  -&&- (viewInformation (Title "Expected result") [] expectation) <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal)))
	||- taskUnderTest
	||- enterInformation (Title "Result") []

//UTILITY TASKS
testEditor :: (Editor a) a EditMode -> Task a | iTask a
testEditor editor model mode
	=   (interact "Editor test" mode null (const ((),model)) (\v l _ -> (l,v,Nothing)) (\_ l v -> (l,v,Nothing)) (Just editor) @ snd
	>&> viewSharedInformation "Editor value" [ViewAs (toString o toJSON)] @? tvFromMaybe
	)  <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal) )

testEditorWithShare :: (Editor a) a EditMode -> Task a | iTask a
testEditorWithShare editor model mode = (withShared model
	\smodel ->
		updateSharedInformation "Edit the shared source" [] smodel 
		||-
	    interact "Editor under test" mode smodel (\r -> ((),r))
												 (\v l _ -> (l,v,Just (\_ -> v)))
												 (\r l v -> (l,r,Nothing)) (Just editor) @ snd
	) <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal)) 

testCommonInteractions :: String -> Task a | iTask a
testCommonInteractions typeName
	= 	 enterInformation ("Enter","Enter information of type " +++ typeName) []
	-||- updateInformation ("Update","Update default value of type " +++ typeName) [] defaultValue
	-||- (withShared defaultValue
			\s -> (updateSharedInformation ("Update shared","Update shared value of type " +++ typeName) [] s
				   -||
				   viewSharedInformation ("View shared","View shared value of type " +++ typeName) [] s
				  )
		 )

testTaskOutput :: String (Task a) [Either Event Int] [UIChange] ([UIChange] [UIChange] -> TestResult) -> Test | iTask a
testTaskOutput name task events exp comparison = utest name test
where
	test world 
		# (argv,world) = getCommandLine world
		# (appPath,world) = toCanonicalPath (hd argv) world
		# iworld = createIWorld "TEST" appPath Nothing Nothing Nothing world
		//Initialize JS compiler support
		# (res,iworld) = initJSCompilerState iworld
		| res =:(Error _)
			= (Failed (Just (fromError res)),destroyIWorld iworld)
		//Empty the store to make sure that we get a reliable task instance no 1
		# iworld = emptyStore iworld
		//Create an instance with autolayouting disabled at the top level
		# (res,iworld) = createTaskInstance task iworld
		= case res of
			(Ok (instanceNo,instanceKey))
				//Apply all events
				# (res,iworld) = applyEvents instanceNo events iworld 
				= case res of
					(Ok ())
						//Collect output
						# (res,iworld) = 'SDS'.read (sdsFocus instanceNo taskInstanceUIChanges) iworld
						# world = destroyIWorld iworld
						//Compare result
						# verdict = case res of
							Ok queue = comparison exp (toList queue)
							(Error (_,e)) = Failed (Just e)
						= (verdict,world)
					(Error e)
						# world = destroyIWorld iworld
						= (Failed (Just e),world)
			(Error (_,e)) 	
				# world = destroyIWorld iworld
				= (Failed (Just e),world)

	applyEvents _ [] iworld = (Ok (),iworld)
	applyEvents instanceNo [Left e:es] iworld
		= case evalTaskInstance instanceNo e iworld of
			(Ok _,iworld) = applyEvents instanceNo es iworld
			(Error e,iworld) = (Error e,iworld)
	applyEvents instanceNo [Right e:es] iworld
		//Wait between events
		# iworld = (sleep e) iworld
		= applyEvents instanceNo es iworld

	//SHOULD BE IN Data.Queue
	toList (Queue front rear) = front ++ reverse rear

	//TODO: Do this with a platform independent standard function
	sleep secs iworld = IF_POSIX (sleep_posix secs iworld) iworld
	sleep_posix secs iworld
		# x = sleep` secs
		| x == 0 && x <> 0 = undef
		= iworld
	where
       sleep` :: !Int -> Int
       sleep` secs = code {
          ccall sleep "I:I"
       }

allPassed :: TestReport -> Bool
allPassed suiteResults = all (checkSuiteResult (\r -> r =: Passed)) suiteResults

noneFailed :: TestReport -> Bool
noneFailed suiteResults = all (checkSuiteResult (\r -> r =: Passed || r =: Skipped)) suiteResults

checkSuiteResult :: (TestResult -> Bool) SuiteResult -> Bool
checkSuiteResult f {SuiteResult|testResults} = all (\(_,r) -> f r) testResults

runTests :: [TestSuite] -> Task ()
runTests suites = application {WebImage|src="/testbench.png",alt="iTasks Testbench",width=200, height=50}
    ( allTasks [runInteractiveTests <<@ Title "Interactive Tests"
			   ,runUnitTests        <<@ Title "Unit Tests"
			   ,viewQualityMetrics  <<@ Title "Metrics"
			   ] <<@ ArrangeWithTabs
    ) @! ()
where
	runInteractiveTests
		= ( editSelection (Title "Select test") False (SelectInTree toTree selectTest) suites [] @? tvHd
		>&> withSelection (viewInformation () [] "Select a test") testInteractive ) <<@ ArrangeWithSideBar 0 LeftSide 250 True @! ()

	toTree suites = reverse (snd (foldl addSuite (0,[]) suites))
	addSuite (i,t) {TestSuite|name,tests}
		| isEmpty [t \\ InteractiveTest t <- tests]  = (i,t) //There are no interactive tests in the suite
		# (i,children) = foldl addTest (i,[]) tests
		= (i, [{ChoiceNode|id = -1 * i, label=name, expanded=False, icon=Nothing, children=reverse children}:t])

	addTest (i,t) (InteractiveTest {InteractiveTest|name})
		= (i + 1, [{ChoiceNode|id = i, label=name, expanded=False, icon=Nothing, children=[]}:t])
	addTest (i,t) _ = (i,t)

	selectTest suites [idx] 
		| idx >= 0  = [(flatten [[t \\ InteractiveTest t <- s.TestSuite.tests] \\ s <- suites]) !! idx]
		| otherwise = []
	selectTest _ _ = []

	runUnitTests
		= 	accWorld (runUnitTestsWorld suites)
		>>- viewInformation () [ViewUsing toHtml (htmlView 'DM'.newMap)]
		@! ()

	toHtml results
		= DivTag [] [suiteHtml res \\ res <- results | not (isEmpty res.testResults)]
	where
		suiteHtml {suiteName,testResults}
			=  DivTag [] [H2Tag [] [Text suiteName]
						 ,TableTag [StyleAttr "width: 100%"] [headerRow:map resultRow testResults]
						 ]

		headerRow = TrTag [] [ThTag [] [Text "Test"],ThTag [] [Text "Result"],ThTag [] [Text "Details"]]

		resultRow (test,Passed) = TrTag [] [TdTag [] [Text test],TdTag [] [SpanTag [StyleAttr "color: green"] [Text "Passed"]],TdTag [] []]
		resultRow (test,Skipped) = TrTag [] [TdTag [] [Text test],TdTag [] [SpanTag [StyleAttr "color: orange"] [Text "Skipped"]],TdTag [] []]
		resultRow (test,Failed Nothing) = TrTag [] [TdTag [] [Text test],TdTag [] [SpanTag [StyleAttr "color: red"] [Text "Failed"]],TdTag [] []]
		resultRow (test,Failed (Just details)) = TrTag [] [TdTag [] [Text test],TdTag [] [SpanTag [StyleAttr "color: red"] [Text "Failed"]],TdTag [] [TextareaTag [] [Text details]]]

	application header mainTask
		= (viewInformation () [] header ||- mainTask) <<@ ArrangeWithSideBar 0 TopSide 50 False <<@ ApplyLayout (setUIType UIContainer)

	viewQualityMetrics :: Task ()
	viewQualityMetrics 
		= 	analyzeITasksCodeBase
		>>- viewInformation () [ViewAs view]  @! ()
	where
		view {numTODO,numFIXME} = UlTag [] [LiTag [] [Text "Number of TODO's found: ",Text (toString numTODO)]
										   ,LiTag [] [Text "Number of FIXME's found: ",Text (toString numFIXME)]
										   ]
//Begin metrics 
//The following section should probably be moved to a separate module
:: SourceTreeQualityMetrics =
	{ numTODO  :: Int
	, numFIXME :: Int
	}
derive class iTask SourceTreeQualityMetrics 

analyzeITasksCodeBase :: Task SourceTreeQualityMetrics 
analyzeITasksCodeBase
	= 	rescanCodeBase [{name="iTasks",rootPath=".."</>"Server",subPaths=[],readOnly=True,modules=[]}]
	@   listFilesInCodeBase
	>>- \files -> allTasks (map determineQualityMetrics files) @ aggregate
where
	aggregate ms = foldr (+) zero ms

determineQualityMetrics :: CleanFile -> Task SourceTreeQualityMetrics
determineQualityMetrics file = importTextFile (cleanFilePath file) @ analyze
where
	analyze text = {numTODO=num "TODO" text ,numFIXME=num "FIXME" text}
	num needle text = length (split needle text) - 1

instance zero SourceTreeQualityMetrics where zero = {numTODO=0,numFIXME=0}
instance + SourceTreeQualityMetrics where (+) {numTODO=xt,numFIXME=xf} {numTODO=yt,numFIXME=yf} = {numTODO = xt+yt, numFIXME= xf+yf}

//End metrics 


runUnitTestsWorld :: [TestSuite] *World -> *(!TestReport,!*World)
runUnitTestsWorld suites world = foldr runSuite ([],world) suites
where
	runSuite {TestSuite|name,tests} (report,world)
		# (testResults,world) = foldr runTest ([],world) [t \\ UnitTest t <- tests]
		= ([{SuiteResult|suiteName=name,testResults=testResults}:report],world)

	runTest {UnitTest|name,test} (results,world)
		# (result,world) = test world
		= ([(name,result):results],world)

runUnitTestsCLI :: [TestSuite] *World -> *World
runUnitTestsCLI suites world
	# (console,world)	       = stdio world
	# (report,(console,world)) = foldl runSuite ([],(console,world)) suites
	# (_,world)			       = fclose console world
	# world 			       = setReturnCode (if (noneFailed report) 0 1) world
    = world
where	
	runSuite (report,(console,world)) {TestSuite|name,tests}
		# console = fwrites ("===[ "+++ name +++ " ]===\n") console
		# (testResults,(console,world)) = foldl runTest ([],(console,world)) [t \\ UnitTest t <- tests]
		= ([{SuiteResult|suiteName=name,testResults=reverse testResults}:report],(console,world))
		
	runTest (results,(console,world)) {UnitTest|name,test}  
		# console = fwrites (name +++ "... ") console
		# (result,world) = test world
		# (console,world) = case result of
			Passed
				# console = fwrites (green "PASSED\n") console
				= (console,world)
			Failed Nothing
				# console = fwrites (red "FAILED\n") console
				= (console,world)
			Failed (Just msg)
				# console = fwrites (red ("FAILED\n" +++msg+++"\n")) console
				= (console,world)
			Skipped
				# console = fwrites (yellow "SKIPPED\n") console
				= (console,world)
		= ([(name,result):results],(console,world))

	//ANSI COLOR CODES -> TODO: Create a library in clean-platform for ANSI colored output
	red s = toString [toChar 27,'[','3','1','m'] +++ s +++ toString [toChar 27,'[','0','m']
	green s = toString [toChar 27,'[','3','2','m'] +++ s +++ toString [toChar 27,'[','0','m']
	yellow s = toString [toChar 27,'[','3','3','m'] +++ s +++ toString [toChar 27,'[','0','m']

runUnitTestsJSON :: [TestSuite] *World -> *World
runUnitTestsJSON suites world
	# (report,world) 	= runUnitTestsWorld suites world
	# (console,world)	= stdio world
	# console 			= fwrites (toString (toJSON report)) console
	# (_,world)			= fclose console world
	# world 			= setReturnCode (if (noneFailed report) 0 1) world
	= world

