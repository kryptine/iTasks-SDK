implementation module TestFramework
import iTasks, StdFile
import iTasks.API.Extensions.Image
import iTasks.UI.Editor, iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Common, iTasks.UI.Definition
import iTasks._Framework.Serialization
import Text, Text.HTML, System.CommandLine
import qualified Data.Map as DM

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

assert :: String (a -> Bool) a -> Test | JSONEncode{|*|} a
assert name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w = (if (exp sut) Passed (Failed (Just ("Actual: " <+++ (toJSON sut)))),w)

assertEqual :: String a a -> Test | gEq{|*|} a & JSONEncode{|*|} a 
assertEqual name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w = (if (exp === sut) Passed (Failed (Just ("Expected: " <+++ (toJSON exp) <+++ "\nActual:   " <+++ (toJSON sut)))),w)

assertWorld :: String (a -> Bool) (*World -> *(a,*World)) -> Test | JSONEncode{|*|} a
assertWorld name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w 
		# (res,w) = sut w
		= (if (exp res) Passed (Failed (Just ("Actual: " <+++ (toJSON res)))),w)

assertEqualWorld :: String a (*World -> *(a,*World)) -> Test | gEq{|*|} a & JSONEncode{|*|} a
assertEqualWorld name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w
		# (res,w) = sut w
		= (if (exp === res) Passed (Failed (Just ("Expected: " <+++ (toJSON exp) <+++ "\nActual:   " <+++ (toJSON res)))),w)

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
	= 	(viewInformation () [] (H1Tag [] [Text name]) <<@ ApplyLayout (setAttributes (heightAttr WrapSize)))
	||-	((viewInformation (Title "Instructions") [] instructions)
		  -&&- (viewInformation (Title "Expected result") [] expectation) <<@ ApplyLayout (setAttributes (directionAttr Horizontal)))
	||- taskUnderTest
	||- enterInformation (Title "Result") []

//UTILITY TASKS
testEditor :: (Editor a) a EditMode -> Task a | iTask a
testEditor editor model mode
	=   (interact "Editor test" mode null (const ((),model)) (\v l _ -> (l,v,Nothing)) (\_ l v -> (l,v,Nothing)) (Just editor) @ snd
	>&> viewSharedInformation "Editor value" [ViewAs (toString o toJSON)] @? tvFromMaybe
	) <<@ ApplyLayout (setAttributes (directionAttr Horizontal))

testEditorWithShare :: (Editor a) a EditMode -> Task a | iTask a
testEditorWithShare editor model mode = (withShared model
	\smodel ->
		updateSharedInformation "Edit the shared source" [] smodel 
		||-
	    interact "Editor under test" mode smodel (\r -> ((),r))
												 (\v l _ -> (l,v,Just (\_ -> v)))
												 (\r l v -> (l,r,Nothing)) (Just editor) @ snd
	) <<@ ApplyLayout (setAttributes (directionAttr Horizontal))

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

allPassed :: TestReport -> Bool
allPassed suiteResults = all suitePassed suiteResults

suitePassed :: SuiteResult -> Bool
suitePassed {SuiteResult|testResults} = all (\(_,r) -> r =: Passed) testResults

runTests :: [TestSuite] -> Task ()
runTests suites = application {WebImage|src="/testbench.png",alt="iTasks Testbench",width=200, height=50}
    ( allTasks [runInteractiveTests <<@ Title "Interactive Tests"
			   ,runUnitTests        <<@ Title "Unit Tests"
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
		= (viewInformation () [] header ||- mainTask) <<@ ArrangeWithSideBar 0 TopSide 50 False <<@ ApplyLayout (setNodeType UIContainer)

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
	# world 			       = setReturnCode (if (allPassed report) 0 1) world
    = world
where	
	runSuite (report,(console,world)) {TestSuite|name,tests}
		# console = fwrites ("===[ "+++ name +++ " ]===\n") console
		# (testResults,(console,world)) = foldr runTest ([],(console,world)) [t \\ UnitTest t <- tests]
		= ([{SuiteResult|suiteName=name,testResults=testResults}:report],(console,world))
		
	runTest {UnitTest|name,test} (results,(console,world)) 
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
	# world 			= setReturnCode (if (allPassed report) 0 1) world
	= world

