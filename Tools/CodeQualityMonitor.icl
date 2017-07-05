module CodeQualityMonitor
/**
* This tool supports the task of monitoring the quality of the iTasks codebase.
* It allows you to run test programs and exlore the codebase
*/
import System.CommandLine
import System.GetOpt
import Text, Text.HTML
import qualified Data.Map as DM

import iTasks
import iTasks._Framework.Test.Definition
import iTasks.UI.Definition
import iTasks.UI.Editor, iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Common
import iTasks.API.Extensions.Editors.Ace
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.Development.Testing
import iTasks.API.Extensions.Image
import iTasks.API.Extensions.TextFile


import Tests.Interactive.BuiltinEditors
import Tests.Interactive.GenericEditors
import Tests.Interactive.BuiltinContainers
import Tests.Interactive.CustomEditors
import Tests.Interactive.Layout
import Tests.Interactive.Editlets
import Tests.Interactive.CoreTasks
import Tests.Interactive.TaskPatterns
import Tests.Common.MinimalTasks

derive class iTask ExitCode

//CPM_PATH :== "/Users/bas/Clean/bin/cpm"
TESTS_PATH :== "../Tests/TestPrograms"
LIBRARY_PATH :== "../Libraries"
EXAMPLE_MODULES :== ["../Examples/BasicApiExamples.icl"
                    ,"../Examples/Applications/Incidone/IncidoneCCC.icl"
                    ,"../Examples/Applications/c2-demo/main.icl"
                    ,"../Examples/GIS/LeafletMapExample.icl"
                    ]

suites = [//Interactive tests
		  testBuiltinEditors
         ,testBuiltinEditorsWithShares
		 ,testGenericEditors
		 ,testBuiltinContainers
		 ,testCustomEditors
 		 ,testLayoutI
		 ,testEditletsI
         ,testCoreTasksI
         ,testTaskPatternsI
		 ]

//Commandline options 
:: CLIOpt = UnitTestOnly | UseJSON | NameFilter String

runTests :: [TestSuite] -> Task ()
runTests suites = application {WebImage|src="/testbench.png",alt="iTasks Testbench",width=200, height=50}
    ( allTasks [runInteractiveTests <<@ Title "Interactive Tests"
			   ,runUnitTests   <<@ Title "Unit Tests"
               ,checkExampleApplications  <<@ Title "Example applications"
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

	runUnitTests = withShared 'DM'.newMap
		\results ->
		(
		 (enterChoiceWithSharedAs () [ChooseFromGrid fst] (testsWithResults results) fst 
		>&> withSelection (viewInformation "Select a test" [] ())
			(\path -> 
				(viewSharedInformation (Title "Code") [ViewUsing id aceTextArea] (sdsFocus (TESTS_PATH </> path) externalFile)
				-&&-
				viewSharedInformation (Title "Results") [ViewAs (toHtml o maybeToList)] (mapRead ('DM'.get path) results) <<@ ArrangeHorizontal)
				>^* [OnAction (Action "Run") (always
						(		runTestModule (TESTS_PATH </> path) <<@ InWindow
							>>- \res -> (upd ('DM'.put path res)) results
						)
					)]
			) @! ()) <<@ ArrangeWithSideBar 0 LeftSide 250 True
		)		
	where
		testsWithResults results = mapRead (\(res,tests) -> [(t,'DM'.get t res) \\t <- tests]) (results |*| tests)
		where
 			tests = mapRead (filter ((==) "icl" o takeExtension)) (sdsFocus TESTS_PATH externalDirectory)


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

	checkExampleApplications = withShared 'DM'.newMap
		\results ->
		(
		 (enterChoiceWithSharedAs () [ChooseFromGrid fst] (examplesWithResults results) fst 
		>&> withSelection (viewInformation "Select an example" [] ())
			(\path -> 
				(viewSharedInformation (Title "Code") [ViewUsing id aceTextArea] (sdsFocus path externalFile)
				-&&-
				viewSharedInformation (Title "Results") [] (mapRead ('DM'.get path) results) <<@ ArrangeHorizontal)
				>^* [OnAction (Action "Run") (always
						(		compileTestModule path <<@ InWindow
							>>- \res -> (upd ('DM'.put path res)) results
						)
					)]
			) @! ()) <<@ ArrangeWithSideBar 0 LeftSide 250 True
		)		
	where
		examplesWithResults results = mapRead (\(res,examples) -> [(e,'DM'.get e res) \\e <- examples ]) (results |*| examples)
		where
			examples = constShare EXAMPLE_MODULES

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
	= 	rescanCodeBase [{name="iTasks",rootPath=LIBRARY_PATH,subPaths=[],readOnly=True,modules=[]}]
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

testInteractive :: InteractiveTest -> Task TestResult
testInteractive {name,instructions,expectation,taskUnderTest}
    =       (viewInformation () [] (H1Tag [] [Text name]) <<@ ApplyLayout (setUIAttributes (heightAttr WrapSize)))
    ||-     ((viewInformation (Title "Instructions") [] instructions)
             -&&- (viewInformation (Title "Expected result") [] expectation) <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal)))
    ||- taskUnderTest
    ||- enterInformation (Title "Result") []

Start world
	# (args,world) = getCommandLine world
	# (options,args,errors) = getOpt Permute [unitOpt,jsonOpt,nameFilterOpt] args
	# unitOnly     = not (isEmpty [UnitTestOnly \\ UnitTestOnly <- options])
	# useJSON      = not (isEmpty [UseJSON \\ UseJSON <- options])
	# nameFilter   = listToMaybe  [f \\ NameFilter f <- options]
	# suites       = maybe suites (\f -> filterSuitesByTestName f suites) nameFilter
	| unitOnly 
		= (if useJSON runUnitTestsJSON runUnitTestsCLI) suites world
	| otherwise
		= startEngine [publish "/" (\_ -> runTests suites <<@ ApplyLayout (setUIAttributes (titleAttr "iTasks Testbench")))
					  :layoutTestTasks] world
where
	unitOpt = Option [] ["unit"] (NoArg UnitTestOnly) "Only run unit tests and show output on console"
	jsonOpt = Option [] ["json"] (NoArg UseJSON) "Output testresults as JSON"
	nameFilterOpt = Option [] ["name"] (ReqArg NameFilter "*") "Only run tests that match a specific name"
