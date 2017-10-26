module CodeQualityMonitor
/**
* This tool supports the task of monitoring the quality of the iTasks codebase.
* It allows you to run test programs and exlore the codebase
*/
import System.OS
import Text, Text.HTML
import qualified Data.Map as DM

import iTasks

import iTasks.Internal.Test.Definition
import iTasks.UI.Definition
import iTasks.UI.Editor, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers
import iTasks.Extensions.Editors.Ace
import iTasks.Extensions.Development.Codebase
import iTasks.Extensions.Development.Testing
import iTasks.Extensions.Development.Tools
import iTasks.Extensions.Image
import iTasks.Extensions.TextFile
import iTasks.Extensions.Document
import iTasks.Extensions.Process

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

inspectCodeQuality :: Task ()
inspectCodeQuality
	= application {WebImage|src="/testbench.png",alt="iTasks Testbench",width=200, height=50}
    	( allTasks [Title "Unit Tests"           @>> runUnitTests 
				   ,Title "Interactive Tests"    @>> runInteractiveTests 
				   ,Title "Example applications" @>> checkExampleApplications
				   ,Title "Code"                 @>> exploreCode 
                   ,Title "Experiment"           @>> inspectCode "module test\nStart = \"Hello World\""
				   ] <<@ ArrangeWithTabs False
		)
where
	application header mainTask
		= (viewInformation () [] header ||- mainTask) <<@ ArrangeWithSideBar 0 TopSide 50 False <<@ ApplyLayout (setUIType UIContainer) @! ()

runInteractiveTests :: Task ()
runInteractiveTests
	= (     editSelection (Title "Select test") False (SelectInTree toTree selectTest) suites [] @? tvHd
		>&> withSelection (viewInformation () [] "Select a test") testInteractive ) <<@ ArrangeWithSideBar 0 LeftSide 250 True @! ()
where
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

testInteractive :: InteractiveTest -> Task TestResult
testInteractive {name,instructions,expectation,taskUnderTest}
    =       (viewInformation () [] (H1Tag [] [Text name]) <<@ ApplyLayout (setUIAttributes (heightAttr WrapSize)))
    ||-     ((viewInformation (Title "Instructions") [] instructions)
             -&&- (viewInformation (Title "Expected result") [] expectation) <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal)))
    ||- taskUnderTest
    ||- enterInformation (Title "Result") []

runUnitTests :: Task ()
runUnitTests = withShared 'DM'.newMap
	\results ->
		(
		 (enterChoiceWithSharedAs () [ChooseFromGrid fst] (testsWithResults results) fst 
		>&> withSelection (viewInformation "Select a test" [] ())
			(\path -> 
				(viewSharedInformation (Title "Code") [ViewUsing id aceTextArea] (sdsFocus (TESTS_PATH </> path) (removeMaybe Nothing fileShare))
				-&&-
				viewSharedInformation (Title "Results") [ViewAs (toTestReport o maybeToList)] (mapRead ('DM'.get path) results) <<@ ArrangeHorizontal)
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
 		tests = mapRead (filter ((==) "icl" o takeExtension)) (sdsFocus TESTS_PATH directoryListing)

	toTestReport results
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

checkExampleApplications = withShared 'DM'.newMap
	\results ->
		(
		 (enterChoiceWithSharedAs () [ChooseFromGrid fst] (examplesWithResults results) fst 
		>&> withSelection (viewInformation "Select an example" [] ())
			(\path -> 
				(viewSharedInformation (Title "Code") [ViewUsing id aceTextArea] (sdsFocus path (removeMaybe Nothing fileShare))
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

exploreCode :: Task ()
exploreCode 
	= ((    ((editSelectionWithShared (Title "Modules") False
				(SelectInTree toModuleSelectTree selectByIndex)
				(sdsFocus LIBRARY_PATH moduleList) (const []) @? tvHd)
			 	-|| viewQualityMetrics
			)
		   >&> withSelection (viewInformation "Select a module" [] ())
                             viewModule 
          )
		@! ()) <<@ ArrangeWithSideBar 0 LeftSide 250 True
where
	selectByIndex nodes indices = [nodes !! i \\ i <- indices | i >= 0 && i < length nodes]

	viewModule (name,MainModule)
		= allTasks
			[viewSharedInformation (Title "Implementation") [] (sdsFocus (LIBRARY_PATH,name) moduleImplementation)
			] <<@ ArrangeWithTabs False

	viewModule (name,AuxModule)
		= allTasks
			[viewSharedInformation (Title "Definition") [ViewAs toCodeTag] (sdsFocus (LIBRARY_PATH,name) moduleDefinition)
			,viewSharedInformation (Title "Implementation") [ViewAs toCodeTag] (sdsFocus (LIBRARY_PATH,name) moduleImplementation)
			] <<@ ArrangeWithTabs False

	toCodeTag lines = PreTag [] [CodeTag [] [RawText (join "\n" lines)]]

//Inspecting individual programs 
:: InspectState
  = { lines       :: [String]
    , executable  :: Maybe Document
    }

derive class iTask InspectState

// To inspect code we need to do a few things:
// We must be able to view it, change it without risk and run it with changes
inspectCode :: String -> Task ()
inspectCode sourceCode = withShared
	(initialInspectState sourceCode)
	(\state ->
		editSourceCode state
		>^* [OnAction (Action "Build") (always (buildExecutable state))
			,OnAction (Action "Run")   (ifValue hasExecutable (\_ -> runProgram state))
			]
    ) @! ()
where
	initialInspectState sourceCode
		= {InspectState
          |lines = split OS_NEWLINE sourceCode
 		  ,executable = Nothing
		  }

	hasExecutable {InspectState|executable} = (executable =: (Just _))

	editSourceCode :: (Shared InspectState) -> Task InspectState
	editSourceCode state
		= updateSharedInformation (Title "Edit code")
			[UpdateUsing (\{InspectState|lines} -> join OS_NEWLINE lines)
                         (\s c -> {InspectState|s & lines = split OS_NEWLINE c})
                         aceTextArea] state

	buildExecutable :: (Shared InspectState) -> Task ()
	buildExecutable state = withTemporaryDirectory 
		( \temporaryDirectory ->
              get state @ (\{InspectState|lines} -> join OS_NEWLINE lines)
          >>-       prepareBuildFiles temporaryDirectory
		  >>- \_ -> runBuildTool temporaryDirectory
		  >>- \_ -> importExecutable temporaryDirectory state
		  @!  ()
        )
	where
		prepareBuildFiles directory sourceCode
			=         exportTextFile (directory </> "test.icl") sourceCode
			>>- \_ -> exportTextFile (directory </> "test.prj") projectTemplate
		
		runBuildTool directory 
			=   get cpmExecutable 
			>>- \cpm -> callProcess () [] cpm ["test.prj"] (Just directory)
		
		importExecutable directory state
			=   importDocument (directory </> "test.exe")
			>>- \executable -> 
                upd (\s -> {InspectState|s & executable = Just executable}) state

	runProgram :: (Shared InspectState) -> Task ()
	runProgram state = withTemporaryDirectory
		(\temporaryDirectory ->
			    get state @ (\{InspectState|executable} -> executable)
			>>-	maybe (throw "Cannot run the program. There is no executable yet")
				      (\executable -> let programPath = temporaryDirectory </> "program.exe" in
						          exportDocument programPath executable 
						>>- \_ -> makeExecutable programPath 
						>>- \_ -> callProcess () [] programPath [] (Just temporaryDirectory)
					  )
		) @! ()
	where
		makeExecutable path = callProcess () [] "chmod" ["+x",path] Nothing

:: SourceTreeQualityMetrics =
	{ numFiles :: Int
    , numLines :: Int
    , numTODO  :: Int
	, numFIXME :: Int
	}

instance zero SourceTreeQualityMetrics
where
	zero =
		{numFiles = 0, numLines = 0, numTODO=0,numFIXME=0}

instance + SourceTreeQualityMetrics
where
	(+) x y = 
		{numFiles = x.numFiles + y.numFiles
		,numLines = x.numLines + y.numLines
		,numTODO  = x.numTODO  + y.numTODO
		,numFIXME = x.numFIXME + y.numFIXME
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
	analyze text = {numFiles = 1, numLines = num OS_NEWLINE text, numTODO=num "TODO" text ,numFIXME=num "FIXME" text}
	num needle text = length (split needle text) - 1

viewQualityMetrics :: Task ()
viewQualityMetrics 
	= 	analyzeITasksCodeBase
	>>- viewInformation (Title "Metrics") [ViewAs view]  @! ()
where
	view {numFiles,numLines,numTODO,numFIXME}
		= UlTag [] [LiTag [] [Text "Number of files: ",Text (toString numFiles)]
				   ,LiTag [] [Text "Number of lines: ",Text (toString numLines)]
				   ,LiTag [] [Text "Number of TODO's found: ",Text (toString numTODO)]
                   ,LiTag [] [Text "Number of FIXME's found: ",Text (toString numFIXME)]
                   ]

Start world = startEngine inspectCodeQuality world

//CREATE THIS WITH CPM LIBRARY
projectTemplate = join OS_NEWLINE
	["Version: 1.4"
	,"Global"
	,"\tProjectRoot: ."
	,"\tTarget: StdEnv"
	,"\tExec: {Project}/test.exe"
	,"\tCodeGen"
	,"\t\tCheckStacks: False"
	,"\t\tCheckIndexes: True"
	,"\tApplication"
    ,"\t\tHeapSize: 20971520"
    ,"\t\tStackSize: 512000"
    ,"\t\tExtraMemory: 8192"
	,"\t\tIntialHeapSize: 204800"
    ,"\t\tHeapSizeMultiplier: 4096"
    ,"\t\tShowExecutionTime: False"
    ,"\t\tShowGC: False"
    ,"\t\tShowStackSize: False"
    ,"\t\tMarkingCollector:	False"
    ,"\t\tDisableRTSFlags: False"
    ,"\t\tStandardRuntimeEnv: True"
    ,"\t\tProfile"
    ,"\t\t\tMemory: False"
    ,"\t\t\tMemoryMinimumHeapSize: 0"
	,"\t\t\tTime: False"
	,"\t\t\tStack: False"
 	,"\t\t\tDynamics:	True"
	,"\t\t\tDescExL: False"
	,"\t\tOutput"
	,"\t\t\tOutput: ShowConstructors"
	,"\t\t\tFont: Monaco"
	,"\t\t\tFontSize:	9"
	,"\t\t\tWriteStdErr: False"
	,"\tLink"
	,"\t\tLinkMethod: Static"
	,"\t\tGenerateRelocations: False"
	,"\t\tGenerateSymbolTable: False"
	,"\t\tGenerateLinkMap: False"
	,"\t\tLinkResources: False"
	,"\t\tResourceSource:"
	,"\t\tGenerateDLL: False"
	,"\t\tExportedNames:"
	,"\tPaths"
	,"\t\tPath: {Project}"
	,"\tPrecompile:"
	,"\tPostlink:"

	,"MainModule"
	,"\tName: test"
	,"\tDir: {Project}"
	,"\tCompiler"
	,"\t\tNeverMemoryProfile: False"
	,"\t\tNeverTimeProfile: False"
	,"\t\tStrictnessAnalysis: True"
	,"\t\tListTypes: StrictExportTypes"
	,"\t\tListAttributes: True"
	,"\t\tWarnings: True"
	,"\t\tVerbose: True"
	,"\t\tReadableABC: False"
	,"\t\tReuseUniqueNodes: True"
	,"\t\tFusion: False"
	]
