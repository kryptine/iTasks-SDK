module CodeQualityMonitor
/**
* This tool supports the task of monitoring the quality of the iTasks codebase.
* It allows you to run test programs and exlore the codebase
*/
import StdArray
import System.OS
import Text, Text.HTML
import Data.List, Data.Func
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
import iTasks.Extensions.FileCollection

UNIT_TESTS_PATH :== "../Tests/TestPrograms"
INTERACTIVE_TESTS_PATH :== "../Tests/TestPrograms/Interactive"

LIBRARY_PATH :== "../Libraries"
EXAMPLE_MODULES :== ["../Examples/BasicApiExamples.icl"
                    ,"../Examples/Applications/Incidone/IncidoneCCC.icl"
                    ,"../Examples/Applications/ShipAdventure/main.icl"
                    ,"../Examples/Applications/TheTaxMan/TheTaxMan.icl"
                    ,"../Examples/GIS/LeafletMapExample.icl"
                    ]

inspectCodeQuality :: Task ()
inspectCodeQuality
	= application {WebImage|src="/testbench.png",alt="iTasks Testbench",width=200, height=50}
    	( allTasks [Title "Unit Tests"           @>> runUnitTests 
				   ,Title "Interactive Tests"    @>> runInteractiveTests 
				   ,Title "Example applications" @>> checkExampleApplications
				   ,Title "Code"                 @>> exploreCode 
                   ,Title "Experiment"           @>> inspectMainModule "test" "module test\nStart = \"Hello World\""
				   ] <<@ ArrangeWithTabs False
		)
where
	application header mainTask
		= (viewInformation () [] header ||- mainTask) <<@ ArrangeWithSideBar 0 TopSide 50 False <<@ ApplyLayout (setUIType UIContainer) @! ()

runInteractiveTests :: Task ()
runInteractiveTests
	= (     editSelectionWithShared (Title "Select test") False (SelectInTree collectionToTree selectTest) tests (const []) @? tvHd
		>&> withSelection (viewInformation () [] "Select a test") testInteractive ) <<@ ArrangeWithSideBar 0 LeftSide 250 True @! ()
where
	tests = sdsFocus INTERACTIVE_TESTS_PATH (fileCollection (\path isDirectory -> isDirectory || takeExtension path == "icl") False)

	collectionToTree collection = itemsToTree [] collection
	where
		itemsToTree prefix subCollection = map (itemToTree prefix) ('DM'.toList subCollection)

		itemToTree prefix (name,FileContent _)
			= {ChoiceNode|id = determineItemId (fileName [name:prefix]) collection, label = name
              , expanded = False, icon = Nothing, children = []}
		itemToTree prefix (name,FileCollection subCollection)
			= {ChoiceNode|id = determineItemId (fileName [name:prefix]) collection, label = name
              , expanded = False, icon = Nothing, children = itemsToTree [name:prefix] subCollection}

	fileName path = join {OS_PATH_SEPARATOR} (reverse path)

	determineItemId path collection = fromMaybe -1 (elemIndex path (toPaths collection))

	selectTest collection indices = filter (((==) "icl") o takeExtension) (getItems (toPaths collection) indices)

	testInteractive modulePath 
		=   importTextFile (INTERACTIVE_TESTS_PATH </> modulePath) 
		>>- inspectMainModule (dropExtension (dropDirectory modulePath))

runUnitTests :: Task ()
runUnitTests = withShared 'DM'.newMap
	\results ->
		(
		 (enterChoiceWithSharedAs () [ChooseFromGrid fst] (testsWithResults results) fst 
		>&> withSelection (viewInformation "Select a test" [] ())
			(\path -> 
				(viewSharedInformation (Title "Code") [ViewUsing id aceTextArea] (sdsFocus (UNIT_TESTS_PATH </> path) (removeMaybe Nothing fileShare))
				-&&-
				viewSharedInformation (Title "Results") [ViewAs (toTestReport o maybeToList)] (mapRead ('DM'.get path) results) <<@ ArrangeHorizontal)
				>^* [OnAction (Action "Run") (always
						(		runTestModule (UNIT_TESTS_PATH </> path) <<@ InWindow
							>>- \res -> (upd ('DM'.put path res)) results
						)
					)]
			) @! ()) <<@ ArrangeWithSideBar 0 LeftSide 250 True
		)		
where
	testsWithResults results = mapRead (\(res,tests) -> [(t,'DM'.get t res) \\t <- tests]) (results |*| tests)
	where
 		tests = mapRead (filter ((==) "icl" o takeExtension)) (sdsFocus UNIT_TESTS_PATH directoryListing)

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
  = { moduleName  :: String
    , lines       :: [String]
    , executable  :: Maybe FilePath
    }

derive class iTask InspectState

// To inspect code we need to do a few things:
// We must be able to view it, change it without risk and run it with changes
inspectMainModule :: String String -> Task ()
inspectMainModule moduleName sourceCode = withShared
	(initialInspectState moduleName sourceCode)
	(\state -> withTemporaryDirectory
		\workDirectory ->
			editSourceCode state
			>^* [OnAction (Action "Build") (always (buildExecutable workDirectory state))
				,OnAction (Action "Run")   (ifValue hasExecutable (\_ -> runProgram workDirectory state))
				]
    ) @! ()
where
	initialInspectState moduleName ourceCode
		= {InspectState
          |moduleName = moduleName
          ,lines = split OS_NEWLINE sourceCode
 		  ,executable = Nothing
		  }

	hasExecutable {InspectState|executable} = (executable =: (Just _))

	editSourceCode :: (Shared InspectState) -> Task InspectState
	editSourceCode state
		= updateSharedInformation (Title "Edit code")
			[UpdateUsing (\{InspectState|lines} -> join OS_NEWLINE lines)
                         (\s c -> {InspectState|s & lines = split OS_NEWLINE c})
                         aceTextArea] state

	buildExecutable :: FilePath (Shared InspectState) -> Task ()
	buildExecutable temporaryDirectory state = 
              get state @ (\{InspectState|moduleName,lines} -> (moduleName,join OS_NEWLINE lines))
		  >>- \(moduleName,sourceCode) -> 
              prepareBuildFiles temporaryDirectory moduleName sourceCode
		  >>- \_ -> runBuildTool temporaryDirectory moduleName
		  >>- \_ -> setExecutable temporaryDirectory moduleName state
		  @!  ()
	where
		prepareBuildFiles directory moduleName sourceCode
			=         exportTextFile (directory </> addExtension moduleName "icl") sourceCode
			>>- \_ -> exportTextFile (directory </> addExtension moduleName "prj") (projectTemplate moduleName)
		
		runBuildTool directory moduleName
			=   get cpmExecutable 
			>>- \cpm -> callProcess () [] cpm [addExtension moduleName "prj"] (Just directory) Nothing
			>>* [OnAction ActionClose (ifStable return)] //Pause after command...
		
		setExecutable directory moduleName state
            = upd (\s -> {InspectState|s & executable = Just (directory </> addExtension moduleName "exe")}) state

	runProgram :: FilePath (Shared InspectState) -> Task ()
	runProgram temporaryDirectory state = (
			    get state @ (\{InspectState|executable} -> executable)
			>>-	maybe (throw "Cannot run the program. There is no executable yet")
				      (\executable -> 
									makeExecutable executable
						>>- \_ -> callProcess () [ViewAs view] executable ["-port","8084"] (Just temporaryDirectory) Nothing
						>>* [OnAction ActionClose (always (return ()))] //Pause after command...
					  )
		) @! ()
	where
		makeExecutable path = callProcess () [] "chmod" ["+x",path] Nothing Nothing
		view _ = ATag [HrefAttr url,TargetAttr "_blank"] [Text "Running the test program at: ",Text url]
		where
			url = "http://localhost:8084"

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
//Start world = startEngineWithOptions (\cli options -> (Just {options & autoLayout = False},[])) inspectCodeQuality world

//CREATE THIS WITH CPM LIBRARY
projectTemplate moduleName = join OS_NEWLINE
	["Version: 1.4"
	,"Global"
	,"\tProjectRoot: ."
	,"\tTarget: iTasks git"
	,"\tExec: {Project}/" +++ addExtension moduleName "exe"
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
	,"\tName: " +++ moduleName
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
