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
import Data.Map.GenJSON

import Testing.TestEvents
import iTasks
import iTasks.Util.Testing
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

UNIT_TESTS_PATH :== "../Tests/Unit"
INTERACTIVE_TESTS_PATH :== "../Tests/Interactive"

LIBRARY_PATH :== "../Libraries"
EXAMPLE_MODULES :== ["../Examples/BasicApiExamples.icl"
                    ,"../Examples/Applications/Incidone/IncidoneCCC.icl"
                    ,"../Examples/Applications/ShipAdventure/main.icl"
                    ,"../Examples/Applications/TheTaxMan/TheTaxMan.icl"
                    ,"../Examples/GIS/LeafletMapExample.icl"
                    ]

derive class iTask EndEventType, Expression

derive gEditor EndEvent, TestLocation, FailReason, FailedAssertion, CounterExample, Relation
derive gEq EndEvent, TestLocation, FailReason, FailedAssertion, CounterExample, Relation
derive gText EndEvent, TestLocation, FailReason, FailedAssertion, CounterExample, Relation

inspectCodeQuality :: Task ()
inspectCodeQuality
	= application {WebImage|src="/testbench.png",alt="iTasks Testbench",width=200, height=50}
    	( allTasks [Title "Unit Tests"           @>> runUnitTests 
				   ,Title "Interactive Tests"    @>> runInteractiveTests 
				 //,Title "Example applications" @>> checkExampleApplications
				   ,Title "Code"                 @>> exploreCode 
                   ,Title "Experiment"           @>> inspectMainModule "test" "module test\nStart = \"Hello World\""
				   ] <<@ ArrangeWithTabs False
		)
where
	application header mainTask
		= (viewInformation [] header ||- mainTask) <<@ ArrangeWithHeader 0 <<@ ApplyLayout (setUIType UIContainer) @! ()

runInteractiveTests :: Task ()
runInteractiveTests
	= (     (Title "Select test") @>> widthAttr FlexSize @>> editSelectionWithShared [SelectMultiple False, SelectInTree fileCollectionToTree selectTest] tests (const []) @? tvHd
		>&> withSelection (viewInformation [] "Select a test") testInteractive ) <<@ ArrangeWithSideBar 0 LeftSide True @! ()
where
	tests = sdsFocus INTERACTIVE_TESTS_PATH (fileCollection [("**/*.icl",ReferenceFile)] True False)

	fileCollectionToTree collection = itemsToTree [] collection
	where
		itemsToTree prefix subCollection = map (itemToTree prefix) ('DM'.toList subCollection)

		itemToTree prefix (name,FileCollection subCollection)
			= {ChoiceNode|id = determineItemId (fileName [name:prefix]) collection, label = name
              , expanded = False, icon = Nothing, children = itemsToTree [name:prefix] subCollection}
		itemToTree prefix (name,_)
			= {ChoiceNode|id = determineItemId (fileName [name:prefix]) collection, label = name
              , expanded = False, icon = Nothing, children = []}

	fileName path = join {OS_PATH_SEPARATOR} (reverse path)

	determineItemId path collection = fromMaybe -1 (elemIndex path (toPaths collection))

	selectTest collection indices = filter (((==) "icl") o takeExtension) (getItems (toPaths collection) indices)

	testInteractive modulePath 
		=   importTextFile (INTERACTIVE_TESTS_PATH </> modulePath) 
		>>- inspectMainModule (dropExtension (dropDirectory modulePath))

runUnitTests :: Task ()
runUnitTests = withShared 'DM'.newMap
	\results ->
	 ((    (((Title "Tests") @>> widthAttr FlexSize @>> editSelectionWithShared
				[SelectMultiple False, SelectInTree toModuleSelectTree selectByIndex]
				(sdsFocus UNIT_TESTS_PATH moduleList) (const []) @? tvHd)
			)
		   >&> withSelection (Hint "Select a test" @>> viewInformation [] ())
                             (viewTest results)
          )
		@! ()) <<@ ArrangeWithSideBar 0 LeftSide True
where
	selectByIndex nodes indices = [nodes !! i \\ i <- indices | i >= 0 && i < length nodes]

	viewTest results (name,_)
		= ((Title "Code" @>> viewSharedInformation [ViewUsing (join "\n") (mapEditorWrite Just aceTextArea)] (sdsFocus (UNIT_TESTS_PATH,name) moduleImplementation))
		-&&-
		  (((Title "Results" @>> viewSharedInformation [ViewAs (toTestReport o maybeToList)] (mapRead ('DM'.get name) results)) <<@ ArrangeHorizontal)
				>^* [OnAction (Action "Run") (always
						(		runTestModule (UNIT_TESTS_PATH,name) <<@ InWindow
							>>- \res -> (upd ('DM'.put name res)) results
						)
					)]
		) @! ()) <<@ ArrangeWithSideBar 1 RightSide True

	toTestReport results
		= DivTag [] [setHtml res \\ res <- results | not (isEmpty results)]
	where
		setHtml testResults
			= TableTag [StyleAttr "width: 100%"] [headerRow:map resultRow testResults]

		headerRow = TrTag [] [ThTag [] [Text "Test"],ThTag [] [Text "Result"],ThTag [] [Text "Details"]]

		resultRow {name,event=Passed,message} = TrTag [] [TdTag [] [Text name],TdTag [] [SpanTag [StyleAttr "color: green"] [Text "Passed"]],TdTag [] [Text message]]
		resultRow {name,event=Skipped,message} = TrTag [] [TdTag [] [Text name],TdTag [] [SpanTag [StyleAttr "color: orange"] [Text "Skipped"]],TdTag [] [Text message]]
		resultRow {name,event=Failed Nothing,message} = TrTag [] [TdTag [] [Text name],TdTag [] [SpanTag [StyleAttr "color: red"] [Text "Failed"]],TdTag [] [Text message]]
		resultRow {name,event=Failed (Just details),message} = TrTag [] [TdTag [] [Text name],TdTag [] [SpanTag [StyleAttr "color: red"] [Text "Failed"]],TdTag [] [TextareaTag [] [Text (toString (toJSON details))]]]

/*
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
			) @! ()) <<@ ArrangeWithSideBar 0 LeftSide True
		)		
where
	examplesWithResults results = mapRead (\(res,examples) -> [(e,'DM'.get e res) \\e <- examples ]) (results |*| examples)
	where
		examples = constShare EXAMPLE_MODULES
*/

exploreCode :: Task ()
exploreCode 
	= ((    (((Title "Modules") @>> widthAttr FlexSize @>> editSelectionWithShared 
				[SelectMultiple False, SelectInTree toModuleSelectTree selectByIndex]
				(sdsFocus LIBRARY_PATH moduleList) (const []) @? tvHd)
			 	-|| viewQualityMetrics
			)
		   >&> withSelection (Hint "Select a module" @>> viewInformation [] ())
                             viewModule 
          )
		@! ()) <<@ ArrangeWithSideBar 0 LeftSide True
where
	selectByIndex nodes indices = [nodes !! i \\ i <- indices | i >= 0 && i < length nodes]

	viewModule (name,MainModule)
		= allTasks
			[(Title "Implementation") @>> viewSharedInformation [] (sdsFocus (LIBRARY_PATH,name) moduleImplementation)
			] <<@ ArrangeWithTabs False

	viewModule (name,AuxModule)
		= allTasks
			[(Title "Definition") @>> viewSharedInformation [ViewAs toCodeTag] (sdsFocus (LIBRARY_PATH,name) moduleDefinition)
			,(Title "Implementation") @>> viewSharedInformation [ViewAs toCodeTag] (sdsFocus (LIBRARY_PATH,name) moduleImplementation)
			] <<@ ArrangeWithTabs False

	toCodeTag lines = PreTag [] [CodeTag [] [Html (join "\n" lines)]]

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

	editSourceCode :: (Shared sds InspectState) -> Task InspectState | RWShared sds
	editSourceCode state
		= Title "Edit code" @>> updateSharedInformation 
			[UpdateSharedUsing (\{InspectState|lines} -> join OS_NEWLINE lines)
                         (\s c -> {InspectState|s & lines = split OS_NEWLINE c})
						 (const o Just)
                         (mapEditorWrite Just aceTextArea)] state

	buildExecutable :: FilePath (Shared sds InspectState) -> Task () | RWShared sds
	buildExecutable temporaryDirectory state = 
              get state @ (\{InspectState|moduleName,lines} -> (moduleName,join OS_NEWLINE lines))
		  >>- \(moduleName,sourceCode) -> 
              prepareBuildFiles temporaryDirectory moduleName sourceCode
		  >-| runBuildTool temporaryDirectory moduleName
		  >-| setExecutable temporaryDirectory moduleName state
		  @!  ()
	where
		prepareBuildFiles directory moduleName sourceCode
			=   exportTextFile (directory </> addExtension moduleName "icl") sourceCode
			>-| exportTextFile (directory </> addExtension moduleName "prj") (projectTemplate moduleName)
		
		runBuildTool directory moduleName
			=   get cpmExecutable 
			>>- \cpm -> callProcess [] cpm [addExtension moduleName "prj"] (Just directory) Nothing
			>>* [OnAction ActionClose (ifStable return)] //Pause after command...
		
		setExecutable directory moduleName state
            = upd (\s -> {InspectState|s & executable = Just (directory </> addExtension moduleName "exe")}) state

	runProgram :: FilePath (Shared sds InspectState) -> Task () | RWShared sds
	runProgram temporaryDirectory state = (
			    get state @ (\{InspectState|executable} -> executable)
			>>-	maybe (throw "Cannot run the program. There is no executable yet")
				      (\executable -> 
									makeExecutable executable
						>-| callProcess [ViewAs view] executable ["-port","8084"] (Just temporaryDirectory) Nothing
						>>* [OnAction ActionClose (always (return ()))] //Pause after command...
					  )
		) @! ()
	where
		makeExecutable path = callProcess [] "chmod" ["+x",path] Nothing Nothing
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
	>>- \a -> (Title "Metrics") @>> viewInformation [ViewAs view] a @! ()
where
	view {numFiles,numLines,numTODO,numFIXME}
		= UlTag [] [LiTag [] [Text "Number of files: ",Text (toString numFiles)]
				   ,LiTag [] [Text "Number of lines: ",Text (toString numLines)]
				   ,LiTag [] [Text "Number of TODO's found: ",Text (toString numTODO)]
                   ,LiTag [] [Text "Number of FIXME's found: ",Text (toString numFIXME)]
                   ]

Start world = doTasks inspectCodeQuality world

//CREATE THIS WITH CPM LIBRARY
projectTemplate moduleName = join OS_NEWLINE
	["Version: 1.4"
	,"Global"
	,"\tProjectRoot: ."
	,"\tTarget: iTasks"
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
