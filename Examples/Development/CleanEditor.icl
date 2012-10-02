module CleanEditor

import iTasks, Text

:: CCLSource :== String
:: CleanSource :== String

CLEAN_COMPILER = IF_POSIX_OR_WINDOWS "cocl" "CleanCompiler.exe"

demoCCL :: Task CCLSource
demoCCL = monitored editSource [viewDiagram,viewCleanCode] <<@ SetLayout tabbedLayout
where
	viewDiagram src = whileUnchanged src view
	where
		view (NoValue)
			= viewInformation (Icon "diagram","Diagram","No source code yet") [] Void @? const NoValue
		view (Value src _)
			=	viewInformation (Icon "diagram","Diagram","Your CCL code has changed, press continue to regenerate the ORM diagram") [] Void
			>>|	generateDiagram (Icon "diagram","Diagram*","Generating diagram...") src
			>>=	viewInformation (Title "Diagram") [ViewWith previewImageDoc] @? const NoValue
		
	viewCleanCode src = whileUnchanged src view
	where
		view (NoValue)
			= viewInformation (Icon "cleancode","Clean Types","No source code yet") [] Void @? const NoValue
		view (Value src _)
			=	viewInformation (Icon "cleancode","Clean Types","Your CCL code has changed, press continue to regenerate the Clean Types") [] Void
			>>|	generateCleanTypes (Icon "cleanCode","Clean Types*","Generating clean types...") src
			>>= viewInformation (Title "Clean Types") [ViewWith (\s -> Note s)] @? const NoValue
			
editSource :: Task CCLSource
editSource = withShared "concept module ccldemo\n\n## Ccl is awesome" (
	\content ->
			(updateSharedInformation (Icon "cclcode","CCL Source","Specify your CCL model") [UpdateWith (\s -> Note s) (\_ (Note s)-> s)] content)
		-|| loadExampleFromFile content <<@ SetLayout (partLayout 0)
	)

generateDiagram :: d CCLSource -> Task Document | descr d
generateDiagram desc src = withTemporaryDirectory (\tmpDir ->	
		exportTextFile (tmpDir </> "ccldemo.ccl") src
	>>|	callProcess desc [] CLEAN_COMPILER ["-o",tmpDir </> "ccldemo", tmpDir </> "ccldemo.ccl"]
	>>|	importDocument (tmpDir </> "ccldemo.png")
	)

generateCleanTypes :: d CCLSource -> Task CleanSource | descr d
generateCleanTypes desc src = withTemporaryDirectory (\tmpDir ->
		exportTextFile (tmpDir </> "ccldemo.ccl") src
	>>|	callProcess desc [] CLEAN_COMPILER ["-o",tmpDir </> "ccldemo", tmpDir </> "ccldemo.ccl"]
	>>| importTextFile (tmpDir </> "ccldemo.dcl")
	)
	
loadExampleFromFile :: (Shared CCLSource) -> Task CCLSource
loadExampleFromFile content
	= forever (
		chooseAction fileChoices
	>>= importTextFile
	>>= \example -> set example content)
where
	fileChoices	= [(Action ("Examples/" +++ upperCaseFirst d +++ "/" +++ l) ,addExtension ("ccl-examples"</>d</>f) "ccl") \\ (l,d,f) <- constructs ++ application] 
	constructs	= [("Single entity type", "constructs", "SingleEntity")
				  ,("Single value type", "constructs", "SingleLabel")
				  ,("Single fact type", "constructs", "SingleFact")
				  ,("Subtypes", "constructs", "SubTypes")
				  ,("Total role constraint", "constructs", "TotalRole")
				  ,("Uniqueness constraint on 2 roles", "constructs", "TwoRoleUnique")
				  ,("Uniqueness constraint on multiple roles", "constructs", "MultiRoleUnique")
				  ,("Primary role", "constructs","PrimaryRole")
				  ]
	application	= [("Music collection", "application","Music")
				  ,("Conference management", "application", "Conference")
				  ]
	
Start :: *World -> *World
Start world = startEngine demoCCL world

//Util
monitored :: (Task a) [(ReadOnlyShared (TaskValue a)) -> Task a] -> Task a | iTask a
monitored lead monitors
	=	parallel Void [(Embedded, \_ -> lead)
					  :[(Embedded,\s -> t (mapRead hd (toReadOnly (taskListState s))) ) \\ t <- monitors]]
	@?	res
where	
	res (Value [(_,fv):_] _)	= fv
	res _						= NoValue

previewImageDoc :: Document -> HtmlTag
previewImageDoc {Document|contentUrl} = ImgTag [SrcAttr contentUrl]