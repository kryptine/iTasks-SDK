module CleanEditor

import iTasks, Text

Start :: *World -> *World
Start world = startEngine ide world 

ide = 				parallel Void //(Title "Clean IDE") 	
						[ (Embedded, topmenu)
						, (Embedded, project)
						, (Embedded, messages (cleanPath +++ errorFile))
						] <<@ SetLayout layout @ const Void
where
	layout = customMergeLayout (sideMerge TopSide 0 (sideMerge LeftSide 150 (sideMerge BottomSide 100 tabbedMerge)))

project _
	=				viewInformation "Project" [] "dummy" @ const Void

messages errorFileName ts 
	= 				viewSharedInformation "Error Messages..." [ViewWith (\txt -> Note txt)] (externalFile errorFileName)
					@ const Void 
	  

topmenu ts
	=				actionTask 
 	>>*				[ OnAction (Action "File/Open") always (const (launch openFile))
 					, OnAction (Action "File/Quit") always (const (return Void))
 					, OnAction (Action "Project/Bring Up To Date") always (const (launch (compile projectName)))
					]  
where
	launch task = appendTask Embedded task ts >>| topmenu ts


openFile _
	=				updateInformation "Give name of text file you want to open..." [] ""
	>>*				[ OnAction ActionCancel 		always   (const (return Void))
					, OnAction (Action "Open File") hasValue (editor o getValue)
					]

:: CCLSource :== String
:: CleanSource :== String

batchBuild		:== "BatchBuild.exe"
cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"

projectPath		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\iTasks-SDK\\Examples\\Development\\"
projectName		:==	"test"


errorFile		:== "Temp\\errors"


compile projectName _
	=				callProcess "Clean Compiler" [] (cleanPath +++ batchBuild) [projectPath +++ projectName +++ ".prj"] @ const Void


// editor

:: Statistics = {lineCount :: Int
				,wordCount :: Int
				}
:: Replace	 =	{ search  	:: String
				, replaceBy :: String
				}
derive class iTask Statistics, Replace

initReplace =	{ search = ""
				, replaceBy = "" 
				}

editor fileName = editor` (externalFile fileName)
where
	editor` file	
		=   			get file
		>>= \content ->	withShared content 
		    \copy ->  	parallel (Title fileName)	
							[ (Embedded, showStatistics copy)
							, (Embedded, editFile fileName copy)
							, (Embedded, replace initReplace copy)
							]  @ const Void
		>>*	 			[ OnAction (Action "File/Close") always (const (return Void))
						, OnAction  ActionClose 		 always (const (return Void))
						, OnAction  ActionSave 		     always (const (save copy >>| editor` file))
						]
	where
		save copy
			=				get copy
			>>= \content -> set	content file		
											
editFile :: String (Shared String) (SharedTaskList Void) -> Task Void
editFile fileName sharedFile _
 =						updateSharedInformation ("edit " +++ fileName) [UpdateWith toV fromV] sharedFile @ const Void
where
	toV text 			= Note text
	fromV _ (Note text) = text

showStatistics sharedFile _  = noStat 
where
	noStat :: Task Void
	noStat	=			actionTask
 				>>*		[ OnAction (Action "File/Show Statistics") always (const showStat)
 						]
	showStat :: Task Void 
	showStat =			viewSharedInformation "Statistics:" [ViewWith stat] sharedFile 
 				>>*		[ OnAction (Action "File/Hide Statistics") always (const noStat)
 						]
 	where
	 	stat text = {lineCount = lengthLines text, wordCount = lengthWords text}
		where
			lengthLines ""   = 0
			lengthLines text = length (split "\n" text)
		
			lengthWords "" 	 = 0
			lengthWords text = length (split " " (replaceSubString "\n" " " text))

replace cmnd sharedFile _ = noReplace cmnd
where
	noReplace :: Replace -> Task Void
	noReplace cmnd 
		=		actionTask
 			>>*	[ OnAction (Action "File/Replace") always (const (showReplace cmnd))
				]

	showReplace :: Replace -> Task Void 
	showReplace cmnd
		=		updateInformation "Replace:" [] cmnd 
 			>>*	[ OnAction (Action "Replace") hasValue (substitute o getValue)
 				, OnAction (Action "Cancel")  always   (const (noReplace cmnd))
 				]
 			
 	substitute cmnd =	update (replaceSubString cmnd.search cmnd.replaceBy) sharedFile 
 						>>| showReplace cmnd

//* utility functions

actionTask :: Task Void
actionTask = viewInformation Void [] Void

undef = undef

always = const True

hasValue (Value _ _) = True
hasValue _ = False

getValue (Value v _) = v

ifValue pred (Value v _) = pred v
ifValue _ _ = False

ifStable (Value v Stable) = True
ifStable _ = False

// old stuff Bas
/*
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


*/










 						
 
