module CleanEditor

import iTasks, Text
import qualified Map

// Global Settings

batchBuild		:== "BatchBuild.exe"
errorFile		:== "Temp\\errors"

//cleanPath 		:== "C:\\Users\\bas\\Desktop\\Clean\\" 
cleanPath 		:== "C:\\Users\\marinu\\Desktop\\Clean_2.2\\"

projectPath		:== cleanPath +++ "iTasks-SDK\\Examples\\Development\\"

:: IDE_State =	{ projectName	:: String
				, projectPath	:: String
				, openFiles		:: [String]
				, recentFiles	:: [String]
				, recentProjects:: [String]
				, cleanPath		:: String
				}

derive class iTask IDE_State

init_IDE_State :: IDE_State			
init_IDE_State
	= 	{ projectName		= "" 
		, projectPath 		= projectPath
		, openFiles			= []
		, recentFiles 		= []
		, recentProjects	= []
		, cleanPath			= cleanPath
		}

IDE_Store = sharedStore "IDE_State" init_IDE_State

// 

Start :: *World -> *World
Start world = startEngine ide world 
				
ide :: Task Void
ide = 	parallel Void //(Title "Clean IDE") 	
						[ (Embedded, topMenu  ideState)
						, (Embedded, projectFiles  ideState)
						, (Embedded, messages ideState)
						] <<@ SetLayout layout @ const Void
where
	layout = customMergeLayout (sideMerge TopSide 0 (sideMerge LeftSide 150 (sideMerge BottomSide 100 tabbedMerge)))

	ideState = IDE_Store

// top menu

topMenu ideState ts = forever (					(get ideState 
								>>= \state -> 	(actionTask >>*	handleMenu state))
												-||- 
												(watch ideState @ const Void)		// no effect ???
												
							  ) 
where
	handleMenu state
	=	[ OnAction (Action "File/Open") always (const (openFile ideState ts))
		] 
		++
		[ OnAction (Action ("File/Recent Files/" +++ fileName)) always (launch (const (editor fileName))) 
		\\ fileName <- state.recentFiles
		] 
		++
		[ OnAction (Action ("File/Recent Projects/" +++ fileName +++ " (.prj)")) always (const (setProjectName ideState fileName)) 
		\\ fileName <- state.recentProjects
		] 
		++
		[ OnAction (Action "Project/New Project...") always (launch (newProject ideState))]
		++
		if (state.projectName <> "")
			[ OnAction (Action ("Project/Bring Up To Date " +++ state.projectName +++ " (.prj)")) always
								 (launch (compile state.projectName ideState))	]
			[ OnAction (Action ("Project/Bring Up To Date ")) never (\_ -> return Void)	]												 
		++ // temp fix to show effects
		[ OnAction (Action "Temp/Refresh") always (const (return Void)) ]	
	launch task _ = appendTask Embedded task ts @ const Void


// project pane

projectFiles ideState _
	=				viewInformation "Project" [] "dummy" @ const Void

// messages pane

messages ideState ts 
	= 				get ideState
	>>= \state ->	let sharedError = externalFile (state.cleanPath +++ errorFile) in
					viewSharedInformation (Title "Error Messages...") [ViewWith (\txt -> Note txt)] sharedError
					@ const Void 

// handling top menu commands

// open file...	  

openFile ideState ts
	=				enterInformation ("Open file","Give name of text file you want to open...") [] <<@ Window
	>>*				[ OnAction ActionCancel 		always   (const (return Void))
					, OnAction (Action "Open File") hasValue (addFileAndEdit ideState o getValue)
					] 
where
	addFileAndEdit ideState fileName
		=			update (\state -> {state & recentFiles = removeDup [fileName:state.recentFiles]}) ideState
		>>|			(launch (const (editor fileName)))	

	launch task  = appendTask Embedded task ts @ const Void

// setting project... 

newProject ideState _ 
	=				updateInformation "Set name of project..." [] "" <<@ Window
	>>*				[ OnAction ActionCancel   always   (const (return Void))
					, OnAction (Action "Set") hasValue (setProjectName ideState o getValue)
					]

setProjectName ideState projectName 
	= 			update (\state -> {state & projectName = name 
										 , recentProjects = removeDup [name:state.recentProjects]}) ideState
				@ const Void 
where
	name = dropExtension projectName

// compile project... 

compile projectName ideState _
	=				get ideState
					
	>>= \state ->	(let compilerMessages = state.projectPath +++ projectName +++ ".log"  in
						exportTextFile compilerMessages ""	//Empty the log file...
						>>|
						callProcess "Clean Compiler" [] (cleanPath +++ batchBuild) [projectPath +++ projectName +++ ".prj"] 
						-&&-
						//View the messages while the compiler is building...
						viewSharedInformation (Title "Compiler Messages...") [ViewWith (\txt -> Note txt)] (externalFile compilerMessages)
					)
					
	>>*				[ OnAction ActionClose always (\_ -> return Void)]
					

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
		    \copy ->  	(parallel (Title fileName)	
							[ (Embedded, showStatistics copy)
							, (Embedded, editFile fileName copy)
							, (Embedded, replace initReplace copy)
							]  @ const Void ) // <<@ AfterLayout (uiDefSetDirection Horizontal)
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
 =						(updateSharedInformation Void [UpdateWith toV fromV] sharedFile  @ const Void) <<@ noHints
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
never = const False
hasValue (Value _ _) = True
hasValue _ = False

getValue (Value v _) = v

ifValue pred (Value v _) = pred v
ifValue _ _ = False

ifStable (Value v Stable) = True
ifStable _ = False

noHints = AfterLayout (tweakControls (\controls -> [(c,'Map'.del VALID_ATTRIBUTE ('Map'.del HINT_ATTRIBUTE m)) \\ (c,m) <- controls]))


