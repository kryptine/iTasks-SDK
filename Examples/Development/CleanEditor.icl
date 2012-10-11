module CleanEditor

import iTasks, Text

batchBuild		:== "BatchBuild.exe"
errorFile		:== "Temp\\errors"

cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"
projectPath		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\iTasks-SDK\\Examples\\Development\\"
projectName		:==	"test"

:: IDE_State =	{ projectName	:: String
				, projectPath	:: String
				, openFiles		:: [String]
				, cleanPath		:: String
				}

derive class iTask IDE_State

init_IDE_State :: IDE_State			
init_IDE_State
	= 	{ projectName	= "" 
		, projectPath 	= projectPath
		, openFiles		= []
		, cleanPath		= cleanPath
		}

Start :: *World -> *World
Start world = startEngine ide world 
				
ide :: Task Void
ide = 	let ideState = sharedStore "IDE_State" init_IDE_State in
		parallel Void //(Title "Clean IDE") 	
						[ (Embedded, topmenu  ideState)
						, (Embedded, project  ideState)
						, (Embedded, messages ideState)
						] <<@ SetLayout layout @ const Void
where
	layout = customMergeLayout (sideMerge TopSide 0 (sideMerge LeftSide 150 (sideMerge BottomSide 100 tabbedMerge)))

topmenu ideState ts = forever handleMenu 
where
	handleMenu
		=				get ideState
		>>= \state ->	
						(actionTask 
	 	>>*				[ OnAction (Action "File/Open") always (launch openFile)
	 					, OnAction (Action "Project/Set Project") always (launch (setProject ideState))
						])
						-||-
						(watch ideState 
	 	>>*				[ OnAction (Action ("Project/Bring Up To Date " +++ state.projectName +++ ".prj")) 
	 							(ifValue (\v -> v.projectName <> "")) (\v -> launch (compile (getValue v).projectName ideState) NoValue)
						])

	launch task _ = appendTask Embedded task ts @ const Void

project ideState _
	=				viewInformation "Project" [] "dummy" @ const Void

	  
setProject ideState _ 
	=				updateInformation "Give name of project file..." [] ""
	>>*				[ OnAction ActionCancel 		always   (const (return Void))
					, OnAction (Action "Set") hasValue (\s -> update (\state -> {state & projectName = getValue s}) ideState @ const Void)
					]
openFile _
	=				updateInformation "Give name of text file you want to open..." [] ""
	>>*				[ OnAction ActionCancel 		always   (const (return Void))
					, OnAction (Action "Open File") hasValue (editor o getValue)
					]

compile projectName ideState _
	=				callProcess "Clean Compiler" [] (cleanPath +++ batchBuild) [projectPath +++ projectName +++ ".prj"] 
	>>|				get ideState
	>>= \state ->	let compilerMessages = externalFile (state.projectPath +++ projectName +++ ".log") in
					viewSharedInformation (Title "Compiler Messages...") [ViewWith (\txt -> Note txt)] compilerMessages
					@ const Void 

messages ideState ts 
	= 				get ideState
	>>= \state ->	let sharedError = externalFile (state.cleanPath +++ errorFile) in
					viewSharedInformation (Title "Error Messages...") [ViewWith (\txt -> Note txt)] sharedError
					@ const Void 
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


