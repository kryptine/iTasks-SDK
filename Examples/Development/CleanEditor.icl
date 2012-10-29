module CleanEditor

/* A Clean IDE for a browser using the iTask system
Status: very drafty
- Assumes BatchBuild.exe and a copy of the IDEEnvs file in the original Clean IDE Application directory
- You have to set cleanPath by hand
*/

//cleanPath 		:== "C:\\Users\\bas\\Desktop\\Clean\\" 
//cleanPath 		:== "C:\\Users\\marinu\\Desktop\\Clean_2.2\\"
cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"
idePath			:== "iTasks-SDK\\Examples\\Development\\"
projectPath		:== cleanPath +++ idePath
environment		:== map ((+++) cleanPath) [idePath,"Libraries\\StdEnv\\","iTasks-SDK\\Server\\"]

batchBuild		:== "BatchBuild.exe"
errorFile		:== "Temp\\errors"

import iTasks, Text
import qualified Map
import projectManager

// Global Settings

:: IDE_State =	{ project			:: Maybe (String,Project)
				, projectPath		:: String
				, cleanPath			:: String
				, openedFiles		:: [String]
				, recentFiles		:: [String]
				, recentProjects	:: [String]
				}
derive class iTask IDE_State

IDE_Store = sharedStore "IDE_State" init_IDE_State
where
	init_IDE_State :: IDE_State			
	init_IDE_State
		= 	{ project			= Nothing 
			, projectPath 		= projectPath
			, cleanPath			= cleanPath
			, openedFiles		= []
			, recentFiles 		= []
			, recentProjects	= []
			}
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

topMenu :: (Shared IDE_State) (ReadOnlyShared (TaskList Void)) -> Task Void
topMenu ideState ts 
	= 				get ideState												// new session, first recover previous screen
	>>= \state ->	openLastProject state.project 
	>>|				openOpenedFiles state.openedFiles 
	>>|				forever (				(get ideState 
							>>= \state -> 	(actionTask >>*	handleMenu state))
											-||- 
											(watch ideState @ const Void)		// no effect ???
							) 
where
	openLastProject Nothing 			  = return Void
	openLastProject (Just (name,project)) = openProject ideState name

	openOpenedFiles [] 		=	return Void
	openOpenedFiles [f:fs]	=	launch (editor ideState f ts) ts >>| openOpenedFiles fs 

	handleMenu state
	=	[ OnAction (Action "File/Open") always (const (openFile ideState ts))
		, OnAction (Action "File/Save All") (const (state.openedFiles <> [])) (const (saveAll state.openedFiles ideState))
		] 
		++
		[ OnAction (Action ("File/Recent Files/" +++ fileName)) always (const (openFileAndEdit ideState fileName ts)) 
		\\ fileName <- state.recentFiles
		] 
		++
		[ OnAction (Action ("File/Recent Projects/" +++ fileName +++ " (.prj)")) always (const (openProject ideState fileName)) 
		\\ fileName <- state.recentProjects
		] 
		++
		[ OnAction (Action "Search/Search Identifier...") always (const (launch (search ideState ts) ts))
		]
		++
		[ OnAction (Action "Project/New Project...") always (const (launch (newProject ideState) ts))
		, OnAction (Action ("Project/Bring Up To Date " +++ projectName +++ " (.prj)")) (const (projectName <> ""))
								 (const (launch (compile projectName ideState <<@ Window) ts))	
		, OnAction (Action "Project/Project Options...") (const (projectName <> "")) (const (changeOptions ideState))
		]

		++ // temp fix to show effects
		[ OnAction (Action "Temp/Refresh") always (const (return Void)) ]	
	where
		projectName = if (isNothing state.project) "" (fst (fromJust state.project)) 

// project pane

projectFiles :: (Shared IDE_State) (ReadOnlyShared (TaskList Void)) -> Task Void
projectFiles ideState _
	=				viewInformation "Project" [] "dummy" @ const Void

// messages pane

messages :: (Shared IDE_State) (ReadOnlyShared (TaskList Void)) -> Task Void
messages ideState ts 
	= 				get ideState
	>>= \state ->	let sharedError = externalFile (state.cleanPath +++ errorFile) in
					viewSharedInformation (Title "Error Messages...") [ViewWith (\txt -> Note txt)] sharedError
					@ const Void 

// handling top menu commands

// open file...	  

openFile :: (Shared IDE_State) (ReadOnlyShared (TaskList Void)) -> Task Void
openFile ideState ts
	=				enterInformation ("Open file","Give name of text file you want to open...") [] <<@ Window
	>>*				[ OnAction ActionCancel 		always   (const (return Void))
					, OnAction (Action "Open File") hasValue (\v -> openFileAndEdit ideState (getValue v) ts)
					] 

openFileAndEdit :: (Shared IDE_State) FileName (ReadOnlyShared (TaskList Void)) -> Task Void
openFileAndEdit ideState fileName ts
	=				get ideState
	>>= \state ->	if (isMember fileName state.openedFiles)
						(return Void)	// file already open
						(			update (\state -> {state & recentFiles 	= removeDup [fileName:state.recentFiles]
										 				     , openedFiles 	= [fileName:state.openedFiles]}) ideState
						>>|			launch (editor ideState fileName ts) ts	
						)

closeEditFile :: (Shared IDE_State) FileName -> Task Void
closeEditFile ideState fileName
	=			update (\state -> {state & openedFiles 	= removeMember fileName state.openedFiles}) ideState
				@ const Void

saveAll :: [FileName] (Shared IDE_State) -> Task Void
saveAll [] ideState 
	= return Void
saveAll [name:names] ideState
	=					get (sharedStore name "")				// read out latest content of the editor from the internal store
		>>= \content ->	set content (externalFile name)			// and store it in the file			
		>>|				saveAll names ideState						

// search department

:: SearchOptions 	:== (SearchWhat, SearchWhere)
:: SearchWhat 		= 	SearchIdentifier | SearchDefinition | SearchImplementation
:: SearchWhere		=	SearchLocal | SearchImports | SearchPaths | SearchProject

derive class iTask SearchWhat, SearchWhere

search ideState ts  
	= 				get ideState
	>>= \state ->	((updateInformation (Title "Search identifier in module") [] ("","") <<@ Window
//					-&&-
//					updateInformation Void [] (SearchIdentifier,SearchLocal)
					)
	>>*				[ OnAction ActionCancel   always (const (return Void))
					, OnAction ActionContinue hasValue (searchIdent state o getValue)
					])
where
	searchIdent state (identifier,file)
		= 	
						searchIdentifierInImports identifier (projectPath, file) environment
		>>=	\found	->	viewInformation ("Found:",identifier) [] found <<@ Window
//		>>=	\found	->	viewInformation ("Found:",identifier) [] (found,length (snd found)) <<@ Window
//		>>=	\found	->	viewInformation ("Found:",identifier) [ViewWith (\(l,v) -> foldl (+++) "" l)] found <<@ Window
		>>|				search ideState ts

// setting project... 

newProject :: (Shared IDE_State) -> Task Void
newProject ideState 
	=				updateInformation "Set name of project..." [] "" <<@ Window
	>>*				[ OnAction ActionCancel   always   (const (return Void))
					, OnAction (Action "Set") hasValue (\v -> let name = getValue v in 
															  (storeProject ideState (initProject name) name))
					]
storeProject :: (Shared IDE_State) Project ModuleName -> Task Void
storeProject ideState project projectName 
	= 				get ideState
	>>= \state ->	saveProjectFile (state.projectPath +++ projectName +++ ".prj") state.cleanPath project
	>>= \ok ->		if (not ok)
						(viewInformation "Write Error..." [] "Could not store project file !" 
						@ const Void
						)
						(update (\state -> { state	& project 			= Just (name,project) 
										   			, recentProjects 	= removeDup [name:state.recentProjects]
										   			}) ideState
						@ const Void
						)
where
	name = dropExtension projectName
	
openProject :: (Shared IDE_State) ModuleName -> Task Void
openProject ideState projectName 
	= 								get ideState
	>>= \state ->					readProjectFile (state.projectPath +++ projectName +++ ".prj") state.cleanPath 
	>>= \(project,ok,message) ->	if (not ok)
										(viewInformation "Read Error..." [] message <<@ Window
										@ const Void
										)
										(update (\state -> { state	& project 			= Just (name,project)
																	, recentProjects 	= removeDup [name:state.recentProjects]
															}
										) ideState
										@ const Void
										)
where
	name = dropExtension projectName

changeOptions :: (Shared IDE_State) -> Task Void
changeOptions ideState
	= changeOptions` <<@ Window
where
	changeOptions`
		=				get ideState
		>>= \state ->	let (name,project) = fromJust state.project in
							changeOptions`` name project (fromProject project)
		
	changeOptions``	name project (rto,diagn,prof)	
		=				runTimeOptions rto
		>>= \rto ->		diagnosticsOptions diagn
		>>= \diagn -> 	profilingOptions prof
		>>= \prof ->	storeProject ideState (toProject project (rto,diagn,prof)) name 

	runTimeOptions :: RunTimeOptions -> Task RunTimeOptions
	runTimeOptions	rto = updateInformation ("Project Options","Run-Time Options:")[] rto 
	
	diagnosticsOptions :: DiagnosticsOptions -> Task DiagnosticsOptions
	diagnosticsOptions	diagn = updateInformation ("Project Options","Diagnostics Options:") [] diagn
	
	profilingOptions :: ProfilingOptions -> Task ProfilingOptions
	profilingOptions prof = updateInformation ("Project Options","Diagnostics Options:") [] prof	

// compile project... 

compile :: ModuleName (Shared IDE_State) -> Task Void
compile projectName ideState 
	=				get ideState
	>>= \state ->	if (state.openedFiles == []) 
					(compile` state)
					(	viewInformation ("Clean Compiler","Do you want to save all ?") [] Void 
					>>* [ OnAction ActionYes 	always (const (saveAll state.openedFiles ideState >>| compile` state))
						, OnAction ActionNo  	always (const (compile` state))
						, OnAction ActionCancel always (const (return Void))
						]
					)
where	
	compile` state
		=
					(let compilerMessages = state.projectPath +++ projectName +++ ".log"  in
						exportTextFile compilerMessages ""	//Empty the log file...
						>>|
						callProcess "Clean Compiler" [] (cleanPath +++ batchBuild) [projectPath +++ projectName +++ ".prj"] 
						-&&-
						//View the messages while the compiler is building...
						viewSharedInformation (Title "Compiler Messages...") [] (externalFile compilerMessages) <<@ Window
					)
		>>*			[ OnAction ActionClose always (\_ -> return Void)
					, OnAction ActionOk    always (\_ -> return Void)
					]

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
editor :: (Shared IDE_State) FileName (ReadOnlyShared (TaskList Void)) -> Task Void
editor ideState fileName ts = editor` (externalFile fileName) // <<@ Window
where
	editor` file	
		=   			get file
		>>= \content ->	let copy = sharedStore fileName content in editor`` copy
	where
		editor`` copy 
			=  				(parallel (Title fileName)	
								[ (Embedded, showStatistics copy)
								, (Embedded, editFile fileName copy)
								, (Embedded, replace initReplace copy)
								]  @ const Void ) // <<@ AfterLayout (uiDefSetDirection Horizontal)
			>>*	 			[ OnAction  ActionClose 		 				  always (const (closeEditFile ideState fileName))
							, OnAction (Action ("File/Close " +++ fileName))  always (const (closeEditFile ideState fileName))
							, OnAction (Action ("File/Save " +++ fileName))   always (const (save copy >>| editor` file))
							, OnAction (Action ("File/Revert " +++ fileName)) always (const (editor` file))
							, OnAction (Action ("File/Open " +++ other fileName)) 
																			  (const isIclOrDcl) 
																			  (const (openFileAndEdit ideState fileName ts))
	
							, OnAction (Action ("Project/Set Project/" +++ noSuffix +++ " (.prj)")) 
																			  (const isIcl)
																			  (const (storeProject ideState (initProject noSuffix) noSuffix
																			  				 >>| editor` file )) 
							]

		noSuffix = RemoveSuffix fileName

		save copy
			=				get copy
			>>= \content -> set	content file		

		other fileName
		| equal_suffix ".icl" fileName = (RemoveSuffix fileName) +++ ".dcl"
		| equal_suffix ".dcl" fileName = (RemoveSuffix fileName) +++ ".icl"
		= ""

		isIclOrDcl	= isIcl || equal_suffix ".dcl" fileName 
		isIcl  		= equal_suffix ".icl" fileName
											
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
 				>>*		[ OnAction (Action "File/Statistics/Show") always (const showStat)
 						]
	showStat :: Task Void 
	showStat =			viewSharedInformation "Statistics:" [ViewWith stat] sharedFile 
 				>>*		[ OnAction (Action "File/Statistics/Hide") always (const noStat)
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
		=		updateInformation "Replace:" [] cmnd // <<@ Window
 			>>*	[ OnAction (Action "Replace") hasValue (substitute o getValue)
 				, OnAction (Action "Cancel")  always   (const (noReplace cmnd))
 				]
 			
 	substitute cmnd =	update (replaceSubString cmnd.search cmnd.replaceBy) sharedFile 
 						>>| showReplace cmnd

//* utility functions

actionTask :: Task Void
actionTask = viewInformation Void [] Void

launch task ts = appendTask Embedded (const task) ts @ const Void

showError msg continue
	=	viewInformation ("Error","") [] msg <<@ Window
		>>| continue

// tiny util

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

