module CleanEditor

/* A Clean IDE for a browser using the iTask system
Status: very drafty
- Assumes BatchBuild.exe and a copy of the IDEEnvs file in the original Clean IDE Application directory
- You have to set cleanPath by hand
*/

//cleanPath 		:== "C:\\Users\\bas\\Desktop\\Clean\\" 
cleanPath 		:== "C:\\Users\\marinu\\Desktop\\Clean_2.2\\"
//cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"

idePath			:== "iTasks-SDK\\Examples\\Development\\"
projectPath		:== cleanPath +++ idePath
compilerPath	:== cleanPath +++ "iTasks-SDK\\Compiler\\"

environment		:== map ((+++) cleanPath) [idePath,"Libraries\\StdEnv\\"]

batchBuild		:== "BatchBuild.exe"
errorFile		:== "Temp\\errors"
environmentFile :== "iTask-Clean-IDE-environment"
stdenv			:== cleanPath +++ "Libraries\\StdEnv\\"

import iTasks, Text
import qualified Map
import projectManager

derive class iTask IDE_State
derive class iTask FileError

// Global Settings

:: IDE_State =	{ projectName		:: !String			// name of the project, empty string indicates no project set
				, projectPath		:: !String			// path where project is located
				, project			:: !Project			// settings will passed to call of BatchBuild, the batch version of the compiler 
				, cleanPath			:: !String			// path whare Clean compiler and BatchBuild is located
				, openedFiles		:: ![String]		// files currently opened in IDE
				, recentFiles		:: ![String]		// recently opened files
				, recentProjects	:: ![String]		// recently opened projects
				, idx				:: !Int				// index in target
				, envTargets		:: ![Target]		// targets are environments
				}

IDE_Store = sharedStore "IDE_State" init_IDE_State
where
	init_IDE_State :: IDE_State			
	init_IDE_State
		= 	{ projectName		= ""
			, projectPath 		= projectPath
			, project			= initProject "" 
			, cleanPath			= cleanPath
			, openedFiles		= []
			, recentFiles 		= []
			, recentProjects	= []
			, idx				= 0
			, envTargets		= [initTarget]
			}
	initIDEenv
		=	{ environmentName		= "IDE environment"	
			, paths					= environment
			, toolsOption			= initTools
			}
	initStdEnv
		=	{ environmentName		= "StdEnv"	
			, paths					= [stdenv]
			, toolsOption			= initTools
			}
	initTools 	
		= 	{ compiler				= compilerPath +++ "CleanCompiler.exe : -h 64M : -dynamics -generics"
			, codeGenerator			= compilerPath +++ "CodeGenerator.exe"
			, staticLinker			= compilerPath +++ "StaticLinker.exe : -h 64M"
			, dynamicLinker			= compilerPath +++ "DynamicLinker.exe"
			, versionOfAbcCode		= 920
			, runOn64BitProcessor	= False
			}
// 

Start :: *World -> *World
Start world = startEngine ide world 
//Start world = startEngine test world 
				
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
	>>= \state ->	openLastProject state.projectName  
	>>|				openOpenedFiles state.openedFiles 
	>>|				forever (				(get ideState 
							>>= \state -> 	(actionTask >>*	handleMenu state))
											-||- 
											(watch ideState @ const Void)		// no effect ???
							) 
where
	openLastProject "" 	= return Void
	openLastProject name	= openProject ideState name

	openOpenedFiles [] 		=	return Void
	openOpenedFiles [f:fs]	=	launch (editor ideState f ts) ts >>| openOpenedFiles fs 

	handleMenu state
	=	[ OnAction (Action "File/Open...") always (const (openFile2 ideState ts))
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
		[ OnAction (Action "Search/Search Identifier...")     ifProject (const (launch (search SearchIdentifier     SearchInImports ideState ts) ts))
		, OnAction (Action "Search/Search Definition...")     ifProject (const (launch (search SearchDefinition     SearchInImports ideState ts) ts))
		, OnAction (Action "Search/Search Implementation...") ifProject (const (launch (search SearchImplementation SearchInImports ideState ts) ts))
		]
		++
		[ OnAction (Action "Project/New Project...") always (const (launch (newProject ideState) ts))
		, OnAction (Action ("Project/Bring Up To Date " +++ projectName +++ " (.prj)")) (const (projectName <> ""))
								 (const (launch (compile projectName ideState <<@ Window) ts))	
		, OnAction (Action "Project/Project Options...") ifProject (const (changeOptions ideState))
		]
		++
		[ OnAction (Action (selectedEnvironment +++ "/Edit " +++ currentEnvName)) always (const (editEnvironment ideState))
		, OnAction (Action (selectedEnvironment +++ "/Import...")) always (const (importEnvironment ideState))
		:[ OnAction (Action (selectedEnvironment +++ "/Select/" +++ target.target_name)) always (const (selectEnvironment ideState i)) 
		 \\ target <- state.envTargets & i <- [0..]
		 ]
		]
		++ // temp fix to show effects
		[ OnAction (Action "Temp/Refresh") always (const (return Void)) ]	
	where
		projectName = state.projectName 

		ifProject = const (projectName <> "")

		currentEnvName			= currEnvName state
		selectedEnvironment 	= "_Environment " +++ currentEnvName

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

openFile2 :: (Shared IDE_State) (ReadOnlyShared (TaskList Void)) -> Task Void
openFile2 ideState ts
	=				get ideState
	>>= \state ->	selectFileInPath state.projectPath (\_ -> True) <<@ Window
	>>= \(path,r)-> if (isNothing r)
						(return Void)
						(openFileAndEdit ideState (fromJust r) ts)

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

derive class iTask SearchWhat, SearchWhere

search what whereSearch ideState ts  
	= 				get ideState
	>>= \state ->	searching state "" what whereSearch ([],[])  <<@ Window
	
where
	searching state identifier what whereSearch found
		=			(viewInformation (length (snd found) +++> " modules searched, " +++> length (fst found) +++> " contained " +++> identifier) [] found
					||-
					updateInformation (Title ("Find: " +++ identifier)) [] (identifier,what,whereSearch)) 
		>>*			[ OnAction ActionCancel   always (const (return Void))
					, OnAction ActionContinue hasValue (performSearch o getValue)
					] 
	where
		performSearch (identifier,what,whereSearch)
			= 				searchTask what whereSearch identifier (projectPath, state.projectName +++ ".icl") environment
			>>=	\found	->	searching state identifier what whereSearch found

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
	>>= \state ->	saveProjectFile project (state.projectPath +++ projectName +++ ".prj") state.cleanPath 
	>>= \ok ->		if (not ok)
						(viewInformation "Write Error..." [] "Could not store project file !" 
						@ const Void
						)
						(update (\state -> { state	& projectName		= name
													, project			= project
													, recentProjects 	= removeDup [name:state.recentProjects]
											}
						) ideState
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
										(update (\state -> { state	& projectName		= name
																	, project			= project
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
		>>= \state ->	changeOptions`` state.projectName state.project (fromProject state.project)
		
	changeOptions``	name project (rto,diagn,prof,co)	
		=				runTimeOptions rto
		>>= \rto ->		diagnosticsOptions diagn
		>>= \diagn -> 	profilingOptions prof
		>>= \prof ->	consoleOptions co
		>>= \co ->		storeProject ideState (toProject project (rto,diagn,prof,co)) name 

	runTimeOptions :: RunTimeOptions -> Task RunTimeOptions
	runTimeOptions	rto = updateInformation ("Project Options","Run-Time Options:")[] rto 
	
	diagnosticsOptions :: DiagnosticsOptions -> Task DiagnosticsOptions
	diagnosticsOptions	diagn = updateInformation ("Project Options","Diagnostics Options:") [] diagn
	
	profilingOptions :: ProfilingOptions -> Task ProfilingOptions
	profilingOptions prof = updateInformation ("Project Options","Diagnostics Options:") [] prof	

	consoleOptions :: ConsoleOptions -> Task ConsoleOptions
	consoleOptions co = updateInformation ("Project Options","Console Options:") [] co	
	
// editing the evironment

currEnvName state 				= 	(getCurrEnv state).target_name
getCurrEnv  state   			= 	state.envTargets!!state.idx
updateEnv idx target state		= 	{ state & idx				= idx
											, envTargets 		= updateAt idx target state.envTargets
											, project.prjpaths 	= (state.envTargets!!idx).target_path 
											, project.target	= (state.envTargets!!idx).target_name
									}
selectEnv idx state				= 	{ state & idx				= idx
											, project.prjpaths 	= (state.envTargets!!idx).target_path 
											, project.target	= (state.envTargets!!idx).target_name
									}

editEnvironment :: (Shared IDE_State) -> Task Void
editEnvironment ideState
	= editEnvironment` <<@ Window
where
	editEnvironment`
		=				get ideState
		>>= \state ->	updateInformation (Title ("Edit " +++ currEnvName state))[] (fromTarget (getCurrEnv state))
		>>*				[ OnAction ActionCancel   always  (const (return Void))
						, OnAction (Action "Set") hasValue (updateCurrEnv o getValue) 
						]
	updateCurrEnv env = update (\state -> updateEnv state.idx (toTarget env) state) ideState @ const Void
	
importEnvironment :: (Shared IDE_State) -> Task Void					
importEnvironment ideState						
	=					get ideState
	>>= \state ->		selectFileInPath state.projectPath (\name -> takeExtension name == "env" || takeExtension name == "") <<@ Window
	>>= \(path,mbEnv)-> if (isNothing mbEnv)
						(showError "Could not read environment file" path)
						(					 readEnvironmentFile (path </> (fromJust mbEnv)) 
							>>=  \ntargets -> update (\state -> {state & envTargets = state.envTargets ++ ntargets}) ideState
											 @ const Void)

selectEnvironment :: (Shared IDE_State) Int -> Task Void					
selectEnvironment ideState i
	=					get ideState
	>>= \state ->		update (selectEnv i) ideState
	>>|					storeProject ideState state.project state.projectName
	 					@ const Void

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
						callProcess "Clean Compiler - BatchBuild" [] (cleanPath +++ batchBuild) [projectPath +++ projectName +++ ".prj"] 
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
								[ (Embedded, editFile fileName copy)
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
//
import File
import Directory
derive class iTask MaybeError, FileInfo, Tm

// iTask utilities of general nature

// file selector...

selectFileInPath :: !PathName !(!FileName -> Bool) -> Task !(PathName,Maybe !FileName)
selectFileInPath path pred
	= 				accWorld (readDirectory path)
	>>= \content -> case content of
						Ok names 	-> select names
						_ 			-> return (path,Nothing)
where
	select names
		=				enterChoice ("File Selector",path) [] (filter pred names)
		>>*				[ OnAction ActionCancel always   (const (return (path,Nothing)))
						, OnAction ActionOk		hasValue (continue o getValue)
						, OnAction ActionNew	always 	 (const (newFile))
						]
	newFile
		=				enterInformation ("Create File",path) []
		>>*				[ OnAction ActionCancel always   (const (return (path,Nothing)))
						, OnAction ActionOk		hasValue (write o getValue)
						]
	write name
		=				accWorld (writeFile (path </> name) "")
		>>|				return (path,Just name)


	continue ".." = selectFileInPath (takeDirectory path) pred
	continue "."  = selectFileInPath path pred
	continue name
	| takeExtension name == "icl" 
		= (return (path,Just name))
	| takeExtension name == "dcl"
		= (return (path,Just name))
	| otherwise		
		=				accWorld (getFileInfo (path </> name))
		>>= \content ->	case content of
							Ok info -> if info.directory
								(selectFileInPath (path </> name) pred)
								(return (path,Just name))
							_		-> selectFileInPath path pred

showError :: String a -> Task Void | iTask a
showError prompt val = (viewInformation ("Error",prompt) [] val >>= \_ -> return Void) //<<@ Window

//* simple utility functions

actionTask :: Task Void
actionTask = viewInformation Void [] Void

launch task ts = appendTask Embedded (const task) ts @ const Void


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

