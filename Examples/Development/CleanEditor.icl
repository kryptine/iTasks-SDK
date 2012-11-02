module CleanEditor

/* A Clean IDE for a browser using the iTask system
Status: very drafty
*/

// Attention:
// Currently it is assumed the you have a copy of BatchBuild.exe and a copy of the IDEEnvs file (from Config) in the original Clean IDE Application directory
// You have to set the cleanPath by hand for the time being...

//cleanPath 		:== "C:\\Users\\bas\\Desktop\\Clean\\" 
//cleanPath 		:== "C:\\Users\\marinu\\Desktop\\Clean_2.2\\"
cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"

batchBuild		:== "BatchBuild.exe"
errorFile		:== "Temp\\errors"

initialPath 	:== cleanPath +++ "iTasks-SDK\\Examples\\Development\\"

import iTasks, Text
import qualified Map
import projectManager

derive class iTask IDE_State
derive class iTask FileError

// Global IDE State

:: IDE_State =	{ projectName		:: !String			// name of the project, empty string indicates no project set
				, projectPath		:: !String			// path where project is located
				, projectSettings	:: !Project			// settings will passed to call of BatchBuild, the batch version of the compiler 
				, cleanPath			:: !String			// path whare Clean compiler and BatchBuild is located
				, openedFiles		:: ![String]		// files currently opened in IDE
				, recentFiles		:: ![String]		// recently opened files
				, recentProjects	:: ![String]		// recently opened projects
				, idx				:: !Int				// index in target
				, envTargets		:: ![Target]		// targets are environments
				}

// IDE state is a shared global state, passed around explicitly to make clear where it is used

Start :: *World -> *World

/* does not seem to work ????
Start w = startEngine test w

test = currentDirectory													// determine directory of this CleanEditor
	>>= \dir ->		viewInformation "Direct = " [] dir

*/

Start world = startEngine (start_ide ideState) world 
where
	ideState :: Shared IDE_State
	ideState = sharedStore "IDE_State" init_IDE_State

	init_IDE_State :: IDE_State			
	init_IDE_State
		= 	{ projectName		= ""
			, projectPath 		= initialPath
			, projectSettings	= PR_InitProject
			, cleanPath			= cleanPath
			, openedFiles		= []
			, recentFiles 		= []
			, recentProjects	= []
			, idx				= 0
			, envTargets		= [t_StdEnv]
			}
/*
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
*/				
start_ide :: (Shared IDE_State) -> Task Void
start_ide ideState
	= 		/*		currentDirectory													// determine directory of this CleanEditor
	>>= \dir ->	*/	readEnvironmentFile (cleanPath +++ EnvsFileName) 					// read environment file used for communication with BatchBuild	
	>>= \env ->		update (\state -> 	{ state & projectPath 		= initialPath // dir				// store project path
												, envTargets		= env				// store environments
										}) ideState	
	>>|				parallel Void // (Title "Clean IDE") 	
						[ (Embedded, topMenu       ideState)
						, (Embedded, projectFiles  ideState)
						, (Embedded, messages      ideState)
						] <<@ SetLayout layout @ const Void
where
	layout = customMergeLayout (sideMerge TopSide 0 (sideMerge LeftSide 150 (sideMerge BottomSide 100 tabbedMerge)))

// top menu

topMenu :: (Shared IDE_State) (ReadOnlyShared (TaskList Void)) -> Task Void
topMenu ideState ts 
	= 				get ideState												// new session, first recover previous screen
	>>= \state -> 	openLastProject state.projectName  							// re-open last project
	>>|				openOpenedFiles state.openedFiles 							// re-open opened files
	>>|				forever (				(get ideState 
							>>= \state -> 	(actionTask >>*	handleMenu state))	// construct main menu
											-||- 
											(watch ideState @ const Void)		// no effect ???
							) 
where
	openLastProject "" 		= return Void
	openLastProject name	= openProject ideState name

	openOpenedFiles [] 		=	return Void
	openOpenedFiles [f:fs]	=	launch (editor ideState f ts) ts >>| openOpenedFiles fs 

	handleMenu state=:{projectName, openedFiles, recentFiles, recentProjects, envTargets}
	=	[ OnAction (Action "File/Open...") always (const (openFile ideState ts))
		, OnAction (Action "File/Save All") (const (openedFiles <> [])) (const (saveAll openedFiles ideState))
		] 
		++
		[ OnAction (Action ("File/Recent Files/" +++ fileName)) always (const (openFileAndEdit ideState fileName ts)) 
		\\ fileName <- recentFiles
		] 
		++
		[ OnAction (Action ("File/Recent Projects/" +++ fileName +++ " (.prj)")) always (const (openProject ideState fileName)) 
		\\ fileName <- recentProjects
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
		, OnAction (Action "Project/Project Options...") ifProject (const (changeProjectOptions ideState))
		]
		++
		[ OnAction (Action (selectedEnvironment +++ "/Edit " +++ currentEnvName)) always (const (editEnvironment ideState))
		, OnAction (Action (selectedEnvironment +++ "/Import...")) always (const (importEnvironment ideState))
		:[ OnAction (Action (selectedEnvironment +++ "/Select/" +++ target.target_name)) always (const (selectEnvironment ideState i)) 
		 \\ target <- envTargets & i <- [0..]
		 ]
		]
		++ // temp fix to show effects
		[ OnAction (Action "Temp/Refresh") always (const (return Void)) ]	
	where

		ifProject = const (projectName <> "")

		currentEnvName			= currEnvName state
		selectedEnvironment 	= "_" +++ currentEnvName

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

// open and save file editors ...	  

openFile :: (Shared IDE_State) (ReadOnlyShared (TaskList Void)) -> Task Void
openFile ideState ts
	=				get ideState
	>>= \state ->	selectFileInPath state.projectPath (\_ -> True) <<@ Window
	>>= \(path,r)-> if (isNothing r)
						(return Void)
						(openFileAndEdit ideState (fromJust r) ts)

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
	searching state=:{projectName,projectPath} identifier what whereSearch found
		=			(viewInformation (length (snd found) +++> " modules searched, " +++> length (fst found) +++> " contained " +++> identifier) [] found
					||-
					updateInformation (Title ("Find: " +++ identifier)) [] (identifier,what,whereSearch)) 
		>>*			[ OnAction ActionCancel   always   (const (return Void))
					, OnAction ActionContinue hasValue (performSearch o getValue)
					] 
	where
		performSearch (identifier,what,whereSearch)
			= 				searchTask what whereSearch identifier (projectPath, projectName +++ ".icl") (StrictListToList (state.envTargets!!state.idx).target_path)
			>>=	\found	->	searching state identifier what whereSearch found

// opening and storing projects... 

newProject :: (Shared IDE_State) -> Task Void
newProject ideState 
	=				updateInformation "Set name of project..." [] "" <<@ Window
	>>*				[ OnAction ActionCancel   always   (const (return Void))
					, OnAction (Action "Set") hasValue (storeNewProject o getValue)
					]
where	
	storeNewProject name 
		=				update (\state -> {state &	projectSettings			= initProject name
										  }) ideState  // BUG, could not add this field with next one
		>>|				update (\state -> {state &	projectSettings.prjpaths = (state.envTargets!!state.idx).target_path 
											     , 	projectSettings.target	= (state.envTargets!!state.idx).target_name
										  }) ideState
		>>|				get ideState
		>>= \state ->	update (selectEnv state.idx) ideState
		>>|				get ideState
		>>= \state ->	storeProject ideState state.projectSettings name

storeProject :: (Shared IDE_State) Project ModuleName -> Task Void
storeProject ideState project projectName 
	= 				get ideState
	>>= \state ->	saveProjectFile project (state.projectPath +++ projectName +++ ".prj") state.cleanPath 
	>>= \ok ->		if (not ok)
						(showError "Could not store project file !" Void)
						(update (\state -> { state	& projectName		= name
													, projectSettings	= project
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
										(showError "Read Error..."  message @ const Void)
										(update (\state -> { state	& projectName		= name
																	, projectSettings	= project
																	, recentProjects 	= removeDup [name:state.recentProjects]
															}
										) ideState
										@ const Void
										)
where
	name = dropExtension projectName

changeProjectOptions :: (Shared IDE_State) -> Task Void
changeProjectOptions ideState
	= changeProjectOptions` <<@ Window
where
	changeProjectOptions`
		=				get ideState
		>>= \state ->	changeProjectOptions`` state.projectName state.projectSettings (fromProject state.projectSettings)
		
	changeProjectOptions``	projectName project (rto,diagn,prof,co)	
		=				runTimeOptions rto
		>>= \rto ->		diagnosticsOptions diagn
		>>= \diagn -> 	profilingOptions prof
		>>= \prof ->	consoleOptions co
		>>= \co ->		storeProject ideState (toProject project (rto,diagn,prof,co)) projectName 
	where
		title :: String				// BUG ??? cannot leave out type ????
		title = "Set Options of Project: " +++ projectName	

		runTimeOptions :: RunTimeOptions -> Task RunTimeOptions
		runTimeOptions	rto = updateInformation (title,"Run-Time Options:")[] rto 
		
		diagnosticsOptions :: DiagnosticsOptions -> Task DiagnosticsOptions
		diagnosticsOptions	diagn = updateInformation (title,"Diagnostics Options:") [] diagn
		
		profilingOptions :: ProfilingOptions -> Task ProfilingOptions
		profilingOptions prof = updateInformation (title,"Diagnostics Options:") [] prof	
	
		consoleOptions :: ConsoleOptions -> Task ConsoleOptions
		consoleOptions co = updateInformation (title,"Console Options:") [] co	

// editing the evironment

currEnvName state 				= 	(getCurrEnv state).target_name
getCurrEnv  state   			= 	state.envTargets!!state.idx
updateEnv idx target state		= 	{ state & idx						= idx
											, envTargets 				= updateAt idx target state.envTargets
											, projectSettings.prjpaths 	= (state.envTargets!!idx).target_path 
											, projectSettings.target	= (state.envTargets!!idx).target_name
									}
selectEnv idx state				= 	{ state & idx						= idx
											, projectSettings.prjpaths 	= (state.envTargets!!idx).target_path 
											, projectSettings.target	= (state.envTargets!!idx).target_name
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
						(showError "Could not read environment file" path  @ const Void)
						(					 readEnvironmentFile (path </> (fromJust mbEnv)) 
							>>=  \ntargets -> update (\state -> {state & envTargets = state.envTargets ++ ntargets}) ideState
											 @ const Void)

selectEnvironment :: (Shared IDE_State) Int -> Task Void					
selectEnvironment ideState i
	=					get ideState
	>>= \state ->		update (selectEnv i) ideState
	>>|					storeProject ideState state.projectSettings state.projectName
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
		=	let compilerMessages = state.projectPath +++ projectName +++ ".log"  in
						exportTextFile compilerMessages ""	//Empty the log file...
			>>|			storeProject ideState state.projectSettings state.projectName
			>>|			callProcess "Clean Compiler - BatchBuild" [] (cleanPath +++ batchBuild) [state.projectPath +++ projectName +++ ".prj"] 
						-&&-
						viewSharedInformation (Title "Compiler Messages...") [] (externalFile compilerMessages) <<@ Window
			
		>>*			[ OnAction ActionClose always (\_ -> return Void)
					, OnAction ActionOk    always (\_ -> return Void)
					]

// editor

:: Replace	 =	{ search  	:: String
				, replaceBy :: String
				}
derive class iTask Replace

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
							, OnAction (Action ("File/Save As..."))   		  always (const (saveAs copy >>| editor` file))
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

		saveAs copy
			=				get copy
			>>= \content -> get ideState
			>>= \state ->	storeFileInPath state.projectPath fileName content <<@ Window

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

storeFileInPath :: !PathName !FileName !String -> Task !Bool
storeFileInPath name string path
	= 				accWorld (readDirectory path)
	>>= \content -> case content of
						Ok names 	-> select names
						_ 			-> return False
where
	select names
		=				enterChoice ("Store " +++ name,path) [] names
						-&&-
						updateInformation "File name" [] name
		>>*				[ OnAction ActionCancel always  (const (return False))
						, OnAction (Action "Browse")	hasValue (browse o getValue)
						, OnAction ActionSave			hasValue (write o getValue)
						]
	write (path,name)
		=				accWorld (writeFile (path </> name) "")
		>>|				return True


	browse ("..",name)  = storeFileInPath (takeDirectory path) name string
	browse (".",name)   = storeFileInPath path name string
	browse (npath,name)		
		=				accWorld (getFileInfo npath)
		>>= \content ->	case content of
							Ok info -> if info.directory
								(storeFileInPath npath name string)
								(storeFileInPath path name string)
							_		-> storeFileInPath path name string

showError :: String a -> Task a | iTask a
showError prompt val = (viewInformation ("Error",prompt) [] val >>= \_ -> return val) <<@ Window

currentDirectory :: Task !FilePath
currentDirectory 
	= 					accWorld getCurrentDirectory 
		>>= \content ->	case content of
							Ok dir -> return ("* " +++ dir +++ " *")
							_      -> showError "Could not obtain current directory name" ""

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

