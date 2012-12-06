module CleanEditor

/* A Clean IDE for a browser using the iTask system
Status: very drafty
*/

import iTasks, Text
import qualified Map
import projectManager

import SmallUtil, IDE_State

derive class iTask FileError

// It Starts here..

Start :: *World -> *World
Start world = startEngine start_ide world 

/* BUG? currentDirectory does not seem to return the  currentDirectory????
Start w = startEngine test w

test = currentDirectory													// determine directory of this CleanEditor
	>>= \dir ->		viewInformation "Direct = " [] dir

*/
/* BUGS:
- shares: not always up-to-date
- creation of a window not always possible
- current dir does not give current dir
- a project file newly created contains something which the other applications do not like (comparrison needed)
- global title not implemented
- radiobuttons & checkbuttons not implemented
*/

start_ide :: Task Void
start_ide 
	= 				init_ide													// initialize the IDE state
	>>|				parallel Void // (Title "Clean IDE") 						// BUG: global title not implemented
						[ (Embedded, topMenu)
						, (Embedded, projectFiles)
						, (Embedded, errorMessages)
						] <<@ SetLayout layout @ const Void
where
	layout = customMergeLayout (sideMerge TopSide 0 (sideMerge LeftSide 300 (sideMerge BottomSide 100 tabbedMerge)))

	init_ide 
		=				get_IDE_State											// read state as left from previous session, if any
		>>= \state -> 	init state.projectName 									
	where
		init ""																	// no project set
		=					currentDirectory									// determine directory of this CleanEditor
		//	>>= \dir ->		set_Project  "" dir									// BUG: currentDirectory does not return dir						
			>>= \dir ->		set_new_Project "" initialPath						// Fix: hardwired path
			>>|				readEnvironmentFile (cleanPath +++ "\\" +++ EnvsFileName) 	// read environment file used for communication with BatchBuild	
			>>= \env ->		setEnvironments env									// store settings in project
		init _ = return Void
// top menu

topMenu :: !(ReadOnlyShared (TaskList Void)) -> Task Void
topMenu ts 
	= 				get_IDE_State												// new session, first recover previous screen
	>>= \state -> 	openLastProject state.projectName  							// re-open last project
	>>|				openOpenedFiles state.openedFiles 							// re-open opened files
	>>|				forever (				(get_IDE_State 
							>>= \state -> 	(actionTask >>*	handleMenu state))	// construct main menu
							) 
where
	openLastProject "" 		= return Void
	openLastProject name	= reopenProject name

	openOpenedFiles [] 		=	return Void
	openOpenedFiles [f:fs]	=	launch (editor f ts) ts >>| openOpenedFiles fs 

	handleMenu state=:{projectName, openedFiles, recentFiles, recentProjects, envTargets}
	=	[ OnAction (Action "File/Open...") always (const (openFile ts))
		, OnAction (Action "File/Save All") (const (openedFiles <> [])) (const (saveAll openedFiles))
		] 
		++
		[ OnAction (Action ("File/Recent Files/" +++ fileName)) always (const (openFileAndEdit fileName ts)) 
		\\ fileName <- recentFiles
		] 
		++
		[ OnAction (Action ("File/Recent Projects/" +++ fileName +++ " (.prj)")) always (const (reopenProject fileName)) 
		\\ fileName <- recentProjects
		] 
		++
		[ OnAction (Action "Search/Search Identifier...")     ifProject (const (launch (search SearchIdentifier     SearchInImports ts) ts))
		, OnAction (Action "Search/Search Definition...")     ifProject (const (launch (search SearchDefinition     SearchInImports ts) ts))
		, OnAction (Action "Search/Search Implementation...") ifProject (const (launch (search SearchImplementation SearchInImports ts) ts))
		]
		++
		[ OnAction (Action "Project/New Project...")  always (const (launch (newProject) ts))
		, OnAction (Action "Project/Open Project...") never (const (openProject))							// temp to avoid selection
//		, OnAction (Action "Project/Open Project...") always (const (openProject))							// BUG does not show window
		, OnAction (Action ("Project/Bring Up To Date " +++ projectName +++ " (.prj)")) (const (projectName <> ""))
								 (const (launch (compile projectName <<@ Window) ts))	
		, OnAction (Action "Project/Project Options...") ifProject (const (changeProjectOptions))
		]
		++
		[ OnAction (Action (selectedEnvironment +++ "/Edit " +++ currentEnvName)) always (const (editEnvironment))
		, OnAction (Action (selectedEnvironment +++ "/Import...")) always (const (addEnvironment))
		:[ OnAction (Action (selectedEnvironment +++ "/Select/" +++ target.target_name)) always (const (selectEnvironment i)) 
		 \\ target <- envTargets & i <- [0..]
		 ]
		]
		++ // BUG: temp fix: the latest state is not always shown: known bug in the current implementation
		[ OnAction (Action "Temp/Refresh") always (const (return Void)) ]	
	where

		ifProject = const (projectName <> "")

		currentEnvName			= currEnvName state
		selectedEnvironment 	= "_" +++ currentEnvName 

// project pane: to do

projectFiles :: (ReadOnlyShared (TaskList Void)) -> Task Void
projectFiles ts
	=				get_IDE_State
	>>= \state ->	findAllModulesInPaths "icl" cleanPath (state.envTargets!!state.idx).target_path
	>>= \dirFiles -> ( showAndSelect state (map (\(d,fs) -> (d,map dropExtension fs)) dirFiles)
						-||-
						watch_IDE_State (\curstate -> curstate.idx <> state.idx) (return Void)
					 )
	>>|				projectFiles ts
where
	showAndSelect state dirFiles 
		=			(showFiles <<@ AfterLayout (tweakControls (map noAnnotation)) 
					>&>
					handleSelected ) 
					<<@ SetLayout {autoLayout & parallel = \prompt defs -> sideMerge BottomSide 100 sequenceMerge prompt (reverse defs)}
	where
		showFiles 
			= 				enterChoice (Title ("project: " +++ state.projectName +++ " using " +++ (state.envTargets!!state.idx).target_name)) 
								[ChooseWith ChooseFromTree id] (mkTree dirFiles)
		where
			mkTree dirfiles = Tree [Node dir [Leaf file \\ file <- files] \\ (dir,files) <- dirfiles]

		handleSelected selected
			=	forever (		viewSharedInformation (Title "Selected:") [] selected  @? onlyJust
							>>* [OnAction (Action "Open .icl") hasValue (\v -> openFileAndEdit (select (getValue v) ".icl") ts)
								,OnAction (Action "Open .dcl") hasValue (\v -> openFileAndEdit (select (getValue v) ".dcl") ts)
								]
						)
		where							
			select chosen extension = cleanPath +++ hd [dir \\ (dir,files) <- dirFiles | isMember chosen files] +++ "\\" +++ chosen +++ extension
	
			onlyJust (Value (Just v) s) = Value v s
			onlyJust _					= NoValue

	noAnnotation (c,_) = (c,'Map'.newMap)

// errorMessages pane, shows error message produced by compiler in error file

errorMessages :: (ReadOnlyShared (TaskList Void)) -> Task Void
errorMessages _ 
	= 				get_IDE_State
	>>= \state ->	let sharedError = externalFile errorFile in
					viewSharedInformation (Title "Error Messages...") [ViewWith (\txt -> Note txt)] sharedError
					@ const Void 

// handling top menu commands

// open and saving files to edit ...	  

openFile :: (ReadOnlyShared (TaskList Void)) -> Task Void
openFile ts
	=				get_IDE_State
	>>= \state ->	selectFileInPath state.projectPath (\_ -> True) <<@ Window
	>>= \(path,r)-> if (isNothing r)
						(return Void)
						(openFileAndEdit (fromJust r) ts)

openFileAndEdit :: FileName (ReadOnlyShared (TaskList Void)) -> Task Void
openFileAndEdit fileName ts
	=				get_IDE_State
	>>= \state ->	if (isMember fileName state.openedFiles)
						(return Void)										// file already open in an editor
						(			addFilesAdmin fileName 
						>>|			launch (editor fileName ts) ts 	
						)

closeEditFile :: FileName -> Task Void
closeEditFile fileName
	=				removeFileAdmin fileName

saveAll :: [FileName]  -> Task Void
saveAll [] 
	= return Void
saveAll [name:names]
	=				get (sharedStore name "")				// read out latest content of the editor from the internal store
	>>= \content ->	set content (externalFile name)			// and store it in the actual file			
	>>|				saveAll names 						

// search department

derive class iTask SearchWhat, SearchWhere
import _SystemStrictLists

search what whereSearch _  
	= 				get_IDE_State
	>>= \state ->	searching state "" what whereSearch ([],[])  <<@ Window
	
where
	searching state=:{projectName,projectPath} identifier what whereSearch found
		=			(viewInformation (length (snd found) +++> " modules searched, " +++> length (fst found) +++> " contained " +++> identifier) [] found
					||-
					updateInformation (Title ("Find: " +++ identifier)) [] (identifier,what,whereSearch)) 
		>>*			[ OnAction ActionCancel   always   (const (return Void))
					, OnAction (Action "Search") hasValue (performSearch o getValue)
					] 
	where
		performSearch (identifier,what,whereSearch)
			= 				searchTask what whereSearch identifier (projectPath, projectName +++ ".icl") searchPaths
			>>=	\found	->	searching state identifier what whereSearch found

		searchPaths :: List !String
		searchPaths = (state.envTargets!!state.idx).target_path

// opening and storing projects... 

newProject :: Task Void
newProject 
	=				updateInformation "Set name of project..." [] "" <<@ Window
	>>*				[ OnAction ActionCancel   always   (const (return Void))
					, OnAction (Action "Set") hasValue (\v -> storeNewProject (getValue v) initialPath)
					]

storeNewProject "" _
	=				return Void 
storeNewProject projectName projectPath 
	=				set_new_Project projectName projectPath 
//	>>|				storeProject  

storeProject :: Task Void
storeProject  
	= 				get_IDE_State
	>>= \state ->	saveProjectFile state.projectSettings (state.projectPath +++ state.projectName +++ ".prj") state.cleanPath 
	>>= \ok ->		if (not ok)	(showError "Could not store project file !" Void) (return Void)
						
	
openProject :: Task Void
openProject  
	= 				get_IDE_State
	>>= \state ->	selectFileInPath state.projectPath (\name -> takeExtension name == "prj" || takeExtension name == "") <<@ Window
	>>= \(path,r)-> if (isNothing r) (return Void) (reopenProject (fromJust r))

reopenProject :: !ModuleName -> Task Void
reopenProject projectName 
	= 								get_IDE_State
	>>= \state ->					readProjectFile (state.projectPath +++ projectName +++ ".prj") state.cleanPath 
	>>= \(project,ok,message) ->	if (not ok)
										(showError "Read Error..."  message  @ const Void)
										(open_Project projectName state.projectPath project)
where
	name = dropExtension projectName

changeProjectOptions :: Task Void
changeProjectOptions 
	= changeProjectOptions` <<@ Window
where
	changeProjectOptions`
		=				get_IDE_State
		>>= \state ->	changeProjectOptions`` state.projectName state.projectSettings (fromProject state.projectSettings)
		
	changeProjectOptions``	projectName project (rto,diagn,prof,co)	
		=				runTimeOptions title rto
		>>= \rto ->		diagnosticsOptions title diagn
		>>= \diagn -> 	profilingOptions title prof
		>>= \prof ->	consoleOptions title co
		>>= \co ->		update_Project (toProject project (rto,diagn,prof,co)) 
//		>>|				storeProject  
	where
		title = "Set Options of Project: " +++ projectName	

		runTimeOptions :: String RunTimeOptions -> Task RunTimeOptions
		runTimeOptions title rto = updateInformation (title,"Run-Time Options:")[] rto 
		
		diagnosticsOptions :: String DiagnosticsOptions -> Task DiagnosticsOptions
		diagnosticsOptions title diagn = updateInformation (title,"Diagnostics Options:") [] diagn
		
		profilingOptions :: String ProfilingOptions -> Task ProfilingOptions
		profilingOptions title prof = updateInformation (title,"Diagnostics Options:") [] prof	
	
		consoleOptions :: String ConsoleOptions -> Task ConsoleOptions
		consoleOptions title co = updateInformation (title,"Console Options:") [] co	

// editing the evironment

currEnvName state 				= 	(getCurrEnv state).target_name
getCurrEnv  state   			= 	state.envTargets!!state.idx

editEnvironment :: Task Void
editEnvironment 
	= editEnvironment` <<@ Window
where
	editEnvironment`
		=				get_IDE_State
		>>= \state ->	updateInformation (Title ("Edit " +++ currEnvName state))[] (fromTarget (getCurrEnv state))
		>>*				[ OnAction ActionCancel   always   (const (return Void))
						, OnAction (Action "Set") hasValue (\env -> updateEnvironment state.idx (toTarget (getValue env))) 
						]
	
addEnvironment :: Task Void					
addEnvironment 						
	=					get_IDE_State
	>>= \state ->		selectFileInPath state.projectPath (\name -> takeExtension name == "env" || takeExtension name == "") <<@ Window
	>>= \(path,mbEnv)-> if (isNothing mbEnv)
						(showError "Could not read environment file" path  @ const Void)
						(					readEnvironmentFile (path </> (fromJust mbEnv)) 
							>>=  \nenv -> 	add_Environments nenv)

selectEnvironment :: Int -> Task Void					
selectEnvironment i
	=					select_Environment i
//	>>|					storeProject  
	 					@ const Void

// compile project... 

compile :: !ModuleName -> Task Void
compile projectName  
	=				get_IDE_State
	>>= \state ->	if (state.openedFiles == []) 
					(compile` state)
					(	viewInformation ("Clean Compiler","Do you want to save all ?") [] Void 
					>>* [ OnAction ActionYes 	always (const (		saveAll state.openedFiles 
																>>| storeProject
																>>| compile` state))
						, OnAction ActionNo  	always (const (compile` state))
						, OnAction ActionCancel always (const (return Void))
						]
					)
where	
	compile` state
		=	let compilerMessages = state.projectPath +++ projectName +++ ".log"  in
						exportTextFile compilerMessages ""	//Empty the log file...
//			>>|			storeProject  
			>>|			callProcess "Clean Compiler - BatchBuild" [] batchBuild [state.projectPath +++ projectName +++ ".prj"] 
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
editor :: FileName (ReadOnlyShared (TaskList Void)) -> Task Void
editor fileName ts = editor` (externalFile fileName) // <<@ Window
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
			>>*	 			[ OnAction  ActionClose 		 				  always (const (closeEditFile fileName))
							, OnAction (Action ("File/Close " +++ fileName))  always (const (closeEditFile fileName))
							, OnAction (Action ("File/Save " +++ fileName))   always (const (save copy >>| editor` file))
							, OnAction (Action ("File/Save As..."))   		  always (const (saveAs copy >>| editor` file))
							, OnAction (Action ("File/Revert " +++ fileName)) always (const (editor` file))
							, OnAction (Action ("File/Open " +++ other fileName)) 
																			  (const isIclOrDcl) 
																			  (const (openFileAndEdit fileName ts))
	
							, OnAction (Action ("Project/Set Project/" +++ noSuffix +++ " (.prj)")) 
																			  (const isIcl)
																			  (const (storeNewProject noSuffix initialPath // to fix
																			  				 >>| editor` file )) 
							]

		noSuffix = RemoveSuffix fileName

		save copy
			=				get copy
			>>= \content -> set	content file		

		saveAs copy
			=				get copy
			>>= \content -> get_IDE_State
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

noHints = AfterLayout (tweakControls (\controls -> [(c,'Map'.del VALID_ATTRIBUTE ('Map'.del HINT_ATTRIBUTE m)) \\ (c,m) <- controls]))

