module CleanIDE

/* A Clean IDE for a browser using the iTask system
Status: very drafty
*/

import iTasks, Text
import qualified Map
import projectManager

import SmallUtil, IDE_State, CleanEditor
from StdFunc import seq

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
*/

start_ide :: Task Void
start_ide 
	= 				init_ide													// initialize the IDE state
	>>|				parallel Void // (Title "Clean IDE") 						// BUG: global title not implemented
						[ (Embedded, topMenu)
						, (Embedded, projectPane)
						, (Embedded, compilerMessages)
						] <<@ SetLayout layout @ const Void
where
	layout = customMergeLayout (sideMerge TopSide 0 (sideMerge LeftSide 250 (sideMerge BottomSide 100 tabbedMerge)))

	init_ide 
		=				get_IDE_State											// read state as left from previous session, if any
		>>= \state -> 	init state												// initiate state 								
	where
		init state =:{projectName = ""	}											// no project set
			=				currentDirectory										// determine directory of this CleanEditor
		//	>>= \dir ->		set_Project  "" dir										// BUG: currentDirectory does not return dir						
			>>= \dir ->		set_new_Project initialPath	""						// Fix: hardwired path
			>>|				readEnvironmentFile (cleanPath +++ "\\" +++ EnvsFileName) 	// read environment file used for communication with BatchBuild	
			>>= \env ->		setEnvironments env										// store settings in projectget_IDE_State											// read state as left from previous session, if any
			>>|				findAllModulesInPaths "icl" cleanPath (env!!0).target_path // find all modules in chosen environment
			>>= \all ->		setAllFilesInEnv all									// and store
		init _ = return Void
// top menu

topMenu :: !(ReadOnlyShared (TaskList Void)) -> Task Void
topMenu ts 
	= 				get_IDE_State												// new session, first recover previous screen
	>>= \state -> 	openLastProject state.projectName 							// re-open last project
	>>|				openEditorOnFiles state.openedFiles ts						// re-open editor on administated files
	>>|				forever (				(get_IDE_State 
							>>= \state -> 	(actionTask >>*	handleMenu state))	// construct main menu
							) 
where
	handleMenu state=:{projectName, openedFiles, recentFiles, recentProjects, envTargets}
	=	[ OnAction (Action "File/Open...") always (const (openFileSelectorAndEdit ts))
		, OnAction (Action "File/Save All") (const (openedFiles <> [])) (const (saveAll openedFiles))
		] 
		++
		[ OnAction (Action ("File/Recent Files/" +++ fileName)) always (const (launchEditorAndAdministrate fileName ts)) 
		\\ fileName <- recentFiles
		] 
		++
		[ OnAction (Action ("File/Recent Projects/" +++ fileName +++ " (.prj)")) always (const (reopenProject fileName)) 
		\\ fileName <- recentProjects
		] 
		++
		[ OnAction (Action "Search/Search Identifier...")     ifProject (const (launch (search SearchIdentifier     ts) ts))
		, OnAction (Action "Search/Search Definition...")     ifProject (const (launch (search SearchDefinition     ts) ts))
		, OnAction (Action "Search/Search Implementation...") ifProject (const (launch (search SearchImplementation ts) ts))
		]
		++
		[ OnAction (Action "Project/New Project...")  always (const (launch (newProject) ts))
		, OnAction (Action "Project/Open Project...") never (const (openProject))							// temp to avoid selection
//		, OnAction (Action "Project/Open Project...") always (const (openProject))							// BUG does not show window
		, OnAction (Action ("Project/Bring Up To Date " +++ projectName +++ " (.prj)")) (const (projectName <> ""))
								 (const (launch (compile projectName <<@ Window) ts))	
		, OnAction (Action "Project/Project Options...")    	ifProject (const (changeProjectOptions))
		, OnAction (Action "Project/Show/All Modules") 			always    (const (setProjectPaneOption InEnvironment))
		, OnAction (Action "Project/Show/Modules In Project") 	ifProject (const (setProjectPaneOption InProject))
		, OnAction (Action "Project/Show/Not Used") 			ifProject (const (setProjectPaneOption NotUsed))
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

// project & environment file selector pane

projectPane :: (ReadOnlyShared (TaskList Void)) -> Task Void
projectPane ts
	=				get_IDE_State
	>>= \state -> 	( (showAndSelect state @ const Void)														// show search path directories 
						-||-
					  (watch_IDE_State (\curstate -> curstate.idx <> state.idx || 								// environment changed
												     curstate.projectName <> state.projectName ||				// project changed
												     not (curstate.moduleOptions === state.moduleOptions) ) 	// show options changed 
									 (return Void)) <<@ SetLayout (partLayout 0)
					)
	>>*				[ OnValue  ifStable (const recalculate)														// recompute on indicated change
					, OnAction (Action "Project/Show/Refresh") always (const recalculate)						// recompute on action end user
					]
where
	layout = SetLayout {autoLayout & parallel = \prompt defs -> sideMerge BottomSide 100 sequenceMerge prompt (reverse defs)} //???

	recalculate
		=				get_IDE_State
		>>= \state ->	findAllModulesInPaths "icl" cleanPath [! idePath:(state.envTargets!!state.idx).target_path !] 	// find all modules in environment
		>>= \allM ->	findAllModulesInProject cleanPath (idePath,state.projectName) allM								// determine which one are used
		>>= \all ->		setAllFilesInEnv all																			// and store in global state
		>>|				projectPane ts

	showAndSelect state  
		=	( showFiles <<@ AfterLayout (tweakControls (map noAnnotation)) 
			  >&>
			  handleSelected 
			) <<@ layout
	where
		showFiles 
			=	enterChoice (Title ( promptName state.projectName +++ " + " +++
								    (state.envTargets!!state.idx).target_name +++
								    promptOptions state.moduleOptions
									)) 
						[ChooseWith ChooseFromTree toView ] (mkTree state.moduleOptions state.allFilesInEnv)
				@ fromEither
		where
			promptName "" 	= "No project"
			promptName name = "Project: " +++ name 

			promptOptions InEnvironment = " + Show All Modules"
			promptOptions InProject 	= " + Show Project Modules"
			promptOptions NotUsed 		= " + Show Unused Modules"

			toView (Left dir) 			= dir
			toView (Right (name,mark))	= name +++ mark

			fromEither (Left dir) 		= dir
			fromEither (Right (name,_)) = name

			mkTree :: !ModuleOptions ![(!DirPathName,![Module])] -> Tree (Either !DirPathName !(!ModuleName,!String))
			mkTree option dirfiles = Tree (removeEmpties (seq [insertModule dir ms \\ (dir,ms) <- dirfiles] []))
			where
				insertModule dir ms nodeList = insertModule` (split "\\" dir) ms nodeList
				where
					insertModule` [] ms nodeList 			= nodeList ++ [Leaf (Right (insertLeaf option m)) \\ m <- ms | checkUsage option m.isUsed]
					insertModule` ["":pathR] ms nodeList	= insertModule` pathR ms nodeList
					insertModule` path=:[nodeP:pathR] ms [node=:(Node (Left nodeL) nodes):nodesR]
						| nodeP == nodeL					= [Node (Left nodeL) (insertModule` pathR ms nodes):nodesR]
						| otherwise							= [node:insertModule` path ms nodesR]
					insertModule` path ms [leaf=:(Leaf _):nodesR] 
															= [leaf:insertModule` path ms nodesR]
					insertModule` [nodeP:pathR] ms [] 		= [Node (Left nodeP) (insertModule` pathR ms [])]

				insertLeaf InEnvironment m = if m.isUsed (m.moduleName,"*") (m.moduleName,"")
				insertLeaf InProject 	 m = (m.moduleName,"")
				insertLeaf NotUsed 		 m = (m.moduleName,"")

				checkUsage InEnvironment _ 		= True	// show all modules			
				checkUsage InProject 	True	= True	// show only modules used in project
				checkUsage NotUsed		False	= True	// show unused modules
				checkUsage _			_		= False	// don't show			

				removeEmpties [] 	 		 = []
				removeEmpties [Node _ []:ds] = removeEmpties ds
				removeEmpties [Node n ln:ds] = [Node n (removeEmpties ln):removeEmpties ds]
				removeEmpties [Leaf a:ds] 	 = [Leaf a:removeEmpties ds] 

		handleSelected selected
			=	forever (		viewSharedInformation (Title "Selected:") [] selected @? onlyJust
							>>* [OnAction (Action "Open .icl") hasValue (\v -> openSelected (getValue v) ".icl" ts)
								,OnAction (Action "Open .dcl") hasValue (\v -> openSelected (getValue v) ".dcl" ts)
								]
						)
		where							
			openSelected :: !ModuleName !String !(ReadOnlyShared (TaskList Void)) -> Task Void
			openSelected chosen extension ts
				=				get_IDE_State
				>>= \state ->	launchEditorAndAdministrate (selectFrom state.allFilesInEnv) ts   
			where
				selectFrom :: ![(!DirPathName,![Module])] -> FileName
				selectFrom dirFiles = cleanPath +++ hd [dir \\ (dir,files) <- dirFiles | isMember chosen (map (\{moduleName} -> moduleName) files)] +++ "\\" +++ chosen +++ extension
		
			onlyJust (Value (Just v) s) = Value v s
			onlyJust _					= NoValue

	noAnnotation (c,_) = (c,'Map'.newMap)

// compilerMessages pane, shows error message produced by compiler in error file

compilerMessages :: (ReadOnlyShared (TaskList Void)) -> Task Void
compilerMessages _ 
	= 				get_IDE_State
	>>= \state ->	let sharedError = externalFile errorFile in
					viewSharedInformation (Title "Error Messages...") [ViewWith (\txt -> Note txt)] sharedError
					@ const Void 

// search department

import _SystemStrictLists

:: FoundTable =	{ file 		:: !FileName
				, directory	:: !DirPathName
				, line	 	:: !Int
				, pos		:: !Int
				, kind		:: !String
				}
derive class iTask FoundTable

search searchOption ts  
	= 				get_IDE_State
	>>= \state ->	searching state "" searchOption state.moduleOptions []  <<@ Window
where
	searching state identifier searchOption moduleOptions found
		=			(updateInformation (Title "Find") [] identifier	-&&- selectSearchOptions)
					-||
					handleFound 0 (toTable found)
			>>*		[ OnAction ActionClose     always   (const (return Void))
					, OnAction (Action "Find") (ifValue (\(s,_) -> s <> "")) (performSearch o getValue)
					] 
	where
		selectSearchOptions
			=	updateChoice Void [ChooseWith ChooseFromRadioButtons searchOptionView]  [SearchDefinition,SearchImplementation,SearchIdentifier] searchOption
				-&&-
				updateChoice Void [ChooseWith ChooseFromRadioButtons moduleOptionsView] [InEnvironment,InProject,NotUsed] moduleOptions

		handleFound i []
			= viewInformation "" [] (if (identifier == "") "" (identifier +++ " has not been found...")) @ const Void
		handleFound i table
			=		(updateChoice (identifier +++ " found in:") [ChooseWith ChooseFromGrid id] table (table!!i) 
					>&>
					handleSelected)					
			>>*    [OnValue  ifStable (const (handleFound (if (length table < (i+1) (i+1) i) table))]	
		where
			handleSelected selected
				= 	viewSharedInformation "Selected:" [ViewWith (\entry -> (fromJust entry).file)] selected
				>>*	[OnAction (Action "Open...") always (openFileSelected o fromJust o getValue)
					,OnAction ActionNext always (const (return Void))
					]
			where
				openFileSelected sel 
					= launchEditorAndAdministrate (cleanPath +++ sel.FoundTable.directory +++ "\\" +++ sel.file) ts 
		

		toTable found = [	{ kind 		= kind
							, file 		= file
							, directory	= directory
							, line	 	= line
							, pos		= pos
							}
						\\ ((directory,file),poslist) <- sortFound found, (kind,line,pos) <- toList poslist ]
		where
			sortFound = sortBy (\((s1,f1),l1) ((s2,f2),l2) -> f1 <= f2)
			
			toList PosNil				  = []
			toList (Pos line pos poslist) = [("identifier",       line, pos):toList poslist]
			toList (Cls line pos poslist) = [("class definition", line, pos):toList poslist]
			toList (Ins line pos poslist) = [("class instance",   line, pos):toList poslist]
	
		searchOptionView SearchDefinition		= "Find definition"
		searchOptionView SearchImplementation	= "Find implementation"
		searchOptionView SearchIdentifier		= "Find identifier"
	
		moduleOptionsView InEnvironment			= "Search in Environment"
		moduleOptionsView InProject				= "Search in Project"
		moduleOptionsView NotUsed				= "Search in unused Modules"

		performSearch (identifier,(searchOption,moduleOptions))
			=	searchFor [] [(path, moduleName) 	\\ (path,modules) <- state.allFilesInEnv
													, {moduleName,isUsed} <- modules
													| case (moduleOptions,isUsed) of
														(InProject,True)  -> True
														(NotUsed,False)   -> True 
														(InEnvironment,_) -> True
														_ -> False] 
		where
			searchFor = case searchOption of
							SearchDefinition 		-> searchDefinition ".dcl"
							SearchImplementation	-> searchDefinition ".icl"
							SearchIdentifier		-> searchIdentifier 

			searchDefinition suffix found [] =  searching state identifier searchOption moduleOptions found
			searchDefinition suffix found [(p,f):fs]
				# fileName = f +++ suffix
				=				findDefinition identifier (cleanPath +++ p +++ "\\" +++ fileName)
				>>= \list ->	case list of
									PosNil ->	searchDefinition suffix found fs 
									list -> 	searchDefinition suffix [((p,fileName),list):found] fs 
					
			searchIdentifier found [] =  searching state identifier searchOption moduleOptions found
			searchIdentifier found [(p,f):fs]
				# defFile = f +++ ".dcl" 
				# impFile = f +++ ".icl"
				=				findIdentifier identifier (cleanPath +++ p +++ "\\" +++ defFile)
				>>= \dList ->	findIdentifier identifier (cleanPath +++ p +++ "\\" +++ impFile)
				>>= \iList ->	case (dList,iList) of
									(PosNil,PosNil) ->	searchIdentifier found fs 
									(PosNil,iList)  -> 	searchIdentifier [((p,impFile),iList):found] fs 
									(dList,PosNil)  -> 	searchIdentifier [((p,defFile),dList):found] fs 
									(dList,iList)   -> 	searchIdentifier [((p,defFile),dList),((p,impFile),iList):found] fs 

// opening and storing projects... 

newProject :: Task Void
newProject 
	=				updateInformation "Set name of project..." [] "" <<@ Window
	>>*				[ OnAction ActionCancel   always   (const (return Void))
					, OnAction (Action "Set") hasValue (\v -> storeNewProject (getValue v) initialPath)
					]
where
	storeNewProject "" _
		=				return Void 
	storeNewProject projectName projectPath 
		=				set_new_Project projectPath projectName  

storeProject :: Task Void
storeProject  
	= 				get_IDE_State
	>>= \state ->	saveProjectFile state.projectSettings (state.projectPath +++ "\\" +++ state.projectName +++ ".prj") state.cleanPath 
	>>= \ok ->		if (not ok)	(showError "Could not store project file !" Void) (return Void)
						
	
openLastProject "" 		= return Void
openLastProject name	= reopenProject name

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
										(open_Project state.projectPath projectName project)
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
			>>|			callProcess "Clean Compiler - BatchBuild" [] batchBuild [state.projectPath +++ projectName +++ ".prj"] 
						-&&-
						viewSharedInformation (Title "Compiler Messages...") [] (externalFile compilerMessages) <<@ Window
			
		>>*			[ OnAction ActionClose always (\_ -> return Void)
					, OnAction ActionOk    always (\_ -> return Void)
					]

