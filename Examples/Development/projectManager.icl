implementation module projectManager

import iTasks
import EditorUtil

import PmTypes, PmProject, PmParse, PmEnvironment, UtilStrictLists
import Directory, File
						   
derive class iTask 	RunTimeOptions, DiagnosticsOptions, ProfilingOptions, TimeProfileOptions, HeapProfileOptions, HeapProfile
derive class iTask  Environment, ToolsOptions

derive class iTask	Project, LinkOptions, ApplicationOptions, CompilerOptions, ModInfo, ABCLinkInfo
derive class iTask	ModEditOptions, EditWdOptions, EditOptions, OptionalWindowPosAndSize, WindowPos_and_Size, NewlineConvention  
derive class iTask	[!!], InfListItem, ListTypes, Output, LinkMethod, ProjectDynamicInfo, StaticLibInfo, CodeGenOptions 
derive class iTask	UndefModule, UndefSymbol 
derive class iTask	IdentifierPositionList
derive class iTask	Target, CompileMethod, Processor

// project files handling

toProject 	:: Project (RunTimeOptions, DiagnosticsOptions, ProfilingOptions, ConsoleOptions) -> Project
toProject p=:{applicationopt,codegenopt} (rto,do,po,co)
	= { p	& applicationopt.initial_heap_size 		= rto.initialHeapSize
		    , applicationopt.heap_size_multiple		= rto.heapSizeIncreaseStep
		    , applicationopt.hs						= rto.maximumHeapSize
		    , applicationopt.ss						= rto.stackSize
		    , applicationopt.set					= do.showExecutionTime
		    , applicationopt.sgc					= do.showGarbageCollections
		    , applicationopt.pss					= do.printStackSize
		    , applicationopt.write_stderr_to_file	= do.checkStacks
		    , codegenopt.cs							= do.showExecutionTime
		    , codegenopt.ci							= do.checkIndices
		    , applicationopt.stack_traces			= po.timeProfile === StackTraceOny ||
		    										  po.timeProfile === TimeProfileAndStackTrace
		    , applicationopt.profiling				= po.timeProfile === TimeProfileAndStackTrace
		    , applicationopt.memoryProfiling		= not (po.heapProfile === NoHeapProfiling)
		    , applicationopt.memoryProfilingMinimumHeapSize
		    										= case po.heapProfile of
		    											(HeapProfile rec) -> rec.minimumHeapProfile
		    											_				  -> 0
			, applicationopt.o						= co
	  }

fromProject :: Project -> (RunTimeOptions, DiagnosticsOptions, ProfilingOptions, ConsoleOptions)
fromProject p=:{applicationopt,codegenopt} = (rto,do,po,co)
where
	rto =	{ initialHeapSize			= applicationopt.initial_heap_size
			, heapSizeIncreaseStep		= applicationopt.heap_size_multiple
			, maximumHeapSize 			= applicationopt.hs			
			, stackSize					= applicationopt.ss			
			}
	do 	=	{ showExecutionTime			= applicationopt.set		
			, showGarbageCollections	= applicationopt.sgc		
			, printStackSize			= applicationopt.pss		
			, writeStderrToFile			= applicationopt.write_stderr_to_file
			, checkStacks				= codegenopt.cs	
			, checkIndices				= codegenopt.ci		
			}
	po 	=	{ timeProfile				= case (applicationopt.stack_traces,applicationopt.profiling) of
											(True,True) 	-> TimeProfileAndStackTrace
											(True,False)	-> StackTraceOny
											_ 				-> NoTimeProfiling
			, heapProfile				= if applicationopt.memoryProfiling
												(HeapProfile {minimumHeapProfile = applicationopt.memoryProfilingMinimumHeapSize})
												NoHeapProfiling
			}
	co	=	applicationopt.o

readProjectFile	:: !ProjectPath !CleanPath -> Task (Project, Bool, String)
readProjectFile projectPath cleanAppDir 
	= accWorld (accFiles (ReadProjectFile projectPath cleanAppDir))

saveProjectFile :: !Project !ProjectPath !CleanPath  -> Task Bool
saveProjectFile project projectPath cleanPath  
	= accWorld (accFiles (SaveProjectFile projectPath project cleanPath))

initProject :: !ModuleName -> Project
initProject main_module_file_name
	= PR_NewProject	main_module_file_name editWdOptions compilerOptions codeGenOptions applicationOptions list linkOptions
where
	editWdOptions 		= 	{	eo 			= { newlines = NewlineConventionNone }
							,	pos_size	= NoWindowPosAndSize 
							}
	compilerOptions		= DefaultCompilerOptions
	codeGenOptions		= DefCodeGenOptions
	applicationOptions	= DefApplicationOptions
	list				= [!!]
	linkOptions			= DefaultLinkOptions

// environments handling

initTarget :: Target
initTarget = t_StdEnv

toTargets :: ![Environment] -> [Target]
toTargets envs = map toTarget envs

toTarget :: !Environment -> Target
toTarget env 
	= { initTarget	& target_name 			= env.environmentName 
					, target_path			= ListToStrictList env.paths  	
					, target_comp			= env.toolsOption.compiler			
					, target_cgen			= env.toolsOption.codeGenerator
					, target_link			= env.toolsOption.staticLinker			
					, target_dynl			= env.toolsOption.dynamicLinker			
					, target_vers			= env.toolsOption.versionOfAbcCode				
					, env_64_bit_processor	= env.toolsOption.runOn64BitProcessor
	  }

fromTargets :: ![Target] -> [Environment]
fromTargets targets = map fromTarget targets

fromTarget :: !Target -> Environment
fromTarget target 
	= 	{ environmentName	= target.target_name
		, paths				= StrictListToList target.target_path
		, toolsOption		=	{ compiler				= target.target_comp
								, codeGenerator			= target.target_cgen
								, staticLinker			= target.target_link
								, dynamicLinker			= target.target_link
								, versionOfAbcCode		= target.target_vers
								, runOn64BitProcessor 	= target.env_64_bit_processor
								}		
		}	

readEnvironmentFile :: !FilePathName -> Task ![Target]
readEnvironmentFile env 
	= 				accWorld (fileExists env)
	>>= \ok ->		if (not ok) 
						(showError ("Cannot read" +++ env) [])
						(accWorld (openEnvironments ""  env))

saveEnvironmentFile ::  !FilePathName ![Target]  -> Task !Bool
saveEnvironmentFile file targets
	= accWorld (saveEnvironments file targets)
	

// find all modules in search paths...

findAllModulesInPaths :: !Extension !DirPathName !(List !DirPathName) -> Task ![(!DirPathName,![Module])]
findAllModulesInPaths extension rootDir searchpaths = accWorld (searchDisk` searchpaths [])
where
	searchDisk` [!!] found world = (reverse (map (\(d,ms)-> (d,map mkModule (sort ms))) found),world) // keep original order in paths
	searchDisk` [!path:paths!] found world
	# (content,world) = readDirectory dirName world
	= case content of
		Ok names 	-> searchDisk` paths [addFiles names:found] world
		_ 			-> searchDisk` paths found world
	where
		addFiles names = (path, [dropExtension name \\ name <- names | takeExtension name == extension]) 
		dirName = rootDir +++ path

	mkModule m = {isUsed = False, moduleName = m}

// determine which modules are used in the project 

findAllModulesInProject :: !DirPathName !(!DirPathName,!ModuleName) ![(!DirPathName,![Module])] -> Task ![(!DirPathName,![Module])] 
findAllModulesInProject rootDir (dirName,"") envPaths
	=				return [(dirName,[{isUsed = False,moduleName = ""}]):envPaths]
findAllModulesInProject rootDir (dirName,startModule) envPaths
	= 				findImports     rootDir (dirName,startModule) ".icl"		// find imported modules 
	>>= \imp ->		findAllImported ([(dirName,startModule)],imp) 				// find all imported modules
where
	findAllImported (found, []) 												// all imported modules have been searched
		=			return (squeeze2 found envPaths)
	findAllImported (found, [mod:mods]) 										// investigate module
		# dir = [d \\ (d,ns) <- envPaths, n <- ns | n.moduleName == mod]			// search module in paths					
		| isEmpty dir = findAllImported (found,mods) 							// cannot find module in path, skip	
		# dirName = hd dir														// directory found
		=			 findImports rootDir (dirName,mod) ".dcl"					// search modules imported in mod.dcl 
		>>= \impd -> findImports rootDir (dirName,mod) ".icl"					// search modules impoted in mod.icl
		>>= \impi -> findAllImported (removeExamined [(dirName,mod):found] (removeDup (impd ++ impi ++ mods))) 
	where
		removeExamined :: [(!DirPathName,!ModuleName)] ![ModuleName] ->  ([(!DirPathName,!ModuleName)],![ModuleName])
		removeExamined examined todo 
		# examinedMods = map snd found
		= (examined,[m \\ m <- todo | not (isMember m examinedMods)])	

	squeeze2 [] envPaths 
		= envPaths
	squeeze2 [(dir,name):rest] envPaths
		= squeeze2 rest (insert (dir,name) envPaths)
	where
		insert (dir,name) [] 
					= []
		insert (dir,name) [(d,ms):envPaths]
		| dir == d 	= [(d,[{isUsed = if (moduleName == name) True isUsed, moduleName = moduleName} \\ {isUsed,moduleName} <- ms])
					  :envPaths
					  ] 
		= [(d,ms):insert (dir,name) envPaths]

findImports	rootDir (dirName,moduleName) extension
	=	findDefinitionInFile "" (rootDir +++ dirName +++ "\\" +++ moduleName +++ extension) True @ (StrictListToList o fst) 

findDefinition	:: !Identifier !FileName -> Task !IdentifierPositionList
findDefinition identifier fileName
	=	findDefinitionInFile identifier fileName False @ snd

findDefinitionInFile identifier fileName showImports
	= accWorld (accFiles (FindDefinitionInFile showImports [!!] identifier fileName))

findIdentifier :: !Identifier !FileName  -> Task !IdentifierPositionList
findIdentifier identifier  fileName 
	=	findIdentifierInFile identifier fileName False @ snd

findIdentifierInFile identifier fileName showImports
	= accWorld (accFiles (FindIdentifiersInFile showImports [!!] identifier fileName))


/*

// search department

findAllModulesInPaths` :: !String !(List !DirPathName) -> Task ![(!DirPathName,!FileName)]
findAllModulesInPaths` extension searchpaths
	= 						findAllModulesInPaths extension "" searchpaths
	>>= \res ->				return [(path,name) \\ (path,names) <- res, name <- names]


searchTask :: !SearchWhat !SearchWhere !Identifier !(!DirPathName,!FileName)  !(List !DirPathName) -> Task (![(!(!DirPathName,!FileName),!IdentifierPositionList)],![FileName])
searchTask what searchWhere identifier path_modulename searchPaths 
	= if (inImports searchWhere)								
			(search [path_modulename] [] [])	// recursive search through imported modules
			(case what of						// search the modules in given paths only 
				SearchIdentifier 		-> findAllModulesInPaths` "icl" searchPaths >>= \found -> search [path_modulename:found] [] []
				SearchImplementation 	-> findAllModulesInPaths` "icl" searchPaths >>= \found -> search [path_modulename:found] [] []
				SearchDefinition 		-> findAllModulesInPaths` "dcl" searchPaths >>= \found -> search [path_modulename:found] [] []
			) 
where
	search [] searched found 	= return (reverse found,reverse searched)								
	search [(path,modulename):rest] searched found 
		=					searchInFile what (inImports searchWhere) identifier (path,modulename)
		>>= \(new,pos) -> 	let (addedImports,nsearched,nfound) = calc new pos rest searched found in
							if (isEmpty addedImports)
								(search rest nsearched nfound) 
								(			  searchFilesInPaths addedImports searchPaths
								>>= \more ->  search (rest ++ more) nsearched nfound
								)
	where
		calc new pos rest searched found
		# nfound 		= ifPosNil pos found [((path,modulename),pos):found]
		# nsearched 	= [modulename:searched]
		# filesInRest 	= map snd rest
		# addedImports	= removeDup [fileName \\ fileName <- new | not (isMember fileName (filesInRest ++ nsearched))]
		= (addedImports,nsearched,nfound)
		
inImports SearchInImports = True
inImports _ 			  = False

ifPosNil PosNil then else = then
ifPosNil _      then else = else

searchInFile :: !SearchWhat !Bool !Identifier !(!DirPathName,!FileName) -> Task !(![String],!IdentifierPositionList)
searchInFile SearchIdentifier inImports identifier (path, moduleName) 
	= 					accWorld (accFiles (FindIdentifiersInFile inImports [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) ->  return (map (\f -> f +++ ".icl") (/*init */(StrictListToList list)),pos)
searchInFile SearchImplementation inImports identifier (path, moduleName) 
	= 					accWorld (accFiles (FindDefinitionInFile inImports [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) ->  return (map (\f -> f +++ ".icl") (/*init */(StrictListToList list)),pos)
searchInFile SearchDefinition inImports identifier (path, moduleName) 
	= 					accWorld (accFiles (FindDefinitionInFile inImports [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) ->  return (map (\f -> f +++ ".dcl") (/*init */(StrictListToList list)),pos)

searchIdentifiersInIclFile :: !Identifier !DirPathName !FileName  -> Task !(![String],!IdentifierPositionList)
searchIdentifiersInIclFile identifier path moduleName 
	= 					accWorld (accFiles (FindIdentifiersInFile True [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) ->  return (map (\f -> f +++ ".icl") (/*init */(StrictListToList list)),pos)

searchFilesInPaths :: ![FileName] !(List !DirPathName) -> Task ![(!DirPathName,!FileName)]
searchFilesInPaths fileNames pathNames = search fileNames pathNames []
where
	search [] _ found = return (reverse found)
	search [fileName:fileNames] pathNames found
		=				searchFileInPaths fileName pathNames
		>>= \res ->		if (isNothing res)
							(search fileNames pathNames found)
							(search fileNames pathNames [(fromJust res,fileName):found])

searchFileInPaths :: !FileName !(List !DirPathName) -> Task !(Maybe !DirPathName)
searchFileInPaths fileName paths = accWorld (searchDisk` paths)
where
	searchDisk` [!!] world = (Nothing,world)
	searchDisk` [!path:paths!] world
	# (content,world) = readDirectory path world
	= case content of
		Ok names 	-> if (isMember fileName names) 
							((Just path),world)
							(searchDisk` paths world)
		_ 			-> searchDisk` paths world
		
		
*/






