implementation module projectManager

import iTasks
import PmTypes, PmProject, PmParse, UtilStrictLists
						   
derive class iTask 	RunTimeOptions, DiagnosticsOptions, ProfilingOptions, TimeProfileOptions, HeapProfileOptions, HeapProfile
derive class iTask	Project, LinkOptions, ApplicationOptions, CompilerOptions, ModInfo, ABCLinkInfo
derive class iTask	ModEditOptions, EditWdOptions, EditOptions, OptionalWindowPosAndSize, WindowPos_and_Size, NewlineConvention  
derive class iTask	[!!], InfListItem, ListTypes, Output, LinkMethod, ProjectDynamicInfo, StaticLibInfo, CodeGenOptions 
derive class iTask	UndefModule, UndefSymbol 

toProject 	:: Project (RunTimeOptions, DiagnosticsOptions, ProfilingOptions) -> Project
toProject p=:{applicationopt,codegenopt} (rto,do,po)
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
	  }

fromProject :: Project -> (RunTimeOptions, DiagnosticsOptions, ProfilingOptions)
fromProject p=:{applicationopt,codegenopt} = (rto,do,po)
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

readProjectFile	:: !ProjectPath !CleanPath -> Task (Project, Bool, String)
readProjectFile projectPath cleanAppDir 
	= accWorld (accFiles (ReadProjectFile projectPath cleanAppDir))

saveProjectFile :: !ProjectPath !CleanPath !Project -> Task Bool
saveProjectFile projectPath cleanAppDir project 
	= accWorld (accFiles (SaveProjectFile projectPath project cleanAppDir))

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
	
derive class iTask IdentifierPositionList

import Directory

searchInImports :: !SearchWhat !Identifier !(!PathName,!FileName) ![PathName] -> Task (![(!(!PathName,!FileName),!IdentifierPositionList)],![FileName])
searchInImports what identifier path_modulename environment = search [path_modulename] [] []
where
	search [] searched found 	= return (reverse found,reverse searched)								
	search [(path,modulename):rest] searched found 
		=					searchInFile what identifier (path,modulename)
//		=					searchIdentifiersInIclFile2 identifier path modulename 
		>>= \(new,pos) -> 	let (addedImports,nsearched,nfound) = calc new pos rest searched found in
							if (isEmpty addedImports)
								(search rest nsearched nfound) 
								(			  searchFilesInPaths addedImports environment
								>>= \more ->  search (rest ++ more) nsearched nfound
								)
	where
		calc new pos rest searched found
		# nfound 		= ifPosNil pos found [((path,modulename),pos):found]
		# nsearched 	= [modulename:searched]
		# filesInRest 	= map snd rest
		# addedImports	= removeDup [fileName \\ fileName <- new | not (isMember fileName (filesInRest ++ nsearched))]
		= (addedImports,nsearched,nfound)
		
ifPosNil PosNil then else = then
ifPosNil _ then else = else

searchInFile :: !SearchWhat !Identifier !(!PathName,!FileName) -> Task !(![String],!IdentifierPositionList)
searchInFile SearchIdentifier identifier (path, moduleName) 
	= 					accWorld (accFiles (FindIdentifiersInFile True [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) ->  return (map (\f -> f +++ ".icl") (/*init */(StrictListToList list)),pos)
searchInFile SearchImplementation identifier (path, moduleName) 
	= 					accWorld (accFiles (FindDefinitionInFile True [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) ->  return (map (\f -> f +++ ".icl") (/*init */(StrictListToList list)),pos)
searchInFile SearchDefinition identifier (path, moduleName) 
	= 					accWorld (accFiles (FindDefinitionInFile True [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) ->  return (map (\f -> f +++ ".dcl") (/*init */(StrictListToList list)),pos)


searchIdentifiersInIclFile2 :: !Identifier !PathName !FileName  -> Task !(![String],!IdentifierPositionList)
searchIdentifiersInIclFile2 identifier path moduleName 
	= 					accWorld (accFiles (FindIdentifiersInFile True [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) ->  return (map (\f -> f +++ ".icl") (/*init */(StrictListToList list)),pos)


searchFilesInPaths :: ![FileName] ![PathName] -> Task ![(!PathName,!FileName)]
searchFilesInPaths fileNames pathNames = search fileNames pathNames []
where
	search [] _ found = return (reverse found)
	search [fileName:fileNames] pathNames found
		=				searchFileInPaths fileName pathNames
		>>= \res ->		if (isNothing res)
							(search fileNames pathNames found)
							(search fileNames pathNames [(fromJust res,fileName):found])

searchFileInPaths :: !FileName ![PathName] -> Task !(Maybe !PathName)
searchFileInPaths fileName paths = accWorld (searchDisk` paths)
where
	searchDisk` [] world = (Nothing,world)
	searchDisk` [path:paths] world
	# (content,world) = readDirectory path world
	= case content of
		Ok names 	-> if (isMember fileName names) 
							((Just path),world)
							(searchDisk` paths world)
		_ 			-> searchDisk` paths world

// there seems to be a bug when it returns  Task (![String],!IdentifierPositionList)

searchIdentifierInImports :: !Identifier !(!PathName,!FileName) ![PathName] -> Task (![(!(!PathName,!FileName),!IdentifierPositionList)],![FileName])
searchIdentifierInImports identifier path_modulename environment = search [path_modulename] [] []
where
	search [] searched found 	= return (found,searched)								
	search [(path,modulename):rest] searched found 
		=					searchIdentifiersInIclFile2 identifier path modulename
		>>= \(new,pos) -> 	let (addedImports,nsearched,nfound) = calc new pos rest searched found in
							if (isEmpty addedImports)
								(search rest nsearched nfound) 
								(			  searchFilesInPaths addedImports environment
								>>= \more ->  search (rest ++ more) nsearched nfound
								)
	where
		calc new pos rest searched found
		# nfound 		= ifPosNil pos found [((path,modulename),pos):found]
		# nsearched 	= [modulename:searched]
		# filesInRest 	= map snd rest
		# addedImports	= removeDup [fileName \\ fileName <- new | not (isMember fileName (filesInRest ++ nsearched))]
		= (addedImports,nsearched,nfound)


/*
searchIdentifiersInIclFile :: !Identifier !PathName !FileName  -> Task (!IdentifierPositionList)
searchIdentifiersInIclFile identifier path moduleName 
	= 			accWorld (accFiles (FindIdentifiersInFile True [!path +++ moduleName!] identifier (path +++ moduleName) ))
	>>= \(list,pos) -> return pos
*/









