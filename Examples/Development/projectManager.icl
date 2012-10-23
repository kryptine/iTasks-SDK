implementation module projectManager

import iTasks
import PmTypes, PmProject
						   
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

	
	
