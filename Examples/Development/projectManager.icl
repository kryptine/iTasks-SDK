implementation module projectManager

import iTasks
import PmTypes, PmProject

initRunTimeOptions	= 	{ initialHeapSize			= 204800
						, heapSizeIncreaseStep		= 4096
						, maximumHeapSize 			= 409600			
						, stackSize					= 102400			
						}
initDiagnosticsOptions = { showExecutionTime		= False		
						, showGarbageCollections	= False		
						, printStackSize			= False		
						, writeStderrToFile			= False
						, checkStacks				= False	
						, checkIndices				= False		
						}
initProfilingOptions =	{ timeProfile				= NoTimeProfiling
						, heapProfile				= NoHeapProfiling
						}
								   
derive class iTask 	RunTimeOptions, DiagnosticsOptions, ProfilingOptions, TimeProfileOptions, HeapProfileOptions, HeapProfile
derive class iTask	Project, LinkOptions, ApplicationOptions, CompilerOptions, ModInfo, ABCLinkInfo
derive class iTask	ModEditOptions, EditWdOptions, EditOptions, OptionalWindowPosAndSize, WindowPos_and_Size, NewlineConvention  
derive class iTask	[!!], InfListItem, ListTypes, Output, LinkMethod, ProjectDynamicInfo, StaticLibInfo, CodeGenOptions 
derive class iTask	UndefModule, UndefSymbol 

setOptions :: (RunTimeOptions, DiagnosticsOptions, ProfilingOptions) -> Task (RunTimeOptions, DiagnosticsOptions, ProfilingOptions)
setOptions (rto,diagn,prof)
	= setOptions` <<@ Window
where
	setOptions`
		=				runTime rto
		>>= \rto ->		diagnostics diagn
		>>= \diagn -> 	profiling prof
		>>= \prof ->	return (rto,diagn,prof) <<@ Window 

	runTime :: RunTimeOptions -> Task RunTimeOptions
	runTime	rto			= updateInformation ("Project Options","Run-Time Options:")[] rto 
	
	diagnostics :: DiagnosticsOptions -> Task DiagnosticsOptions
	diagnostics	diagn	= updateInformation ("Project Options","Diagnostics Options:") [] diagn
	
	profiling :: ProfilingOptions -> Task ProfilingOptions
	profiling prof		= updateInformation ("Project Options","Diagnostics Options:") [] prof	

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
	editWdOptions 		= 	{	eo 			= { newlines = NewlineConventionNone}
							,	pos_size	= NoWindowPosAndSize 
							}
	compilerOptions		= DefaultCompilerOptions
	codeGenOptions		= DefCodeGenOptions
	applicationOptions	= DefApplicationOptions
	list				= [!!]
	linkOptions			= DefaultLinkOptions

/*

ReadProjectFile	:: !String !String !*Files -> (!(!Project, !Bool, !{#Char}),!*Files)

SaveProjectFile	:: !String !Project !String !*Files -> (!Bool, !*Files);


PR_InitProject	:: Project


PR_NewProject	:: !String !EditWdOptions !CompilerOptions !CodeGenOptions !ApplicationOptions
					!(List String) !LinkOptions -> Project

*/	
	
	
