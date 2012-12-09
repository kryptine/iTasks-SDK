definition module projectManager

import iTasks
import EditorUtil

// from the original windows Clean IDE, slightly modified
import PmTypes, PmProject, PmParse, PmEnvironment

derive class iTask	Project, LinkOptions, ApplicationOptions, CompilerOptions, ModInfo, ABCLinkInfo
derive class iTask	ModEditOptions, EditWdOptions, EditOptions, OptionalWindowPosAndSize, WindowPos_and_Size, NewlineConvention  
derive class iTask	[!!], InfListItem, ListTypes, Output, LinkMethod, ProjectDynamicInfo, StaticLibInfo, CodeGenOptions 
derive class iTask	UndefModule, UndefSymbol 
derive class iTask	Target, CompileMethod, Processor

// project options, iTask version

:: RunTimeOptions	= 	{ initialHeapSize			:: !Int
						, heapSizeIncreaseStep		:: !Int
						, maximumHeapSize 			:: !Int			
						, stackSize					:: !Int			
						}
:: DiagnosticsOptions =	{ showExecutionTime			:: !Bool		
						, showGarbageCollections	:: !Bool		
						, printStackSize			:: !Bool		
						, writeStderrToFile			:: !Bool
						, checkStacks				:: !Bool	
						, checkIndices				:: !Bool		
						}
:: ProfilingOptions =	{ timeProfile				:: !TimeProfileOptions
						, heapProfile				:: !HeapProfileOptions
						}
:: TimeProfileOptions 	= NoTimeProfiling 
						| TimeProfileAndStackTrace 
						| StackTraceOny
:: HeapProfileOptions 	= NoHeapProfiling 
						| HeapProfile !HeapProfile 
:: HeapProfile		 	= { minimumHeapProfile 		:: !Int
						}
:: ConsoleOptions		:== Output

// environments, iTask version

:: Environment		=	{ environmentName			:: !String
						, paths						:: ![String]
						, toolsOption				:: !ToolsOptions
						}						
:: ToolsOptions		= 	{ compiler					:: !String
						, codeGenerator				:: !String
						, staticLinker				:: !String
						, dynamicLinker				:: !String
						, versionOfAbcCode			:: !Int
						, runOn64BitProcessor		:: !Bool
						}

derive class iTask 	RunTimeOptions, DiagnosticsOptions, ProfilingOptions, TimeProfileOptions, HeapProfileOptions, HeapProfile
derive class iTask  Environment, ToolsOptions

// accessing project files (of type Project):

initProject 	:: !ModuleName 						-> Project						

readProjectFile	:: !ProjectPath !CleanPath 			-> Task (Project, Bool, String)
saveProjectFile :: !Project !ProjectPath !CleanPath -> Task Bool

toProject 		:: Project (RunTimeOptions, DiagnosticsOptions, ProfilingOptions, ConsoleOptions) -> Project
fromProject 	:: Project -> (RunTimeOptions, DiagnosticsOptions, ProfilingOptions, ConsoleOptions)

// accessing environment files (of type [Target]):

initTarget 			:: Target
readEnvironmentFile :: !FilePathName 			-> Task ![Target]
saveEnvironmentFile :: !FilePathName ![Target] 	-> Task !Bool

toTarget 			:: !Environment 	-> Target
toTargets 			:: ![Environment] 	-> [Target]
fromTarget 			:: !Target 			-> Environment
fromTargets 		:: ![Target] 		-> [Environment]

// searching

derive class iTask 	IdentifierPositionList

findAllModulesInPaths	 :: !Extension !DirPathName !(List !DirPathName) -> Task ![(!DirPathName,![Module])]
findAllModulesInProject	 :: !DirPathName !(!DirPathName,!ModuleName) ![(!DirPathName,![Module])] -> Task ![(!DirPathName,![Module])] 
			   
findDefinition	:: !Identifier !FileName -> Task !IdentifierPositionList


