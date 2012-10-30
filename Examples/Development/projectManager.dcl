definition module projectManager

import iTasks

// from the original windows Clean IDE, slightly modified
import PmTypes, PmProject, PmParse

derive class iTask	Project, LinkOptions, ApplicationOptions, CompilerOptions, ModInfo, ABCLinkInfo
derive class iTask	ModEditOptions, EditWdOptions, EditOptions, OptionalWindowPosAndSize, WindowPos_and_Size, NewlineConvention  
derive class iTask	[!!], InfListItem, ListTypes, Output, LinkMethod, ProjectDynamicInfo, StaticLibInfo, CodeGenOptions 
derive class iTask	UndefModule, UndefSymbol 

// Synonyms to make clearer what kind of string is expected 

:: Identifier	:== String		// Clean Identifier
:: PathName		:== String		// Path name leading to a directory
:: ProjectPath 	:== PathName	// Directory where project is located
:: CleanPath	:== PathName	// Directory where clean application / batchbuild is located
:: ModuleName 	:== String		// Name of module, without .dcl or .dcl extension
:: FileName		:== String		// Name of file, with extension

// accessing project files:

initProject 	:: !ModuleName 						-> Project						
readProjectFile	:: !ProjectPath !CleanPath 			-> Task (Project, Bool, String)
saveProjectFile :: !ProjectPath !CleanPath !Project -> Task Bool

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

// conversion 

toProject 	:: Project (RunTimeOptions, DiagnosticsOptions, ProfilingOptions, ConsoleOptions) -> Project
fromProject :: Project -> (RunTimeOptions, DiagnosticsOptions, ProfilingOptions, ConsoleOptions)

// searching

derive class iTask 	IdentifierPositionList

:: SearchOptions 	:== (SearchWhat, SearchWhere)
:: SearchWhat 		= 	SearchIdentifier | SearchDefinition | SearchImplementation
:: SearchWhere		=	SearchInImports | SearchInPaths //| SearchInProject

searchTask :: !SearchWhat !SearchWhere !Identifier !(!PathName,!FileName) ![PathName] -> Task (![(!(!PathName,!FileName),!IdentifierPositionList)],![FileName])

searchIdentifierInImports :: !Identifier !(!PathName,!FileName) ![PathName] -> Task (![(!(!PathName,!FileName),!IdentifierPositionList)],![FileName])

findAllModulesInPaths :: !String ![PathName] -> Task ![(!PathName,!FileName)]


			   


