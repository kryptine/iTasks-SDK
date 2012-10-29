definition module projectManager

import iTasks

// from the windows Clean IDE
import PmTypes, PmProject, PmParse

derive class iTask	Project, LinkOptions, ApplicationOptions, CompilerOptions, ModInfo, ABCLinkInfo
derive class iTask	ModEditOptions, EditWdOptions, EditOptions, OptionalWindowPosAndSize, WindowPos_and_Size, NewlineConvention  
derive class iTask	[!!], InfListItem, ListTypes, Output, LinkMethod, ProjectDynamicInfo, StaticLibInfo, CodeGenOptions 
derive class iTask	UndefModule, UndefSymbol 

// accessing project files:

:: Identifier	:== String		// Clean Identifier
:: PathName		:== String		// Path name leading to a directory
:: ProjectPath 	:== PathName	// Directory where project is located
:: CleanPath	:== PathName	// Directory where clean application / batchbuild is located
:: ModuleName 	:== String		// Name of module, without .dcl or .dcl extension
:: FileName		:== String		// Name of file, with extension

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
:: HeapProfile		  = { minimumHeapProfile 		:: !Int
						}
derive class iTask 	RunTimeOptions, DiagnosticsOptions, ProfilingOptions, TimeProfileOptions, HeapProfileOptions, HeapProfile

// conversion 

toProject 	:: Project (RunTimeOptions, DiagnosticsOptions, ProfilingOptions) -> Project
fromProject :: Project -> (RunTimeOptions, DiagnosticsOptions, ProfilingOptions)

// searching

derive class iTask 	IdentifierPositionList

searchIdentifierInImports :: !Identifier !(!PathName,!FileName) ![PathName] -> Task (![(!(!PathName,!FileName),!IdentifierPositionList)],![FileName])
searchFilesInPaths :: ![FileName] ![PathName] -> Task ![(!PathName,!FileName)]
searchIdentifiersInIclFile2 :: !Identifier !PathName !FileName  -> Task !(![String],!IdentifierPositionList)
searchIdentifiersInIclFile 	:: !Identifier !PathName !FileName  -> Task !IdentifierPositionList
//searchIdentifierInImports 	:: !Identifier !PathName !FileName ![PathName] -> Task ![(!PathName,!FileName,!IdentifierPositionList)]


			   


