definition module IDE_State

/* Global Shared State of the IDE
*/

// Attention:
// Currently it is assumed the you have a copy of BatchBuild.exe and a copy of the IDEEnvs file (from Config) in the original Clean IDE Application directory
// cleanpath pointing to batchBuild.exe currently has to set by hand !!!

//cleanPath 		:== "C:\\Users\\bas\\Desktop\\Clean\\" 
//cleanPath 			:== "C:\\Users\\marinu\\Desktop\\Clean_2.2\\"
cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"

idePath				:== "iTasks-SDK\\Examples\\Development\\"
batchBuild			:== cleanPath +++ "BatchBuild.exe"
errorFile			:== cleanPath +++ "Temp\\errors"
initialPath 		:== cleanPath +++ idePath
IDE_State_fileName 	:== "IDE_State"

// Here follows some type synonyms for Strings, used to make clear what exactly is wanted...
// Perhaps we should make a type for it...

:: Identifier		:== String		// Clean Identifier
:: DirPathName		:== String		// Path name leading to a directory, should end with \\
:: ProjectPath 		:== DirPathName	// Directory where project is located
:: CleanPath		:== DirPathName	// Directory where clean application / batchbuild is located
:: ModuleName 		:== String		// Name of module, without extension
:: Extension		:== String		// Extension, e.g. ".icl", ".dcl", ".txt", ".exe", ...
:: FileName			:== String		// Name of file, with extension
:: FilePathName		:== String		// Full path name of file, with extension

import PmEnvironment, PmProject							// uses some modules from the original Clean IDE 
import EditorUtil 

:: IDE_State =	{ projectName		:: !ModuleName						// name of the project, empty string indicates no project set
				, projectPath		:: !ProjectPath						// path where project is located
				, projectSettings	:: !Project							// settings will passed to call of BatchBuild, the batch version of the compiler 
				, cleanPath			:: !CleanPath						// path whare Clean compiler and BatchBuild is located
				, openedFiles		:: ![FilePathName]					// files currently opened in IDE
				, recentFiles		:: ![FilePathName]					// recently opened files
				, recentProjects	:: ![FilePathName]					// recently opened projects
				, idx				:: !Int								// index in target
				, envTargets		:: ![Target]						// targets are environments
				, allFilesInEnv		:: ![(!DirPathName,![Module])]		// all modules in chosen environment
				, moduleOptions		:: !ModuleOptions					// what to show in project pane
				}
:: Module = 	{ isUsed			:: !Bool							// is module used in project
				, moduleName		:: !ModuleName						// name of module 
				} 
:: ModuleOptions = InEnvironment | InProject | NotUsed
:: SearchOptions = SearchDefinition | SearchImplementation | SearchIdentifier

import iTasks
derive class iTask IDE_State, Module, ModuleOptions, SearchOptions 

// access functions to global state

init_IDE_State 		:: IDE_State			

get_IDE_State 		:: Task IDE_State
update_IDE_State 	:: !(IDE_State -> IDE_State) -> Task !Void

watch_IDE_State 	:: !(IDE_State -> Bool) !(Task a) -> Task a | iTask a

set_new_Project 	:: !ProjectPath !ModuleName				-> Task Void
open_Project 		:: !ProjectPath !ModuleName !Project	-> Task Void
	
update_Project 		:: !Project -> Task Void

setEnvironments 	:: ![Target] -> Task Void
add_Environments 	:: ![Target] -> Task Void
updateEnvironment 	:: !Int !Target -> Task Void
select_Environment	:: !Int -> Task Void

setAllFilesInEnv	:: ![(!DirPathName,![Module])] -> Task Void
setProjectPaneOption :: !ModuleOptions -> Task Void

addFileToAdmin 		:: !FileName -> Task Void
removeFileFromAdmin	:: !FileName -> Task Void

