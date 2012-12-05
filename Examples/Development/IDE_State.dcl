definition module IDE_State

/* Global Shared State of the IDE
*/

// Attention:
// Currently it is assumed the you have a copy of BatchBuild.exe and a copy of the IDEEnvs file (from Config) in the original Clean IDE Application directory
// cleanpath pointing to batchBuild.exe currently has to set by hand !!!

//cleanPath 		:== "C:\\Users\\bas\\Desktop\\Clean\\" 
cleanPath 			:== "C:\\Users\\marinu\\Desktop\\Clean_2.2\\"
//cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"

batchBuild			:== cleanPath +++ "BatchBuild.exe"
errorFile			:== cleanPath +++ "Temp\\errors"
initialPath 		:== cleanPath +++ idePath
idePath				:== "iTasks-SDK\\Examples\\Development\\"
IDE_State_fileName 	:== "IDE_State"

:: Identifier		:== String		// Clean Identifier
:: DirPathName		:== String		// Path name leading to a directory, should end with \\
:: ProjectPath 		:== DirPathName	// Directory where project is located
:: CleanPath		:== DirPathName	// Directory where clean application / batchbuild is located
:: ModuleName 		:== String		// Name of module, without .dcl or .dcl extension
:: FileName			:== String		// Name of file, with extension
:: FilePathName		:== String		// Full path name of file, with extension

import PmEnvironment, PmProject							// uses some modules from the original Clean IDE 
import EditorUtil 

:: IDE_State =	{ projectName		:: !String			// name of the project, empty string indicates no project set
				, projectPath		:: !String			// path where project is located
				, projectSettings	:: !Project			// settings will passed to call of BatchBuild, the batch version of the compiler 
				, cleanPath			:: !String			// path whare Clean compiler and BatchBuild is located
				, openedFiles		:: ![String]		// files currently opened in IDE
				, recentFiles		:: ![String]		// recently opened files
				, recentProjects	:: ![String]		// recently opened projects
				, idx				:: !Int				// index in target
				, envTargets		:: ![Target]		// targets are environments
				}

import iTasks
derive class iTask IDE_State

// access functions to global state

init_IDE_State 		:: IDE_State			

get_IDE_State 		:: Task IDE_State
update_IDE_State 	:: !(IDE_State -> IDE_State) -> Task !Void

watch_IDE_State 	:: !(IDE_State -> Bool) !(Task a) -> Task a | iTask a

set_new_Project 	:: !ModuleName !ProjectPath 			-> Task Void
open_Project 		:: !ModuleName !ProjectPath !Project 	-> Task Void
	
update_Project 		:: !Project -> Task Void

setEnvironments 	:: ![Target] -> Task Void
add_Environments 	:: ![Target] -> Task Void
updateEnvironment 	:: !Int !Target -> Task Void
select_Environment	:: !Int -> Task Void
addFilesAdmin 		:: !FileName -> Task Void
removeFileAdmin 	:: !FileName -> Task Void

