definition module IDE_State

/* Global Shared State of the IDE
*/

// Attention:
// Currently it is assumed the you have a copy of BatchBuild.exe and a copy of the IDEEnvs file (from Config) in the original Clean IDE Application directory
// cleanpath pointing to batchBuild.exe currently has to set by hand !!!

//cleanPath 		:== "C:\\Users\\bas\\Desktop\\Clean\\" 
cleanPath 		:== "C:\\Users\\marinu\\Desktop\\Clean_2.2\\"
//cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"

batchBuild			:== "BatchBuild.exe"
errorFile			:== "Temp\\errors"
initialPath 		:== cleanPath +++ "iTasks-SDK\\Examples\\Development\\"
IDE_State_fileName 	:== "IDE_State"

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

