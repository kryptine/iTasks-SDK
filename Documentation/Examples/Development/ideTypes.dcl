definition module ideTypes

import iTasks
import PmTypes, PmProject, PmParse, PmEnvironment

:: IDE_State =	{ projectName		:: !String			// name of the project, empty string indicates no project set
				, projectPath		:: !String			// path where project is located
				, project			:: !Project			// settings will passed to call of BatchBuild, the batch version of the compiler 
				, cleanPath			:: !String			// path whare Clean compiler and BatchBuild is located
				, openedFiles		:: ![String]		// files currently opened in IDE
				, recentFiles		:: ![String]		// recently opened files
				, recentProjects	:: ![String]		// recently opened projects
				, idx				:: !Int				// index in target
				, envTargets		:: ![Target]		// targets are environments
				}

derive class iTask IDE_State

// Synonyms to make clearer what kind of string is being used 

:: Identifier	:== String		// Clean Identifier
:: PathName		:== String		// Path name leading to a directory
:: ProjectPath 	:== PathName	// Directory where project is located
:: CleanPath	:== PathName	// Directory where clean application / batchbuild is located
:: ModuleName 	:== String		// Name of module, without .dcl or .dcl extension
:: FileName		:== String		// Name of file, with extension
:: FullFileName	:== String		// Full pathname of file, with extension

// shared global store:

IDE_Store :: Shared IDE_State

//cleanPath 		:== "C:\\Users\\bas\\Desktop\\Clean\\" 
//cleanPath 		:== "C:\\Users\\marinu\\Desktop\\Clean_2.2\\"
cleanPath 		:== "C:\\Users\\rinus\\Work\\Clean_2.2\\"

idePath			:== "iTasks-SDK\\Examples\\Development\\"
projectPath		:== cleanPath +++ idePath
compilerPath	:== cleanPath +++ "iTasks-SDK\\Compiler\\"

batchBuild		:== "BatchBuild.exe"
errorFile		:== "Temp\\errors"
environmentFile :== "iTask-Clean-IDE-environment"
stdenv			:== cleanPath +++ "Libraries\\StdEnv\\"


