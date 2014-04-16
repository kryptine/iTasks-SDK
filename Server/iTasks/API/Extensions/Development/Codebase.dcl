definition module iTasks.API.Extensions.Development.Codebase

import iTasks
import System.FilePath
import iTasks.API.Extensions.CodeMirror

:: CodeBase 		:== [SourceTree]
:: SourceTree 		:== (DirPathName,[TreeNode FileName])	// absolute path, tree of code 
:: DirPathName		:== String								// Path name leading to a directory
:: FileName			:== String								// Name of file, with extension
:: ModuleName 		:== String								// Name of module, without extension
:: Extension		= 	Icl | Dcl



:: CleanModuleName	:== (FilePath,ModuleName)				// Clean Module Name
:: CleanModule		:== (CleanModuleName,Extension)			// Either a definition or implementation module


:: CleanPath		:== DirPathName		// Directory where clean application / batchbuild is located

// for the time being one has to set the Clean path being used by hand
cleanPath 			:== "C:\\Users\\rinus\\Desktop\\Clean_2.2"

:: Environment		:== [DirPathName]	// Directories where code is stored

// define your environment by hand for the time being

//myEnv 			:: Environment
myEnv				:== [idePath,itaskEnv,stdEnv]

idePath 			:==  cleanPath </> "iTasks-SDK\\Examples\\IDE"
stdEnv   			:==	 cleanPath </> "Libraries\\StdEnv"
itaskEnv			:==  cleanPath </> "iTasks-SDK\\Server"

:: Identifier		:== String								// Clean identifier 

derive class iTask Extension
instance toString Extension

// Scan directory environment and find all the modules on disk
codeBaseFromEnvironment :: Environment -> Task CodeBase

// Browse the modules in a code base and select a Clean module
navigateCodebase :: CodeBase -> Task CleanModuleName

// Convert CodeBase Tree to a list
codeBaseToCleanModuleNames :: CodeBase -> [CleanModuleName]

//Editor for Clean Source Code
editCleanModule :: CleanModule -> Task CodeMirror
