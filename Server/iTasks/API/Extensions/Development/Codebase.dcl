definition module iTasks.API.Extensions.Development.Codebase

import iTasks
import System.FilePath
import iTasks.API.Extensions.CodeMirror

:: CodeBase 		:== [SourceTree]

:: SourceTree =
    { rootDir       :: DirPathName
    , modules       :: [(ModuleName,ModuleType)]
    , moduleFiles   :: [TreeNode FileName]
    }

:: DirPathName		:== String								// Path name leading to a directory
:: FileName			:== String								// Name of file, with extension
:: ModuleName 		:== String								// Name of module, without extension
:: ModuleType       =   MainModule | AuxModule              // main module: (only icl) auxilary module (icl + dcl)
:: Extension		= 	Icl | Dcl

:: CleanFile        :== (FilePath,ModuleName,Extension)     // A reference to a clean file on disk (either an icl or dcl)

:: CleanModuleName	:== (FilePath,ModuleName)				// Clean Module Name
:: CleanModule		:== (CleanModuleName,Extension)			// Either a definition or implementation module

:: CleanPath		:== DirPathName		// Directory where clean application / batchbuild is located
:: Environment		:== [DirPathName]	// Directories where code is stored
:: Identifier		:== String								// Clean identifier 

derive class iTask SourceTree, ModuleType, Extension
instance toString Extension
instance == Extension

// Scan directory environment and find all the modules on disk
codeBaseFromEnvironment :: Environment -> Task CodeBase

// Browse the modules in a code base and select a Clean module
//navigateCodebase :: CodeBase -> Task CleanModuleName
navigateCodebase :: CodeBase -> Task (FilePath,ModuleName,ModuleType)

//List all clean files in a codebase
listFilesInCodeBase :: CodeBase -> [CleanFile]

//Convert a CleanFile triple to an absolute file path
cleanFilePath :: CleanFile -> FilePath

// Convert CodeBase Tree to a list
codeBaseToCleanModuleNames :: CodeBase -> [CleanModuleName]

//Editor for Clean Source Code
editCleanModule     :: Bool CleanModule -> Task CodeMirror

initCleanEditor     :: Bool String -> CodeMirror

updateCleanEditor 	:: (Shared CodeMirror) FilePath ModuleName Extension -> Task CodeMirror
viewCleanEditor 	:: (Shared CodeMirror) CleanModule -> Task CodeMirror

