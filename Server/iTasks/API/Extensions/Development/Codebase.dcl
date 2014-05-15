definition module iTasks.API.Extensions.Development.Codebase

import iTasks
import System.FilePath
import iTasks.API.Extensions.CodeMirror

:: CodeBase 		:== [SourceTree]

:: SourceTree
    = LibraryTree       LibraryTree
    | ApplicationTree   ApplicationTree

:: LibraryTree =
    { rootDir       :: DirPathName
    , moduleFiles   :: [TreeNode FileName]
    }
:: ApplicationTree =
    { rootDir       :: DirPathName
    , moduleFiles   :: [TreeNode FileName]
    , mainModule    :: FileName
    }

:: DirPathName		:== String								// Path name leading to a directory
:: FileName			:== String								// Name of file, with extension
:: ModuleName 		:== String								// Name of module, without extension
:: Extension		= 	Icl | Dcl

:: CleanModuleName	:== (FilePath,ModuleName)				// Clean Module Name
:: CleanModule		:== (CleanModuleName,Extension)			// Either a definition or implementation module

:: CleanPath		:== DirPathName		// Directory where clean application / batchbuild is located
:: Environment		:== [DirPathName]	// Directories where code is stored
:: Identifier		:== String								// Clean identifier 

derive class iTask SourceTree, LibraryTree, ApplicationTree, Extension
instance toString Extension
instance == Extension

// Scan directory environment and find all the modules on disk
codeBaseFromEnvironment :: Environment -> Task CodeBase

// Browse the modules in a code base and select a Clean module
navigateCodebase :: CodeBase -> Task CleanModuleName

// Convert CodeBase Tree to a list
codeBaseToCleanModuleNames :: CodeBase -> [CleanModuleName]

//Editor for Clean Source Code
editCleanModule :: Bool CleanModule -> Task CodeMirror

initCleanEditor     :: Bool String -> CodeMirror

updateCleanEditor 	:: (Shared CodeMirror) CleanModule -> Task CodeMirror
viewCleanEditor 	:: (Shared CodeMirror) CleanModule -> Task CodeMirror

