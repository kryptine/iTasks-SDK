definition module iTasks.API.Extensions.Development.Codebase

import iTasks
import System.FilePath

:: CodeBase 		:== [SourceTree]
:: SourceTree 		:== (FilePath,[TreeNode FilePath])	// absolute path, tree of code 
:: ModuleName   	:== String
:: Extension		= 	Icl | Dcl

derive class iTask Extension

//Scan filepaths and find all the modules on disk
codeBaseFromFiles :: [FilePath] -> Task CodeBase

//Browse the modules in a code base and select a Clean module
navigateCodebase :: CodeBase -> Task (FilePath,ModuleName)

//Editor for Clean Source Code
editCleanModule :: (FilePath,ModuleName) Extension -> Task ()
