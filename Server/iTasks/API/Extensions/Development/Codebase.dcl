definition module iTasks.API.Extensions.Development.Codebase

import iTasks
import System.FilePath

//A code base is simpky a list of source trees
:: CodeBase :== [SourceTree]
//A source tree is a collection of modules organized using into a hierarchy
:: SourceTree =
    { baseDir   :: FilePath         //A directory in the filesystem that holds the source tree
    , modules   :: [ModuleName]
    }

:: ModuleName   :== String

derive class iTask SourceTree

//Scan a filepath and find all the modules on disk
sourceTreeFromFiles :: FilePath -> Task SourceTree

//Browse the modules in a code base and select a module
navigateCodebase :: CodeBase -> Task (FilePath,ModuleName)
