implementation module iTasks.API.Extensions.Development.Codebase
import iTasks

derive class iTask SourceTree

sourceTreeFromFiles :: FilePath -> Task SourceTree
sourceTreeFromFiles baseDir = return {SourceTree|baseDir = baseDir, modules = []}

navigateCodebase :: CodeBase -> Task (FilePath,ModuleName)
navigateCodebase codebase
    = enterChoice "Select a module" []
        (flatten [[(baseDir,module) \\ module <- modules] \\ {SourceTree|baseDir,modules} <- codebase])
