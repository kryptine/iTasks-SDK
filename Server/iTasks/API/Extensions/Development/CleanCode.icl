implementation module iTasks.API.Extensions.Development.CleanCode
import iTasks

viewCleanModule :: FilePath String -> Task ()
viewCleanModule baseDir moduleName
    = viewInformation "TODO" [] (baseDir,moduleName) @! ()
