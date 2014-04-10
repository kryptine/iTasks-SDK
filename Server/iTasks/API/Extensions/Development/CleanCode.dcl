definition module iTasks.API.Extensions.Development.CleanCode
import iTasks


:: FileExtension :== !String

/**
* Given a list of absolute path names, offers a tree structure choice to select a file 
* @param  absolute paths of directories to search through
* @param  only show files with given extensions, all files are shown if this list is empty
* @return selected file and the absolute path directory name it is found 
*/


chooseFile :: [FilePath] [FileExtension] -> Task (FilePath,FilePath)


viewCleanModule :: FilePath String -> Task ()
