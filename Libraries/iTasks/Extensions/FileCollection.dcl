definition module iTasks.Extensions.FileCollection
/**
* This extension provides a set of SDS functions to map
* somewhat complex data structures to a directory tree structure with text files on disk.
*/
import iTasks
from Data.Map import :: Map
from System.FilePath import :: FilePath

//Determine if a path is part of the collection based on the relative path
:: FileFilter :== FilePath -> FileFilterDecision
:: FileFilterDecision
	= IncludeFile   //The file is part of the managed collection
	| ExcludeFile   //The file is not part of the collection, do not touch it
	| ReferenceFile //The file is part of the collection, but don't read or write its content

:: FileCollection :== Map String FileCollectionItem
:: FileCollectionItem
	= FileContent String 
	| FileReference
	| FileCollection FileCollection

derive class iTask FileCollectionItem

/**
* Writes a map of key/value pairs to a directory with one file per key/value
* It will ignore all files in the directory that don't match the filter

* @param The filter that specifies which files and directories are part of the collection
# @param Readonly flag: When this is true, the files are only read, never written
* @param Delete flag: When this is true, files on disk that are not in the collection, but match the filter are deleted during a write.
                      If it is false, entries on that are removed are only marked in a file called 'exclude.txt' but not deleted.
*/
fileCollection :: FileFilter Bool Bool -> SDSSource FilePath FileCollection FileCollection

/**
* Test the path against a list of 'glob' rules. Return the decision for the first rule that matches. 
* If none of the rules match, the default decision is returned.
*/
matchRules :: [(String,FileFilterDecision)] FileFilterDecision -> FileFilter

//Filter to ignore all hidden files (e.g. starting with a '.')
ignoreHiddenFiles :: FileFilter

//Access utilities:
getStringContent:: String FileCollection -> Maybe String
setStringContent:: String String FileCollection -> FileCollection

getIntContent :: String FileCollection -> Maybe Int
setIntContent :: String Int FileCollection -> FileCollection

toPaths :: FileCollection -> [FilePath]
