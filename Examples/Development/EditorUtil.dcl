definition module EditorUtil

import iTasks

// Synonyms to make clearer what kind of string is expected 

import IDE_State

currentDirectory 	:: Task !DirPathName
selectFileInPath 	:: !DirPathName !(!FileName -> Bool) -> Task !(DirPathName,Maybe !FileName)
storeFileInPath  	:: !DirPathName !FileName !String -> Task !Bool

searchFilesInPaths	:: ![FileName] ![DirPathName] -> Task ![(!DirPathName,!FileName)]
searchFileInPaths  	:: !FileName   ![DirPathName] -> Task !(Maybe !DirPathName)

showError 			:: String a -> Task a | iTask a


