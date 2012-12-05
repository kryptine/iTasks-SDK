definition module EditorUtil

import iTasks

// Synonyms to make clearer what kind of string is expected 

:: Identifier		:== String		// Clean Identifier
:: DirPathName		:== String		// Path name leading to a directory
:: ProjectPath 		:== DirPathName	// Directory where project is located
:: CleanPath		:== DirPathName	// Directory where clean application / batchbuild is located
:: ModuleName 		:== String		// Name of module, without .dcl or .dcl extension
:: FileName			:== String		// Name of file, with extension
:: FilePathName		:== String		// Full path name of file, with extension

currentDirectory 	:: Task !DirPathName
selectFileInPath 	:: !DirPathName !(!FileName -> Bool) -> Task !(DirPathName,Maybe !FileName)
storeFileInPath  	:: !DirPathName !FileName !String -> Task !Bool

searchFilesInPaths	:: ![FileName] ![DirPathName] -> Task ![(!DirPathName,!FileName)]
searchFileInPaths  	:: !FileName   ![DirPathName] -> Task !(Maybe !DirPathName)


showError 			:: String a -> Task a | iTask a


