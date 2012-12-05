implementation module EditorUtil

import iTasks
import File, StdFile
import Directory

derive class iTask MaybeError, FileInfo, Tm, FileError

// iTask utilities of general nature

// file selector...

selectFileInPath :: !DirPathName !(!FileName -> Bool) -> Task !(DirPathName,Maybe !FileName)
selectFileInPath path pred
	= 				accWorld (readDirectory path)
	>>= \content -> case content of
						Ok names 	-> select names
						_ 			-> return (path,Nothing)
where
	select names
		=				enterChoice ("File Selector",path) [] (filter pred names) <<@ Window
		>>*				[ OnAction ActionCancel always   (const (return (path,Nothing)))
						, OnAction ActionOk		hasValue (continue o getValue)
						, OnAction ActionNew	always 	 (const (newFile))
						]
	newFile
		=				enterInformation ("Create File",path) []
		>>*				[ OnAction ActionCancel always   (const (return (path,Nothing)))
						, OnAction ActionOk		hasValue (write o getValue)
						]
	write name
		=				accWorld (writeFile (path </> name) "")
		>>|				return (path,Just name)

	continue ".." = selectFileInPath (takeDirectory path) pred
	continue "."  = selectFileInPath path pred
	continue name
	| takeExtension name == "icl" 
		= (return (path,Just name))
	| takeExtension name == "dcl"
		= (return (path,Just name))
	| otherwise		
		=				accWorld (getFileInfo (path </> name))
		>>= \content ->	case content of
							Ok info -> if info.directory
								(selectFileInPath (path </> name) pred)
								(return (path,Just name))
							_		-> selectFileInPath path pred

storeFileInPath :: !DirPathName !FileName !String -> Task !Bool
storeFileInPath name string path
	= 				accWorld (readDirectory path)
	>>= \content -> case content of
						Ok names 	-> select names
						_ 			-> return False
where
	select names
		=				enterChoice ("Store " +++ name,path) [] names
						-&&-
						updateInformation "File name" [] name
		>>*				[ OnAction ActionCancel always  (const (return False))
						, OnAction (Action "Browse")	hasValue (browse o getValue)
						, OnAction ActionSave			hasValue (write o getValue)
						]
	write (path,name)
		=				accWorld (writeFile (path </> name) "")
		>>|				return True


	browse ("..",name)  = storeFileInPath (takeDirectory path) name string
	browse (".",name)   = storeFileInPath path name string
	browse (npath,name)		
		=				accWorld (getFileInfo npath)
		>>= \content ->	case content of
							Ok info -> if info.directory
								(storeFileInPath npath name string)
								(storeFileInPath path name string)
							_		-> storeFileInPath path name string

showError :: String a -> Task a | iTask a
showError prompt val = (viewInformation ("Error",prompt) [] val >>= \_ -> return val) <<@ Window

currentDirectory :: Task !DirPathName
currentDirectory 
	= 					accWorld getCurrentDirectory 
		>>= \content ->	case content of
							Ok dir -> return dir
							_      -> showError "Could not obtain current directory name" ""


searchFilesInPaths :: ![FileName] ![DirPathName] -> Task ![(!DirPathName,!FileName)]
searchFilesInPaths fileNames pathNames = search fileNames pathNames []
where
	search [] _ found = return (reverse found)
	search [fileName:fileNames] pathNames found
		=				searchFileInPaths fileName pathNames
		>>= \res ->		if (isNothing res)
							(search fileNames pathNames found)
							(search fileNames pathNames [(fromJust res,fileName):found])

searchFileInPaths :: !FileName ![DirPathName] -> Task !(Maybe !DirPathName)
searchFileInPaths fileName paths = accWorld (searchDisk` paths)
where
	searchDisk` [] world = (Nothing,world)
	searchDisk` [path:paths] world
	# (content,world) = readDirectory path world
	= case content of
		Ok names 	-> if (isMember fileName names) 
							((Just path),world)
							(searchDisk` paths world)
		_ 			-> searchDisk` paths world


//* simple utility functions


actionTask :: Task Void
actionTask = viewInformation Void [] Void

launch task ts = appendTask Embedded (const task) ts @ const Void

// tiny util


always = const True
never = const False

hasValue (Value _ _) = True
hasValue _ = False

getValue (Value v _) = v

ifValue pred (Value v _) = pred v
ifValue _ _ = False

ifStable (Value v Stable) = True
ifStable _ = False


