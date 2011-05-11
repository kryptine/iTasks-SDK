module textEdit

import iTasks

derive bimap (,), Maybe

Start w = startEngine [ workflow "text editor1" "simple text editor" textEditor1 // bug: don't use the same name twice !!!
					  , workflow "text editor2" "simple text editor" (textEditor2 [])
					  ] w

// ---------

textEditor1 
	= 					updateInformation "Edit the text:" (Note "")
		>>= 			showMessage "Resulting text is:"



// ---------

:: Directory 	:== [IFile]
:: IFile 		= 	FileName FileName 			// File is already taken as a name
				|	Directory FileName
:: FileName		:== String
:: DirectoryName:== [String]
:: FileContent	:== String

derive class iTask IFile
	
ActionNewFile 		:== Action "New File" "New File"
ActionNewDirectory	:== Action "New Directory" "New Directory"
ActionNewDirectory	:== Action "New Directory" "New Directory"

textEditor2 :: DirectoryName ->Task Void
textEditor2 pwd
	= 										chooseFile pwd
		>>= \(file,pwd,fileName,content) -> updateSharedInformationA ("Dir: " <+++ pwd <+++ "; File: " <+++ fileName) (toView,fromView) [(ActionQuit,alwaysShared)] file
//		>>= \(Note content) ->				writeShared file content
		>>|									textEditor2 pwd
where
	toView text = Note text
	fromView (Note text) _ = text 

	chooseFile pwd
		=						readDirectory pwd
		>>= \(myFiles,dir) -> 	updateInformationA ("Current directory: " <+++ pwd) idView actions (choice myFiles)
			>>= \(event,val) -> case event of 
									ActionNewFile ->  					updateInformation "Choose file name:" ""
														>>= \newName -> updateShared (\dir -> dir ++ [FileName newName]) dir
														>>|				chooseFile pwd
									ActionNewDirectory ->  				updateInformation "Choose directory name:" ""
														>>= \newName -> updateShared (\dir -> dir ++ [Directory newName]) dir 
														>>|				chooseFile pwd
									ActionOpen ->							case (getChoice (fromJust val)) of 
																			(FileName name) -> 					readShared (openFile pwd name)
																								>>= \content -> return (openFile pwd name, pwd, name, content)
																			(Directory name ) ->				chooseFile (pwd ++ [name])
	
	actions
		= [(ActionOpen,ifvalid),(ActionNewFile,always),(ActionNewDirectory,always)]


//	readDirectory :: DirectoryName -> Task (IFile, SymmetricShared IFile)
	readDirectory pwd = let dir = openDir pwd in readShared dir >>= \files -> return (files, dir)

	openDir pwd			= sharedStore (foldl (+++) "_" pwd) []
	openFile pwd name 	= sharedStore (foldl (+++) "_" (pwd ++ [name])) ""

	getChoice (Choice elem i) = elem!!i

	
	