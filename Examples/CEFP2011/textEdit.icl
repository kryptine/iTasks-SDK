module textEdit

import iTasks

derive bimap (,), Maybe

Start w = startEngine [ workflow "text editor1" "simple text editor" textEditor1 // bug: don't use the same name twice !!!
					  , workflow "shell" "simple shell" (shell [])
					  , workflow "text2" "advanced text editor" (textEditor2 "aap")
					  ] w

// ---------

//updateSharedInformationA		:: !d !(View r v w) ![PredAction (Valid,r)]			!(Shared r w) -> Task (!Action,!r)	| descr d & iTask r & iTask v & iTask w

import Text

derive class iTask Replace, TextStatistics

:: Replace			=	{ search 		:: String
						, replaceBy 	:: String
						}
:: TextStatistics 	=	{ lines			:: Int
						, words			:: Int
						, characters	:: Int
						}

ActionReplace 		:== Action "Replace" "Replace"
ActionStatistics	:== Action "Statistics" "Statistics"

textEditor2 :: String -> Task Void
textEditor2 name
	=					parallel ("Editor") "" voidResult [InBodyTask editor]
where
	editor :: (SymmetricShared String) (ParallelInfo String) -> Task Void
	editor s os 
		= 						updateSharedInformationA (name,"Edit text...") (toView,fromView) actions s
			>>= \(event,val) ->	case event of
									ActionReplace -> 				writeShared os [AppendTask (InBodyTask (replace {search = "", replaceBy = ""}))]
														>>|			editor s os
									ActionStatistics -> 			writeShared os [AppendTask (InBodyTask statistics)]
														>>|			editor s os
									ActionSave -> 					writeShared (sharedStore name "") val
														>>| 		editor s os
									ActionQuit -> 		return Void
		
	toView text = Note text
	fromView (Note text) _ = text 

	actions
		= [ (ActionReplace,ifvalidShared)
		  , (ActionStatistics,ifvalidShared)
		  , (ActionSave,ifvalidShared)
		  , (ActionQuit,alwaysShared)
		  ]

	replace replacement s os
		=						updateInformationA ("Replace","Define replacement...") idView [(ActionOk,ifvalid),(ActionQuit,always)] replacement
			>>= \(event,val) ->	case event of
									ActionOk ->		let r = fromJust val in
														updateShared (\text -> replaceSubString r.search r.replaceBy text) s
												>>|	replace r s os 
									ActionQuit -> 	return Void

	statistics s os 
		= 						monitorA ("Statistics","Statistics of your document") toView (\_->False) [(ActionQuit,\_->True)] s
			>>|					return Void
	where
		toView txt 
			=	 {lines = length (split "\n" txt), words = length (split " " (replaceSubString "\n" " " txt)), characters = textSize txt}
					


// ---------

textEditor1 
	= 					updateInformation "Edit the text:" (Note "")
		>>= 			showMessage "Resulting text is:"

// ---------

derive class iTask IFile

:: Directory 	:== [IFile]
:: IFile 		= 	FileName FileName 			// File is already taken as a name
				|	Directory FileName
:: FileName		:== String
:: DirectoryName:== [String]
:: FileContent	:== String

voidResult _ _ = Void 
	
normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

shell :: DirectoryName ->Task Void
shell pwd
	=					getCurrentUser
			>>= \me ->	parallel  ("Shell","pwd = " <+++ pwd) Void voidResult [newShell me]
where
	newShell me
		= 				DetachedTask (normalTask me) noMenu (shellHandler me)
			
shellHandler :: User (SymmetricShared Void) (ParallelInfo Void) -> Task Void
shellHandler me s os = shellInterpreter me [] os

ActionNewFile 		:== Action "New File" "New File"
ActionNewDirectory	:== Action "New Directory" "New Directory"
ActionUp			:== Action "Up" "Up"
ActionDelete		:== Action "Delete" "Delete"
ActionNewShell		:== Action "New Shell" "New Shell"

shellInterpreter :: User DirectoryName (ParallelInfo Void) -> Task Void
shellInterpreter me pwd os
	= 					shellCommand pwd
		>>= \pwd ->		shellInterpreter me pwd os
where
	shellCommand pwd
		=						readDirectory pwd
		>>= \(myFiles,dir) -> 	updateInformationA ("Current directory: " <+++ pwd) idView actions (choice myFiles)
			>>= \(event,val) -> case event of 
									ActionOpen ->						open pwd (getChoice (fromJust val))
									ActionDelete ->						delete (fromJust val) dir 
														>>| 			return pwd 
									ActionNewFile ->  					updateInformation "Choose file name:" ""
														>>= \newName -> updateShared (\dir -> dir ++ [FileName newName]) dir
														>>|				return pwd 
									ActionNewDirectory ->  				updateInformation "Choose directory name:" ""
														>>= \newName -> updateShared (\dir -> dir ++ [Directory newName]) dir 
														>>|				return pwd 
									ActionUp ->							return (init pwd)
									ActionNewShell ->					writeShared os [AppendTask (DetachedTask (normalTask me) noMenu (shellHandler me))] 
														>>|				return (init pwd)
	where
		actions
			= [ (ActionOpen,ifvalid)
			  , (ActionDelete,ifvalid)
			  , (ActionNewFile,always)
			  , (ActionNewDirectory,always)
			  , (ActionUp,\_ -> length pwd > 0)
			  , (ActionNewShell,always)
			  ]
		where
			isParentDir (Valid (Choice dir i)) = length dir > 0
			isParentDir _ = False

	readDirectory pwd = let dir = openDir pwd in readShared dir >>= \files -> return (files, dir)
	where
		openDir pwd = sharedStore (foldl (+++) "_" pwd) []
		

	open pwd (FileName name) 
		=				writeShared os [AppendTask (DetachedTask (normalTask me) noMenu (editor (openFile pwd name) pwd name))] 
			>>|			return pwd
	where
		openFile pwd name 	= sharedStore (foldl (+++) "_" (pwd ++ [name])) ""

	open pwd (Directory name ) =	return (pwd ++ [name]) 
	
	delete (Choice elem i) dir = updateShared (\dir -> removeAt i dir) dir

	getChoice (Choice elem i) = elem!!i

editor file pwd name s os
	= 	updateSharedInformationA ("Dir: " <+++ pwd <+++ "; File: " <+++ name) (toView,fromView) [(ActionQuit,alwaysShared)] file
where
	toView text = Note text
	fromView (Note text) _ = text 

 






















	
	