module textEdit

import iTasks

derive bimap (,), Maybe

Start w = startEngine [ workflow "text editor1" "simple text editor" textEditor1 // bug: don't use the same name twice !!!
					  , workflow "shell" "simple shell" (shell [])
					  , workflow "text2" "advanced text editor" (textEditor2 "aap")
					  ] w

// ---------

import Text

derive class iTask Replace, TextStatistics, EditorState

:: Replace			=	{ search 		:: String
						, replaceBy 	:: String
						}
:: TextStatistics 	=	{ lines			:: Int
						, words			:: Int
						, characters	:: Int
						}
:: EditorState		=	{ mytext		:: String
						, replace		:: Bool
						, statistics	:: Bool
						}
initEditorState 	= 	{mytext = "", replace = False, statistics = False}
updateReplace b s	=  	{s & replace = b}
updateStat b s		=	{s & statistics = b}

ActionReplace 		:== Action "Replace" "Replace"
ActionStatistics	:== Action "Statistics" "Statistics"

textEditor2 :: String -> Task Void
textEditor2 name
	=					parallel "Editor" initEditorState voidResult [InBodyTask editor, InBodyTask viewOptions]
where
	editor :: (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	editor s os 
		= 						updateSharedInformationA (name,"Edit text...") (toView,fromView) actions s
			>>= \(event,val) ->	case event of
									ActionSave -> 					set (sharedStore name "") val.mytext
														>>| 		editor s os
									ActionQuit -> 					set os [StopParallel] 
														>>| 		return Void
	where	
		toView state = Note state.mytext
		fromView (Note text) state = {state & mytext = text} 

		actions
			= [ (ActionSave,alwaysShared)
			  , (ActionQuit,alwaysShared)
			  ]

	viewOptions :: (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task [Control EditorState]
	viewOptions s os = forever viewOption
	where
		viewOption 
			= 					chooseActionDyn "try" actions s
				>>= \task -> 	task
	
		actions state
			= [ (ActionReplace, 	if state.replace    Nothing (Just replace))
			  , (ActionStatistics,	if state.statistics Nothing (Just statistics))
			  ]
		where
			replace 
				=		update (updateReplace True) s
					>>| set os [AppendTask (InBodyTask (replaceTask {search = "", replaceBy = ""}))]
	
			statistics 
				=		update (updateStat True) s
					>>|	set os [AppendTask (InBodyTask statisticsTask)]

	replaceTask :: Replace (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	replaceTask replacement s os
		=						updateInformationA ("Replace","Define replacement...") idView [(ActionOk,ifvalid),(ActionQuit,always)] replacement
			>>= \(event,val) ->	case event of
									ActionOk ->		let r = fromJust val in
														update (\state -> {state & mytext = replaceSubString r.search r.replaceBy state.mytext}) s
												>>|	replaceTask r s os 
									ActionQuit -> 	update (updateReplace False) s >>| return Void

	statisticsTask :: (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	statisticsTask s os 
		= 						monitorA ("Statistics","Statistics of your document") toView (\_->False) [(ActionQuit,\_->True)] s
			>>|					return Void
	where
		toView state=:{mytext} 
			=	 {lines = length (split "\n" mytext), words = length (split " " (replaceSubString "\n" " " mytext)), characters = textSize mytext}
					


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
														>>= \newName -> update (\dir -> dir ++ [FileName newName]) dir
														>>|				return pwd 
									ActionNewDirectory ->  				updateInformation "Choose directory name:" ""
														>>= \newName -> update (\dir -> dir ++ [Directory newName]) dir 
														>>|				return pwd 
									ActionUp ->							return (init pwd)
									ActionNewShell ->					set os [AppendTask (DetachedTask (normalTask me) noMenu (shellHandler me))] 
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

	readDirectory pwd = let dir = openDir pwd in get dir >>= \files -> return (files, dir)
	where
		openDir pwd = sharedStore (foldl (+++) "_" pwd) []
		

	open pwd (FileName name) 
		=				set os [AppendTask (DetachedTask (normalTask me) noMenu (editor (openFile pwd name) pwd name))] 
			>>|			return pwd
	where
		openFile pwd name 	= sharedStore (foldl (+++) "_" (pwd ++ [name])) ""

	open pwd (Directory name ) =	return (pwd ++ [name]) 
	
	delete (Choice elem i) dir = update (\dir -> removeAt i dir) dir

	getChoice (Choice elem i) = elem!!i

editor file pwd name s os
	= 	updateSharedInformationA ("Dir: " <+++ pwd <+++ "; File: " <+++ name) (toView,fromView) [(ActionQuit,alwaysShared)] file
where
	toView text = Note text
	fromView (Note text) _ = text 

 






















	
	