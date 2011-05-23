module textEdit

import iTasks

derive bimap (,), Maybe

Start w = startEngine [ workflow "text editor1" "simple text editor" textEditor1 // bug: don't use the same name twice !!!
					  , workflow "text editor2" "advanced text editor" (textEditor2 "aap.txt")
					  , workflow "shell" "simple shell" shell
					  ] w

// ---------

textEditor1 
	= 					updateInformation "Edit the text:" (Note "")
		>>= 			showMessage "Resulting text is:"

// ---------

import Text

voidResult _ _ = Void 


onlyIf :: Bool a -> Maybe a
onlyIf b do
	| b 		= Just do
	| otherwise	= Nothing

derive class iTask Replace, TextStatistics, EditorState

normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

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
initEditorState text 	= 	{mytext = text, replace = False, statistics = False}
updateReplace b  	=  	update (\s ->{s & replace = b}) 
updateStat b 		=	update (\s -> {s & statistics = b}) 
updateText f 		=	update (\s -> {s & mytext = f s.mytext}) 

ActionReplace 		:== Action "Replace" "Replace"
ActionStatistics	:== Action "Statistics" "Statistics"

textEditor2 :: String -> Task Void
textEditor2 fileName
	=						getPresentPath
		>>= \pwd ->			readFile pwd fileName
		>>= \(_,text) -> 	parallel "Editor" (initEditorState text) voidResult [taskKind (editor pwd fileName)]
where

	taskKind = InBodyTask
	
	taskKind2 = DetachedTask (normalTask  RootUser) myMenu // window does not work yet
	
	myMenu s =  [ Menu "File" 	[ MenuItem ActionSave (ctrl 's')
								, MenuSeparator
								, MenuItem ActionQuit (ctrl 'q')
								]
				, Menu "Edit" 	[ MenuItem "Replace"  (ctrl 'r')
								, MenuItem "Statistics" (ctrl 's')
								]				
				]
	where
		ctrl c = Just {key=c,ctrl=True,alt=False,shift=False}

	editor :: DirPath String (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	editor pwd fileName ls os 
		= 			updateSharedInformationA (fileName,"Edit text file " +++ fileName) (toView,fromView) ls
			>?* 	[ (ActionSave, 		IfValid	save)
			  		, (ActionQuit,		Always 	quit)
			  		, (ActionReplace,	Sometimes (\s -> onlyIf (not s.modelValue.replace)    replace))
			  		, (ActionStatistics,Sometimes (\s -> onlyIf (not s.modelValue.statistics) statistics))
			  		]
	where	
		toView state = Note state.mytext
		fromView (Note text) state = {state & mytext = text} 

		save val
			=		safeFile pwd fileName val.mytext
				>>|	editor pwd fileName ls os
		quit
			=		set os [StopParallel] 
				>>| return Void

		replace 
			=		updateReplace True ls
				>>| set os [AppendTask (InBodyTask (replaceTask {search = "", replaceBy = ""}))]
				>>| editor pwd fileName ls os

		statistics 
			=		updateStat True ls
				>>|	set os [AppendTask (InBodyTask statisticsTask)]
				>>| editor pwd fileName ls os

	replaceTask :: Replace (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	replaceTask replacement ls os
		=			updateInformationA ("Replace","Define replacement...") idView replacement
			>?*		[(ActionOk,   IfValid 	(\r -> 		updateText (replaceSubString r.search r.replaceBy) ls
								 					>>|	replaceTask r ls os))
					,(ActionQuit, Always 	(	updateReplace False ls 
												>>| return Void))
					]

	statisticsTask :: (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	statisticsTask ls os 
		= 			monitorA ("Statistics","Statistics of your document") toView ls
					(\_ -> UserActions [(ActionQuit, Just Void)]) 
	where
		toView state=:{mytext} 
			=	{ lines 	 = length (split "\n" mytext)
				, words 	 = length (split " " (replaceSubString "\n" " " mytext))
				, characters = textSize mytext
				}
					


// ---------

derive class iTask IFile
:: DirPath :== (String,[String])

:: IFile 		= 	FileName FileName 			// File is already taken as a name
				|	Directory FileName
:: FileName		:== String
:: DirectoryName:== [String]
:: FileContent	:== String

ActionNewFile 		:== Action "New File" "New File"
ActionNewDirectory	:== Action "New Directory" "New Directory"
ActionUp			:== Action "Up" "Up"
ActionDelete		:== Action "Delete" "Delete"
ActionNewShell		:== Action "New Shell" "New Shell"
ActionRefresh		:== Action "Refresh" "Refresh"

shell :: Task Void
shell 
	=					get currentUser
			>>= \me ->	getPresentPath
			>>= \pwd -> parallel  ("Shell","pwd = " <+++ pwd)  Void voidResult [newShell me pwd]
where
	newShell me pwd
		= 				DetachedTask (normalTask me) noMenu (shellInterpreter me pwd)

shellInterpreter :: User DirPath (SymmetricShared Void) (ParallelInfo Void) -> Task Void
shellInterpreter me pwd ls os
	= 					shellCommand pwd
		>>= \pwd ->		shellInterpreter me pwd ls os
where
	shellCommand pwd=:(abs,rel)
		=					readDir pwd
		>>= \(ok,names) -> 	updateInformationA ("Current directory: " <+++ pwd) idView (choice names)
		>?*		[ (ActionOpen, 		IfValid    (\val -> 	open pwd (getChoice val)))
				, (ActionUp, 		Sometimes  (\_ -> 		onlyIf (length rel > 0) (return (abs,init rel))))
				, (ActionNewFile, 	Always (				updateInformation "Choose file name:" ""
											>>= \newName -> safeFile pwd (newName +++ ".txt") ""
											>>|				return pwd)) 
				, (ActionRefresh,	Always (				return pwd))
				, (ActionDelete,	IfValid (\val ->		delete pwd (getChoice val)
											>>| 			return pwd))
				,(ActionNewDirectory, Always (				updateInformation "Choose directory name:" ""
											>>= \dirName -> newDir (abs,rel++[dirName])
											>>|				return pwd)) 
//				,(ActionNewShell, 	Just (					set os [AppendTask (DetachedTask (normalTask me) noMenu (shellInterpreter me pwd))] 
//											>>|				return (init pwd)))
				]

	getChoice (Choice elem i) = elem!!i

	open pwd (FileName name) 
		=				set os [AppendTask (DetachedTask (normalTask me) noMenu (\ls os -> textEditor2 name))] 
			>>|			return pwd
	open (path,pwd) (Directory name ) =	return (path,pwd ++ [name]) 
	
	delete pwd (FileName name)
		=				deleteFile pwd name
			>>|			return pwd
	delete pwd (Directory name)
		= 				removeDir pwd name
			>>|			return pwd


/*
editor file pwd name s os
	= 	updateSharedInformationA ("Dir: " <+++ pwd <+++ "; File: " <+++ name) (toView,fromView) actions file
where
	toView text = Note text
	fromView (Note text) _ = text 

 	actions _
 		=	[(ActionQuit,Just Void)]
*/


// -------------


//readDirectory pwd = let dir = openDir pwd in get dir >>= \files -> return (files, dir)
//where
//	openDir pwd = sharedStore (foldl (+++) "_" pwd) []

/*
createDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
removeDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
readDirectory :: !FilePath !*World -> (!MaybeOSError [FilePath], !*World)
getCurrentDirectory :: !*World -> (!MaybeOSError FilePath, !*World)
setCurrentDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
*/
// utility function for reading files

import StdFile
import FilePath, Directory, StdString
import _WinBase

derive class iTask MaybeError

instance toString DirPath 
where
	toString (absPath,names) = absPath +++ relativePath names
	where
		relativePath names = foldl (\x y -> x +++ (toString pathSeparator) +++ y) ""  names

getPresentPath :: Task DirPath
getPresentPath 
	=						accWorld getCurrentDirectory 
		>>= \(Ok dir) ->	return (dir,[])

safeFile :: DirPath String String -> Task Bool
safeFile dirPath fileName text 
	= 						accWorld (safeFileMonad dirPath fileName text)
where
	safeFileMonad :: DirPath String String *World -> (Bool,*World)
	safeFileMonad dirPath fileName text world 
	# (ok,file,world)  	= fopen (toString dirPath +++ toString pathSeparator +++ fileName) FWriteText world
	| not ok			= (False,world)
	# file				= fwrites text file
	= fclose file world

readFile :: DirPath String  -> Task (Bool,String)
readFile dirPath fileName  
	= 						accWorld (readFileMonad (toString dirPath +++ toString pathSeparator +++ fileName))
where
	readFileMonad :: String  *World -> ((Bool,String),*World)
	readFileMonad fileName world 
	# (ok,file,world)  	= fopen fileName FReadText world
	| not ok			= ((False,""),world)
	# (text,file)		= freads file 1000000
	| text == ""		= ((False,""),world)
	# (ok,world)		= fclose file world
	| not ok			= ((False,""),world)
	= ((True,text),world)

deleteFile :: DirPath String  -> Task Bool
deleteFile dirPath fileName 
	=				accWorld (deleteFileA (toString dirPath +++ toString pathSeparator +++ fileName))
		>>= \i ->	return (i == 0)

newDir :: DirPath -> Task Bool
newDir dirPath
	=						accWorld (createDirectory (toString dirPath))
		>>= \result ->		case result of
								(Ok _) -> return True
								_	-> return False

removeDir :: DirPath String -> Task Bool
removeDir dirPath dirName
	=						accWorld (removeDirectory (toString dirPath +++ toString pathSeparator +++ dirName))
		>>= \result ->		case result of
								(Ok _) -> return True
								_	-> return False

readDir :: DirPath -> Task (Bool,[IFile])
readDir dirPath 
	=						accWorld (readDirectory (toString dirPath))
		>>= \result ->		case result of
								(Error _) -> return (False,[])
								(Ok names) -> return (True,[FileName name \\ name <- names | takeExtension name == "txt"] ++
														   [Directory name \\ name <- names | takeExtension name == "" && name <> "." && name <> ".."])







	
	