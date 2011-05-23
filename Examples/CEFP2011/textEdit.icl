module textEdit

import iTasks

derive bimap (,), Maybe

Start w = startEngine [ //workflow "text editor1" "simple text editor" textEditor1 // bug: don't use the same name twice !!!
//					  , workflow "shell" "simple shell" (shell [])
	//				  , 
					  workflow "text2" "advanced text editor" (textEditor2 "aap")
					  ] w

// ---------

import Text

voidResult _ _ = Void 

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
updateText f s		=	{s & mytext = f s.mytext}

ActionReplace 		:== Action "Replace" "Replace"
ActionStatistics	:== Action "Statistics" "Statistics"

textEditor2 :: String -> Task Void
textEditor2 name
	=					parallel "Editor" initEditorState voidResult [InBodyTask editor]
where
	editor :: (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	editor ls os 
		= 			updateSharedInformationA (name,"Edit text...") (toView,fromView) ls
			>?* 	[ (ActionSave, 		IfValid	(\val ->	safeFile name val.mytext
														>>| editor ls os)
												)
			  		, (ActionQuit,		Always 	(			set os [StopParallel] 
														>>| return Void)
												)
			  		, (ActionReplace,	Sometimes (\state -> if state.modelValue.replace    Nothing (Just replace)))
			  		, (ActionStatistics,Sometimes (\state -> if state.modelValue.statistics Nothing (Just statistics)))
			  		]
	where	
		toView state = Note state.mytext
		fromView (Note text) state = {state & mytext = text} 

		replace 
			=		update (updateReplace True) ls
				>>| set os [AppendTask (InBodyTask (replaceTask {search = "", replaceBy = ""}))]
				>>| return Void

		statistics 
			=		update (updateStat True) ls
				>>|	set os [AppendTask (InBodyTask statisticsTask)]
				>>| return Void

	replaceTask :: Replace (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	replaceTask replacement ls os
		=			updateInformationA ("Replace","Define replacement...") idView replacement
			>?*		[(ActionOk,   IfValid 	(\r -> 		update (updateText (replaceSubString r.search r.replaceBy)) ls
								 					>>|	replaceTask r ls os))
					,(ActionQuit, Always 	(	update (updateReplace False) ls 
												>>| return Void))
					]

	statisticsTask :: (SymmetricShared EditorState) (ParallelInfo EditorState) -> Task Void
	statisticsTask ls os 
		= 			monitorA ("Statistics","Statistics of your document") toView ls
					(\_ -> UserActions [(ActionQuit, Just Void)]) 
	where
		toView state=:{mytext} 
			=	 {lines = length (split "\n" mytext), words = length (split " " (replaceSubString "\n" " " mytext)), characters = textSize mytext}
					

/*
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

	
normalTask user = { worker = user, priority = NormalPriority, deadline = Nothing, status = Active}

shell :: DirectoryName ->Task Void
shell pwd
	=					get currentUser
			>>= \me ->	parallel  ("Shell","pwd = " <+++ pwd)  Void voidResult [newShell me initDirectory]
where
	newShell me pwd
		= 				DetachedTask (normalTask me) noMenu (shellInterpreter me pwd)
			
ActionNewFile 		:== Action "New File" "New File"
ActionNewDirectory	:== Action "New Directory" "New Directory"
ActionUp			:== Action "Up" "Up"
ActionDelete		:== Action "Delete" "Delete"
ActionNewShell		:== Action "New Shell" "New Shell"

shellInterpreter :: User DirPath (SymmetricShared Void) (ParallelInfo Void) -> Task Void
shellInterpreter me pwd ls os
	= 					shellCommand pwd
		>>= \pwd ->		shellInterpreter me pwd os
where
	shellCommand pwd
		=						readDirectory pwd
		>>= \(myFiles,dir) -> 	updateInformationA ("Current directory: " <+++ pwd) idView (actions dir) (choice myFiles)
		>>=						id
	where
		actions dir (Just val)
			= 	[(ActionOpen, 		Just (					open pwd (getChoice val)))
				,(ActionDelete,		Just (					delete val dir 
											>>| 			return pwd))
				,(ActionNewFile, 	Just (					updateInformation "Choose file name:" ""
											>>= \newName -> update (\dir -> dir ++ [FileName newName]) dir
											>>|				return pwd)) 
				,(ActionNewDirectory, Just(					updateInformation "Choose directory name:" ""
											>>= \newName -> update (\dir -> dir ++ [Directory newName]) dir 
											>>|				return pwd)) 
				,(ActionUp, 		Just(					return (init pwd)))
				,(ActionNewShell, 	Just (					set os [AppendTask (DetachedTask (normalTask me) noMenu (shellInterpreter me pwd))] 
											>>|				return (init pwd)))
				]

		

	open pwd (FileName name) 
		=				set os [AppendTask (DetachedTask (normalTask me) noMenu (editor (openFile pwd name) pwd name))] 
			>>|			return pwd
	where
		openFile pwd name 	= sharedStore (foldl (+++) "_" (pwd ++ [name])) ""

	open pwd (Directory name ) =	return (pwd ++ [name]) 
	
	delete (Choice elem i) dir = update (\dir -> removeAt i dir) dir

	getChoice (Choice elem i) = elem!!i

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
import StdFile

safeFile :: String String -> Task Bool
safeFile fileName text 
	= 						accWorld (safeFileMonad fileName text)

safeFileMonad :: String String *World -> (Bool,*World)
safeFileMonad fileName text world 
# (ok,file,world)  	= fopen fileName FWriteText world
# file				= fwrites text file
= fclose file world


:: DirPath :== (String,[String])

import FilePath, Directory, StdString

derive class iTask MaybeError

initDirectory :: Task DirPath
initDirectory 
	=						accWorld getCurrentDirectory 
		>>= \(Ok dir) ->	return (dir,[])

newDirectory :: DirPath -> Task Bool
newDirectory (prefix,names)
	=						accWorld (createDirectory (prefix +++ dirName))
		>>= \result ->		case result of
								(Ok _) -> return True
								_	-> return False
								
where
	dirName = foldl (\x y -> x +++ (toString pathSeparator) +++ y) ""  names











	
	