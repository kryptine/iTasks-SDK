implementation module BasicAPIExamples.ParallelExamples.TinyTextEditor

// A tiny editor with two windows, one to replace test, one to show statistical information

import iTasks
import Text, Data.Maybe

wf :: String -> Workflow
wf a = workflow a "Tiny text editor" editWithStatistics

Start :: *World -> *World
Start world
	= doTasks editWithStatistics world

:: Statistics 	= 	{	lineCount :: Int
					,	wordCount :: Int
					}
:: Replace		=	{ search    :: String
					, replaceBy :: Maybe String
					}
initReplace = { search = "", replaceBy = Nothing}

derive class iTask Statistics, Replace


editWithStatistics :: Task ()
editWithStatistics
 =						enterInformation "Give name of text file you want to edit..." []
	>>= \fileName -> 	let file = sharedStore fileName ""
						in	editFile fileName file
							-||-
							(showStatistics file -||- replace initReplace file <<@ ApplyLayout arrangeHorizontal)
							>>*	 [ OnAction (Action "Quit") (always (return ()))
								 ]

editFile :: String (Shared String)  -> Task ()
editFile fileName sharedFile
	=	updateSharedInformation ("edit " +++ fileName) [UpdateUsing id (const id) textArea] sharedFile @! ()

showStatistics :: (Shared String) -> Task ()
showStatistics sharedFile = viewSharedInformation "Statistics:" [ViewAs stat] sharedFile @! ()
where
	stat text = {lineCount = lengthLines text, wordCount = lengthWords text}
	where
		lengthLines ""   = 0
		lengthLines text = length (split "\n" text)

		lengthWords "" 	 = 0
		lengthWords text = length (split " " (replaceSubString "\n" " " text))

replace :: Replace (Shared String) -> Task ()
replace cmnd sharedFile
 = 	(	updateInformation "Replace:" [] cmnd
	>>*	[ OnAction (Action "Replace") (hasValue substitute)
		]
	)
where
 	substitute cmnd
	 	=	upd (replaceSubString cmnd.search (if (isNothing cmnd.replaceBy) "" (fromJust cmnd.replaceBy))) sharedFile
	 	>>| replace cmnd sharedFile

