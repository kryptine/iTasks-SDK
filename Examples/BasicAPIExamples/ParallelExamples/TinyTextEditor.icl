implementation module BasicAPIExamples.ParallelExamples.TinyTextEditor

// A tiny editor with two windows, one to replace test, one to show statistical information

import iTasks
import Text, Data.Maybe

wf :: String -> Workflow
wf a = workflow a "Tiny text editor" editWithStatistics

main :: Task ()
main = editWithStatistics @! ()

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
 =						enterInformation [EnterWithHint "Give name of text file you want to edit..."]
	>>= \fileName -> 	let file = sharedStore fileName ""
						in	editFile fileName file
							-||-
							(showStatistics file -||- replace initReplace file <<@ ApplyLayout arrangeHorizontal)
							>>*	 [ OnAction (Action "Quit") (always (return ()))
								 ]

editFile :: String (Shared sds String)  -> Task () | RWShared sds
editFile fileName sharedFile
	=	updateSharedInformation [UpdateSharedWithHint ("edit " +++ fileName), UpdateSharedUsing id (const id) const textArea] sharedFile @! ()

showStatistics :: (Shared sds String) -> Task () | RWShared sds
showStatistics sharedFile = viewSharedInformation [ViewWithHint "Statistics:", ViewAs stat] sharedFile @! ()
where
	stat text = {lineCount = lengthLines text, wordCount = lengthWords text}
	where
		lengthLines ""   = 0
		lengthLines text = length (split "\n" text)

		lengthWords "" 	 = 0
		lengthWords text = length (split " " (replaceSubString "\n" " " text))

replace :: Replace (Shared sds String) -> Task () | RWShared sds
replace cmnd sharedFile
 = 	(	updateInformation [UpdateWithHint "Replace:"] cmnd
	>>*	[ OnAction (Action "Replace") (hasValue substitute)
		]
	)
where
 	substitute cmnd
	 	=	upd (replaceSubString cmnd.search (if (isNothing cmnd.replaceBy) "" (fromJust cmnd.replaceBy))) sharedFile
	 	>>| replace cmnd sharedFile

