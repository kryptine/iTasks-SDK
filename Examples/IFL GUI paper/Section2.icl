implementation module Section2

import iTasks, StdMisc, Text

section2 :: Workflow
section2 = workflow "Section 2" "" textEditorApplication

:: State = State !Note !FileInfo
:: FileInfo = NewFile | StoredFile !String !(DBId Note)

derive class iTask	State, FileInfo
derive bimap (,), Maybe

initState :: State
initState = State (Note "") NewFile

ActionEdit		:== "Edit Content"
ActionNew		:== "New"
ActionOpen		:== "Open"
ActionSave		:== "Save"
ActionSaveAs	:== "SaveAs"
ActionQuit		:== "Quit"
ActionReplace	:== "Replace"

allActions :: State -> [String]
allActions (State (Note cont) _) =
	[ActionEdit, ActionNew, ActionOpen, ActionSave, ActionSaveAs, ActionQuit]
	++ if (cont <> "") [ActionReplace] []

textEditorApplication = performAction initState

performAction :: State -> Task State
performAction state =
					enterChoice "Choose Action" "Which action to perform?" (allActions state)
	>>= \action.	case action of
						ActionEdit    = edit state    >>= performAction
						ActionNew     =                   performAction initState
						ActionOpen    = openFile      >>= performAction
						ActionSave    = save state    >>= performAction
						ActionSaveAs  = saveAs state  >>= performAction
						ActionQuit    =                   return state
						ActionReplace = replace state >>= performAction
		
edit :: State -> Task State
edit (State content file) =
						updateInformation "New content" "Update the file's content." content
	>>= \newContent.	return (State newContent file)


saveAs state = undef

save :: State -> Task State
save state=:(State content fileInfo) = case fileInfo of
  NewFile          = saveAs state
  StoredFile _ ref =
        writeDB ref content
    >>| return state
  
openFile = undef

:: Replace =	{ searchFor		:: String
				, replaceWith	:: String
				}
derive class iTask Replace
replace :: State -> Task State
replace state=:(State (Note cont) info) =
							enterInformationA "Replace" "..." buttons
	>>= \((action,_), repl).	case action of
								Action "replaceAll" _ = return (State (Note (replaceSubString repl.searchFor repl.replaceWith cont)) info)
								ActionClose = return state
where									
	buttons = [(ActionClose, always), (Action "replaceAll" "Replace All", ifvalid)]