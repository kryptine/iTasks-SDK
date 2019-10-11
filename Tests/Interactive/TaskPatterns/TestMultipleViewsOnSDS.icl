module TestMultipleViewsOnSDS
import iTasks

test :: Task (Maybe String)
test = withShared "" editTextWithDifferentViews

editTextWithDifferentViews model
	=           editInTextArea model
				-||-
				editAsListOfLines model
		>>*     [OnAction ActionQuit (Just o return o toMaybe)]

editInTextArea model
	=           updateSharedInformation ("Text","Edit text") [noteEditor] model
	>^*         [ OnAction (Action "Trim") (\txt -> Just (upd trim model))  
				]

editAsListOfLines model
	=   Title "Lines" @>> Hint "Edit lines" @>> updateSharedInformation [listEditor] model

noteEditor = UpdateSharedUsing id (const id) (const o Just) textArea
listEditor = UpdateSharedAs (split "\n") (\_ l -> join "\n" l) (const o Just)

toMaybe (Value v _) =  (Just v)
toMaybe _   =  Nothing

Start world = doTasks test world
