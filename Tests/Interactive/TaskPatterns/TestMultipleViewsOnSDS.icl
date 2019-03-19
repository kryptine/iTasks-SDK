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
	=   updateSharedInformation ("Lines","Edit lines") [listEditor] model

noteEditor = UpdateUsing id (const id) textArea
listEditor = UpdateAs (split "\n") (\_ l -> join "\n" l)

toMaybe (Value v _) =  (Just v)
toMaybe _   =  Nothing

Start world = doTasks test world
