implementation module Tests.Interactive.TaskPatterns
import iTasks.Internal.Test.Definition
import Data.Maybe, Text
import qualified Data.Map as DM
import iTasks.UI.Editor.Builtin

testTaskPatternsI :: TestSuite
testTaskPatternsI = testsuite "Task patterns" "Tests for common task patterns" 
	[testMultipleViewsOnSDS]


testMultipleViewsOnSDS= itest "Multiple views on SDS" "Edit either the text, or the list of lines." "Both editors should update each other" sut
where
	sut :: Task (Maybe String)
	sut = withShared "" editTextWithDifferentViews

	editTextWithDifferentViews model
		= 			editInTextArea model
					-||- 
					editAsListOfLines model
			>>* 	[OnAction ActionQuit (Just o return o toMaybe)]

	editInTextArea model
		= 			updateSharedInformation ("Text","Edit text") [noteEditor] model
		>^*			[ OnAction (Action "Trim") (\txt -> Just (upd trim model))	
					]

	editAsListOfLines model
		=	updateSharedInformation ("Lines","Edit lines") [listEditor] model

	noteEditor = UpdateUsing id (const id) (textArea 'DM'.newMap)
	listEditor = UpdateAs (split "\n") (\_ l -> join "\n" l)

	toMaybe (Value v _) =  (Just v)
	toMaybe _   =  Nothing

