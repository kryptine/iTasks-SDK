implementation module BasicAPIExamples.InteractionUsingShares.SharedNoteAsList

// Update a shared note in a text area and as list

import iTasks
import Text

wf :: String -> Workflow
wf a = workflow a "Edit a shared note as a list" sharedNoteAsList

main :: Task ()
main = sharedNoteAsList @! ()

sharedNoteAsList :: Task String
sharedNoteAsList
	=	withShared "" doEditor
where
	doEditor state
		= 		(Title "Text" @>> Hint "Edit text" @>> updateSharedInformation [noteEditor] state)
				-||-
				(Title "Lines" @>> Hint "Edit lines" @>> updateSharedInformation [listEditor] state)
						<<@ ArrangeHorizontal
		>>=		\result -> Hint "Result:" @>> viewInformation [] result
		>>=		return

	noteEditor = UpdateSharedUsing id (const id) const textArea
	listEditor = UpdateSharedAs (split "\n") (\_ l -> join "\n" l) const
