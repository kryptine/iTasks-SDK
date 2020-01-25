implementation module BasicAPIExamples.InteractionUsingShares.SharedNotes

// Two updates on shared string and one view on it

import iTasks

wf :: String -> Workflow
wf a = workflow a "Edit a shared note" sharedNotes

main :: Task ()
main = sharedNotes @! ()

// Update and view shared notifications

sharedNotes :: Task String
sharedNotes
	// create an initial empty shared string
	= withShared ""
		(\note -> // one to view the resulting string
			(Hint "View on note" @>> viewSharedInformation [ViewUsing id (ignoreEditorWrites textArea)] note)
			-||-
			// an editor to update the shared string
			((Hint "Update shared note 1" @>> updateSharedInformation [UpdateSharedUsing id (const id) (const o Just) (ignoreEditorWrites textArea)] note)
			  -||-
			// and an other updating editor
			 (Hint "Update shared note 2" @>> updateSharedInformation [UpdateSharedUsing id (const id) (const o Just) (ignoreEditorWrites textArea)] note)
			) <<@ ArrangeHorizontal 
		)
	>>= \result -> Hint "Resulting string is:" @>> viewInformation [ViewUsing id (ignoreEditorWrites textArea)] result
	>>= return
