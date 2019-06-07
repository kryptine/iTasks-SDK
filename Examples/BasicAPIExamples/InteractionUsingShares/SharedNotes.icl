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
	= withShared ""																									// create an initial empty shared string
		(\note -> 	viewSharedInformation [ViewWithHint "View on note", ViewUsing id textArea] note								// one to view the resulting string
					-||-
					(	updateSharedInformation [UpdateSharedWithHint "Update shared note 1", UpdateSharedUsing id (const id) const textArea] note	// an editor to update the shared string
					 	-||-
					 	updateSharedInformation [UpdateSharedWithHint "Update shared note 2", UpdateSharedUsing id (const id) const textArea] note	// and an other updating editor
					 		<<@ ArrangeHorizontal 
					 )
		)
	>>= viewInformation [ViewWithHint "Resulting string is:", ViewUsing id textArea]
	>>= return
