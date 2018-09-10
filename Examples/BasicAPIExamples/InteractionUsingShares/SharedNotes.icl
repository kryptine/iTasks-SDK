implementation module BasicAPIExamples.InteractionUsingShares.SharedNotes

// Two updates on shared string and one view on it

import iTasks

wf :: String -> Workflow
wf a = workflow a "Edit a shared note" sharedNotes

Start :: *World -> *World
Start world
	= doTasks sharedNotes world

// Update and view shared notifications

sharedNotes :: Task String
sharedNotes
	= withShared ""																									// create an initial empty shared string
		(\note -> 	viewSharedInformation "View on note" [ViewUsing id textArea] note								// one to view the resulting string
					-||-
					(	updateSharedInformation "Update shared note 1" [UpdateUsing id (const id) textArea] note	// an editor to update the shared string
					 	-||-
					 	updateSharedInformation "Update shared note 2"  [UpdateUsing id (const id) textArea] note	// and an other updating editor
					 		<<@ ApplyLayout horizontal
					 )
		)
	>>= viewInformation "Resulting string is:" [ViewUsing id textArea]
	>>= return
where
	horizontal = setUIAttributes (directionAttr Horizontal)