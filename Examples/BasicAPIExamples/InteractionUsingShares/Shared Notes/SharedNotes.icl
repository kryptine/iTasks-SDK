module SharedNotes

// Two updates on shared string and one view on it

import iTasks

Start :: *World -> *World
Start world 
	= startEngine SharedNotes world
	
// Update and view shared notifications

SharedNotes :: Task String
SharedNotes 
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