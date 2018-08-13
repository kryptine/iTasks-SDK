implementation module BasicAPIExamples.InteractionUsingShares.SharedNoteAsList

// Update a shared note in a text area and as list

import iTasks
import Text

wf :: String -> Workflow
wf a = workflow a "Edit a shared note as a list" sharedNoteAsList

Start :: *World -> *World
Start world
	= doTasks sharedNoteAsList world


sharedNoteAsList :: Task String
sharedNoteAsList
	=	withShared "" doEditor
where
	doEditor state
		= 		updateSharedInformation ("Text","Edit text")  [noteEditor] state
				-||-
				updateSharedInformation ("Lines","Edit lines") [listEditor] state
						<<@ ApplyLayout horizontal
		>>=		viewInformation "Result:" []
		>>=		return

	noteEditor = UpdateUsing id (const id) textArea
	listEditor = UpdateAs (split "\n") (\_ l -> join "\n" l)

	horizontal = setUIAttributes (directionAttr Horizontal)
