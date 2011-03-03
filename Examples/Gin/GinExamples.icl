implementation module GinExamples

import iTasks

import GinEditor
import GinDomain
from GinSyntax import ::GModule

import GinTypes

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ ginExamples ]

ginExamples :: [Workflow]
ginExamples = [ workflow "Examples/Graphical Editors/Graphical Editor" "Create or edit workflows in Gin notation" ginEditor
			  , simpleEditorWorkflow "Petri net" petriNetORYXEditor
//			  , simpleEditorWorkflow "BPMN" bpmnORYXEditor
			  , simpleEditorWorkflow "Gin" ginORYXEditor
              , workflow "Examples/Graphical Editors/Shared Petri net editors" "Two shared Petri net editors" petrinetShareExample
              , workflow "Examples/Graphical Editors/Shared Gin editor" "Gin editor" sourceGinEditor
			  ]
			  
simpleEditorWorkflow :: !String !ORYXEditor -> Workflow
simpleEditorWorkflow language editor = 
	workflow ("Examples/Graphical Editors/" +++ language +++ " editor") ("Simple " +++ language +++ " editor")
		(updateInformationA ("Simple " +++ language +++ " editor") idView [quitButton] editor <<@ FWFullWidth)

petrinetShareExample = createSharedStore petriNetORYXEditor
	>>= \dbid -> updateSharedInformationA "Editor 1" idView [quitButton] dbid
				 -||
				 updateSharedInformationA "Editor 2" idView [quitButton] dbid

sourceGinEditor = createSharedStore ginORYXEditor
	>>= \dbid -> (updateSharedInformationA "Graphical editor" idView [quitButton] dbid <<@ FWFullWidth)
				 -||
				 updateSharedInformationA "Source view" (get,putback) [quitButton] dbid
where
	get		= \editor -> Note (editor.ORYXEditor.toString editor)
	putback	= \_ -> id

quitButton :: (Action,(Verified a) -> Bool)
quitButton = (ActionQuit,always)
