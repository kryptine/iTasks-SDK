implementation module GinExamples

import iTasks

import GinFlowLibrary
import GinEditor
import GinDomain
from GinSyntax import ::GModule

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ ginExamples ]

ginExamples :: [Workflow]
ginExamples = [ workflow "Examples/Graphical Editors/Graphical Editor" "Create or edit workflows in Gin notation" ginEditor
			  , simpleEditorWorkflow "Petri net" petriNetORYXEditor
			  , simpleEditorWorkflow "BPMN" bpmnORYXEditor
			  //, simpleEditorWorkflow "Gin" (defaultDetached (ginORYXEditor ["InteractionTasks"] newGinORYXDiagram))
              , workflow "Examples/Graphical Editors/Shared Petri net editors" "Two shared Petri net editors" petrinetShareExample
			  ]
			  
simpleEditorWorkflow :: !String !ORYXEditor -> Workflow
simpleEditorWorkflow language editor = 
	workflow ("Examples/Graphical Editors/" +++ language +++ " editor") ("Simple " +++ language +++ " editor")
		(getConfig >>| updateInformationA ("Simple " +++ language +++ " editor") idView [(ActionQuit,always)] editor <<@ fullWidthInteractiveLayout)

petrinetShareExample = createSharedStore petriNetORYXEditor
	>>= \dbid -> updateSharedInformationA "Editor 1" idView [quitButton] dbid
				 -||
				 updateSharedInformationA "Editor 2" idView [quitButton] dbid

quitButton = (ActionQuit,alwaysShared)
