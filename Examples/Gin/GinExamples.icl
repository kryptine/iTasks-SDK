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
		(getConfig >>| (updateInformation ("Simple " +++ language +++ " editor") [] editor >>+ quitButton) <<@ fullWidthInteractionLayout)

petrinetShareExample :: Task Void
petrinetShareExample = parallel "Petrinet Share Example" petriNetORYXEditor (\_ _ -> Void)
	[ (BodyTask, \s -> updateSharedInformation "Editor 1" [] (taskListState s) Void >>+ quitButton)
	, (BodyTask, \s -> updateSharedInformation "Editor 2" [] (taskListState s) Void >>+ quitButton)
	]

quitButton _ = UserActions [(ActionQuit,Just Stop)]
