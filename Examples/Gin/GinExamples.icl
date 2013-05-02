implementation module GinExamples

from iTasks import :: Task

import iTasks.Gin.FlowLibrary
import GinEditor
import iTasks.API.Extensions.Gin.Domain
import iTasks.Gin.Syntax

Start :: *World -> *World
Start world = world // startEngine (manageWorkflows workflows) world
//where
	//workflows = flatten [ ginExamples ]

//ginExamples :: [Workflow]
//ginExamples = [ workflow "Examples/Graphical Editors/GiN Editor" "Create or edit workflows in GiN notation" ginEditor
			  //, simpleEditorWorkflow "Petri net" petriNetORYXEditor
			  //, simpleEditorWorkflow "XMAS" xmasORYXEditor
              //, workflow "Examples/Graphical Editors/Shared Petri net editors" "Two shared Petri net editors" petrinetShareExample
			  //]
			  
//simpleEditorWorkflow :: !String !ORYXEditor -> Workflow
//simpleEditorWorkflow language editor = 
	//workflow ("Examples/Graphical Editors/" +++ language +++ " editor") ("Simple " +++ language +++ " editor")
		//(getConfig >>| (ginInteractionLayout @>> updateInformation ("Simple " +++ language +++ " editor") [] editor >>| quitButton) <<@ fullWidthInteractionLayout)

//petrinetShareExample :: Task Void
//petrinetShareExample = parallel "Petrinet Share Example" petriNetORYXEditor (\_ _ -> Void)
	//[ (BodyTask, \s -> updateSharedInformation "Editor 1" [] (taskListState s) Void >>| quitButton)
	//, (BodyTask, \s -> updateSharedInformation "Editor 2" [] (taskListState s) Void >>| quitButton)
	//]

//quitButton _ = UserActions [(ActionQuit,Just Stop)]

//ginInteractionLayout :: InteractionLayouter
//ginInteractionLayout = \interaction = 
	//case interaction.editorParts of
		//[{UIDef | content = UIEditControl (UIORYXControl _) _}] =
			//({UIDef | hd interaction.editorParts & width = Just (FillParent 1 (FixedMinSize 400))},interaction.UIInteraction.actions)
		//_ 	= defaultInteractionLayout interaction
