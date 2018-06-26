module BasicAPIExamples

import iTasks
import qualified BasicAPIExamples.EditorsOnCustomTypes.EnterPerson
import qualified BasicAPIExamples.EditorsOnCustomTypes.EnterFamilyTree
import qualified BasicAPIExamples.InteractionUsingShares.CurrentDateAndTime
import qualified BasicAPIExamples.InteractionUsingShares.BrowseAndViewGoogleMap
import qualified BasicAPIExamples.InteractionUsingShares.SharedNoteAsList
import qualified BasicAPIExamples.InteractionUsingShares.UpdateSharedPersonsAndView
import qualified BasicAPIExamples.InteractionUsingShares.SharedNotes
import qualified BasicAPIExamples.MultiUserExamples.Chat
import qualified BasicAPIExamples.MultiUserExamples.MeetingDate
import qualified BasicAPIExamples.MultiUserExamples.OptionsChat
import qualified BasicAPIExamples.ParallelExamples.TinyTextEditor
import qualified BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterInteger
import qualified BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterText
import qualified BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterListOfInt
import qualified BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.HelloWorld
import qualified BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.GoogleMap
import qualified BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterDateAndTime
import qualified BasicAPIExamples.SequentialExamples.CalculateSumStepwiseAndBack
import qualified BasicAPIExamples.SequentialExamples.CalculateSumStepwise
import qualified BasicAPIExamples.SequentialExamples.Calculator
import qualified BasicAPIExamples.SequentialExamples.CalculateSumInShare
import qualified BasicAPIExamples.SequentialExamples.Palindrome
import qualified BasicAPIExamples.SequentialExamples.CoffeeMachine
import qualified BasicAPIExamples.SequentialExamples.EditPerson1by1
import qualified BasicAPIExamples.InteractionWithTheSystem.RunProcess

Start :: *World -> *World
Start world = startEngine
	[ publish "/" (\_->loginAndManageWorkList title basicAPIExamples <<@ ApplyLayout (setUIAttributes (titleAttr title)))
	] world
where
	title = "iTasks Example Collection"

basicAPIExamples :: [Workflow]
basicAPIExamples =
	['BasicAPIExamples.EditorsOnCustomTypes.EnterPerson'.wf " Basic API Examples/Editors On Custom Types/Enter Person"
	,'BasicAPIExamples.EditorsOnCustomTypes.EnterFamilyTree'.wf " Basic API Examples/Editors On Custom Types/Enter Family Tree"
	,'BasicAPIExamples.InteractionUsingShares.CurrentDateAndTime'.wf " Basic API Examples/Interaction Using Shares/Current Date And Time"
	,'BasicAPIExamples.InteractionUsingShares.BrowseAndViewGoogleMap'.wf " Basic API Examples/Interaction Using Shares/Browse And View Google Map"
	,'BasicAPIExamples.InteractionUsingShares.SharedNoteAsList'.wf " Basic API Examples/Interaction Using Shares/Shared Note As List"
	,'BasicAPIExamples.InteractionUsingShares.UpdateSharedPersonsAndView'.wf " Basic API Examples/Interaction Using Shares/Update Shared Persons And View"
	,'BasicAPIExamples.InteractionUsingShares.SharedNotes'.wf " Basic API Examples/Interaction Using Shares/Shared Notes"
	,'BasicAPIExamples.MultiUserExamples.Chat'.wf " Basic API Examples/Multi User Examples/Chat"
	,'BasicAPIExamples.MultiUserExamples.MeetingDate'.wf " Basic API Examples/Multi User Examples/Meeting Date"
	,'BasicAPIExamples.MultiUserExamples.OptionsChat'.wf " Basic API Examples/Multi User Examples/Options Chat"
	,'BasicAPIExamples.ParallelExamples.TinyTextEditor'.wf " Basic API Examples/Parallel Examples/Tiny Text Editor"
	,'BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterInteger'.wf " Basic API Examples/Editors On Basic And Predefined Types/Enter Integer"
	,'BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterText'.wf " Basic API Examples/Editors On Basic And Predefined Types/Enter Text"
	,'BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterListOfInt'.wf " Basic API Examples/Editors On Basic And Predefined Types/Enter List Of Int"
	,'BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.HelloWorld'.wf " Basic API Examples/Editors On Basic And Predefined Types/Hello World"
	,'BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.GoogleMap'.wf " Basic API Examples/Editors On Basic And Predefined Types/Google Map"
	,'BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterDateAndTime'.wf " Basic API Examples/Editors On Basic And Predefined Types/Enter Date And Time"
	,'BasicAPIExamples.SequentialExamples.CalculateSumStepwiseAndBack'.wf " Basic API Examples/Sequential Examples/Calculate Sum Stepwise And Back"
	,'BasicAPIExamples.SequentialExamples.CalculateSumStepwise'.wf " Basic API Examples/Sequential Examples/Calculate Sum Stepwise"
	,'BasicAPIExamples.SequentialExamples.Calculator'.wf " Basic API Examples/Sequential Examples/Calculator"
	,'BasicAPIExamples.SequentialExamples.CalculateSumInShare'.wf " Basic API Examples/Sequential Examples/Calculate Sum In Share"
	,'BasicAPIExamples.SequentialExamples.Palindrome'.wf " Basic API Examples/Sequential Examples/Palindrome"
	,'BasicAPIExamples.SequentialExamples.CoffeeMachine'.wf " Basic API Examples/Sequential Examples/Coffee Machine"
	,'BasicAPIExamples.SequentialExamples.EditPerson1by1'.wf " Basic API Examples/Sequential Examples/Edit Person1by1"
	,'BasicAPIExamples.InteractionWithTheSystem.RunProcess'.wf " Basic API Examples/Interaction With The System/Run Process"
	]
