implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterListOfInt

// Enter a list of Integer numbers

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter a list of Integer numbers" enterListOfInt

Start :: *World -> *World
Start world
	= startEngine enterListOfInt world

enterListOfInt :: Task [Int]
enterListOfInt
	=   enterInformation "Enter a list of Integer numbers:" []
	>>= viewInformation "You Entered:" []
