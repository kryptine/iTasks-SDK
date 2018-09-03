implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterInteger

// Enter an Integer Number

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter an Integer" enterInt

Start :: *World -> *World
Start world
	= doTasks enterInt world

enterInt :: Task Int
enterInt
	=   enterInformation "Enter an Integer number:" []
	>>= viewInformation "You entered:" []
