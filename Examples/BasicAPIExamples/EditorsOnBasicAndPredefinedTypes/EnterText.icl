implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterText

// Enter a text string in a text are

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter text" enterText

Start :: *World -> *World
Start world
	= startEngine enterText world

enterText :: Task String
enterText
	=   enterInformation "Enter text:" [EnterUsing id textArea]
	>>= viewInformation "You entered:" [ViewUsing id textArea]
