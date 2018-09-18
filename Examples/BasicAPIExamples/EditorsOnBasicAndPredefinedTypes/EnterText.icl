implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterText

// Enter a text string in a text are

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter text" enterText

main :: Task ()
main = enterText @! ()

enterText :: Task String
enterText
	=   enterInformation "Enter text:" [EnterUsing id textArea]
	>>= viewInformation "You entered:" [ViewUsing id textArea]
