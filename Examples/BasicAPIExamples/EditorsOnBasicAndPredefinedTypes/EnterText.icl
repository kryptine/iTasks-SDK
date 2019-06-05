implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterText

// Enter a text string in a text are

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter text" enterText

main :: Task ()
main = enterText @! ()

enterText :: Task String
enterText
	=   enterInformation [EnterWithHint "Enter text:", EnterUsing id textArea]
	>>= viewInformation [ViewWithHint "You entered:", ViewUsing id textArea]
