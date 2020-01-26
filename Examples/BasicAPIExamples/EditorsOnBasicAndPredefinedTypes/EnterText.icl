implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterText

// Enter a text string in a text are

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter text" enterText

main :: Task ()
main = enterText @! ()

enterText :: Task String
enterText
	=   Hint "Enter text:" @>> enterInformation [EnterUsing id (mapEditorWrite Just textArea)]
	>>= \result -> Hint "You entered:" @>> viewInformation [ViewUsing id (ignoreEditorWrites textArea)] result
