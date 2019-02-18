implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterListOfInt

// Enter a list of Integer numbers

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter a list of Integer numbers" enterListOfInt

main :: Task ()
main = enterListOfInt @! ()

enterListOfInt :: Task [Int]
enterListOfInt
	=   enterInformation "Enter a list of Integer numbers:" []
	>>= viewInformation "You Entered:" []
