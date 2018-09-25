implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterInteger

// Enter an Integer Number

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter an Integer" enterInt

main :: Task ()
main = enterInt @! () 

enterInt :: Task Int
enterInt
	=   enterInformation "Enter an Integer number:" []
	>>= viewInformation "You entered:" []
