implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.HelloWorld

// Just displaying a message to welcome you.

import iTasks

wf :: String -> Workflow
wf a = workflow a "Hello, World!" helloWorld

main :: Task ()
main = helloWorld @! ()

helloWorld :: Task String
helloWorld
	= Hint "You have a message from iTasks:" @>> viewInformation [] "Hello, world!"
