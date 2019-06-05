implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.HelloWorld

// Just displaying a message to welcome you.

import iTasks

wf :: String -> Workflow
wf a = workflow a "Hello, World!" helloWorld

main :: Task ()
main = helloWorld @! ()

helloWorld :: Task String
helloWorld
	= viewInformation [ViewWithHint "You have a message from iTasks:"] "Hello, world!"
