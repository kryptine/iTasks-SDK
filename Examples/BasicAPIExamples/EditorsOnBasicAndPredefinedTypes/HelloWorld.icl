implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.HelloWorld

// Just displaying a message to welcome you.

import iTasks

wf :: String -> Workflow
wf a = workflow a "Hello, World!" helloWorld

Start :: *World -> *World
Start world
	= doTasks helloWorld world

helloWorld :: Task String
helloWorld
	= viewInformation "You have a message from iTasks:" [] "Hello, world!"
