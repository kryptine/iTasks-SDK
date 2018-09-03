implementation module BasicAPIExamples.SequentialExamples.PositiveNumber

module positiveNumber

// Enter a postive Integer number

import iTasks

wf :: String -> Workflow
wf a = workflow "Enter a positive number" positiveNumber

Start :: *World -> *World
Start world
	= doTasks positiveNumber world

positiveNumber :: Task Int
positiveNumber
	= 		enterInformation "Please enter a positive number" []
	>>* 	[ OnAction  ActionOk (ifValue (\n -> n > 0) return)
	        ]
	>>=		viewInformation "Entered number is:" []
