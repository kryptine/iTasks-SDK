implementation module BasicAPIExamples.EditorsOnBasicAndPredefinedTypes.EnterDateAndTime

// Enter a list of dates and a times

import iTasks
import iTasks.Extensions.DateTime

wf :: String -> Workflow
wf a = workflow a "Enter a date and a time" enterDateTime

Start :: *World -> *World
Start world
	= doTasks enterDateTime world

enterDateTime :: Task [(Date, Time)]
enterDateTime
	= 	enterInformation "Enter a date and time" []
	>>=	viewInformation "You Entered:" []
