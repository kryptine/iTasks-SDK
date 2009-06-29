module ChangeTest

import iTasks
from TSt import :: Change(..)
from TaskTree import :: TaskProperties(..), :: TaskProgress

import Purchase


Start :: *World -> *World
Start world = startEngine tests world
where
	tests = [ 	{ name		= "Change priorities"
		  		, label		= "Change priorities"
		  		, roles		= []
		  		, mainTask	= changePrio
		  		},
		  		{ name		= "Do something silly"
		  		, label		= "Do something silly"
		  		, roles		= []
		  		, mainTask	= doSilly
		  		}
		  	: purchaseExample
		  	]

//Very simple task to experiment with changes
doSilly :: Task Void
doSilly = 0 @: ("SILLY", sillyEdit) >>| ok
where
	sillyEdit :: Task Int
	sillyEdit = editTask "DOH" createDefault



//Simple change which will run once and change the priority of all tasks to high

allImportant :: Change Int
allImportant =
	Change (\props t t0 -> (Just {TaskProperties| props & priority = HighPriority}, Nothing, Nothing))

changePrio :: Task Void
changePrio
	=				[Text "What process do you want to change?"]
	?>>				chooseProcess
	>>= \proc -> 	applyChangeToProcess proc "allImportant" allImportant

chooseProcess :: Task ProcessId
chooseProcess
	=				getProcesses [Active] True
	>>= \procs ->	selectWithPulldown [toString processId +++ ": " +++ subject \\{Process|processId,properties={TaskProperties|subject}} <- procs] 0
	>>= \index ->	return (procs !! index).Process.processId