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
		  		{ name		= "Add warning"
		  		, label		= "Add warning"
		  		, roles		= []
		  		, mainTask	= changeWarningTask
		  		},
		  		{ name		= "Duplicate task"
		  		, label		= "Duplicate task"
		  		, roles		= []
		  		, mainTask	= duplicateTask
		  		},
		  		{ name		= "Do something silly"
		  		, label		= "Do something silly"
		  		, roles		= []
		  		, mainTask	= doSilly
		  		},
		  		{ name		= "Do something silly delayed"
		  		, label		= "Do something silly delayed"
		  		, roles		= []
		  		, mainTask	= doSillyDelayed
		  		}
		  	:purchaseExample]

//Very simple task to experiment with changes
doSilly :: Task Void
doSilly = (0 @: ("Root should enter a number", sillyEdit)) -&&- (0 @: ("Root should enter a string", sillyEdit2)) >>= \nr -> showMessageAbout "The result is" nr
where
	sillyEdit :: Task Int
	sillyEdit = editTask "Done" createDefault
	
	sillyEdit2 :: Task String
	sillyEdit2 = editTask "Done" createDefault

doSillyDelayed :: Task Void
doSillyDelayed = requestConfirmation "Start with something silly?" >>| doSilly

//Simple change which will run once and change the priority of all tasks to high
allImportant :: Dynamic
allImportant =
	dynamic change :: A.a: Change a | iData a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iData a
	change props t t0 = (Just {TaskProperties| props & priority = HighPriority},Just (editTask "OK" createDefault), Just allImportant)
	
//Add a big red warning message prompt to the running task
addWarning :: String -> Dynamic
addWarning msg = 
	dynamic (change msg) :: A.a: Change a | iData a
where
	change :: String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iData a
	change msg props t t0 = (Nothing, Just (redText msg ?>> t), Just (addWarning msg))

	redText msg = [DivTag [StyleAttr "color: red; font-size: 30px;"] [Text msg]]

//This will duplicate a running task n times
duplicate :: Int -> Dynamic
duplicate howMany =
	dynamic (change howMany) :: A.a: Change a | iData a
where
	change :: Int TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iData a
	change howMany p t t0 = (Nothing, Just (anyTask [t \\ i <- [1 .. howMany]]), Just (duplicate howMany) )
//Tests

changePrio :: Task Void
changePrio
	=				[Text "Of which process do you want to change the priority?"]
	?>>				chooseProcess
	>>= \proc -> 	applyChangeToProcess proc allImportant CLTransient


changeWarningTask :: Task Void
changeWarningTask
	=				[Text "What process do you want to change?"]
	?>>				chooseProcess
	>>= \proc ->	applyChangeToProcess proc (addWarning "Warning you are working on a changed task") (CLPersistent "warning")

duplicateTask :: Task Void
duplicateTask
	=				[Text "What process do you want to duplicate?"]
	?>>				chooseProcess
	>>= \proc ->	[Text "How many times?"]	
	?>>				editTask "Ok" 2
	>>= \times ->	applyChangeToProcess proc (duplicate times) CLTransient

//Utility
chooseProcess :: Task ProcessId
chooseProcess
	=				getProcesses [Active] True
	>>= \procs ->	selectWithPulldown [toString processId +++ ": " +++ subject \\{Process|processId,properties={TaskProperties|subject}} <- procs] 0
	>>= \index ->	return (procs !! index).Process.processId