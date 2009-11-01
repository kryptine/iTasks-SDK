implementation module SimpleChanges

import iTasks
from TSt import :: Change(..)
from TaskTree import :: TaskProperties(..),::TaskWorkerProperties(..),::TaskManagerProperties(..), :: TaskSystemProperties(..), :: TaskProgress


changeExamples :: [Workflow]
changeExamples =
	[ 	{ name		= "Examples/Changes/Change priorities"
		, label		= "Change priorities"
		, roles		= []
		, mainTask	= changePrio
		},
		{ name		= "Examples/Changes/Add warning"
		, label		= "Add warning"
		, roles		= []
		, mainTask	= changeWarningTask
		},
		{ name		= "Examples/Changes/Duplicate task"
		, label		= "Duplicate task"
		, roles		= []
  		, mainTask	= duplicateTask
  		}
	]

//Simple change which will run once and change the priority of all tasks to high
allImportant :: Dynamic
allImportant =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Just {TaskProperties| props & managerProps = {props.managerProps & priority = HighPriority}},Just (enterInformation "Just edit the value"), Just allImportant)

//Add a big red warning message prompt to the running task
addWarning :: String -> Dynamic
addWarning msg = 
	dynamic (change msg) :: A.a: Change a | iTask a
where
	change :: String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change msg props t t0 = (Nothing, Just (((showStickyMessage (redText msg) >>| getDefaultValue) -||- t) <<@ TTVertical), Just (addWarning msg))

	redText msg = [DivTag [StyleAttr "color: red; font-size: 30px;"] [Text msg]]

//This will duplicate a running task n times
duplicate :: Int -> Dynamic
duplicate howMany =
	dynamic (change howMany) :: A.a: Change a | iTask a
where
	change :: Int TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change howMany p t t0 = (Nothing, Just (anyTask [t \\ i <- [1 .. howMany]]), Just (duplicate howMany) )


changePrio :: Task Void
changePrio
	=				chooseProcess "Of which process do you want to change the priority?"			
	>>= \proc -> 	applyChangeToProcess proc allImportant CLTransient


changeWarningTask :: Task Void
changeWarningTask
	=				chooseProcess "What process do you want to change?"			
	>>= \proc ->	applyChangeToProcess proc (addWarning "Warning you are working on a changed task") (CLPersistent "warning")

duplicateTask :: Task Void
duplicateTask
	=				chooseProcess "What process do you want to duplicate?"
	>>= \proc ->	updateInformation "How many times?" 2		
	>>= \times ->	applyChangeToProcess proc (duplicate times) CLTransient

//Utility
chooseProcess :: String -> Task ProcessId
chooseProcess question
	=				getProcesses [Active]
	>>= \procs ->	enterChoice question procs
	>>= \proc ->	return proc.Process.processId