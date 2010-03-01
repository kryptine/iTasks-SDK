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
  		},
 		{ name		= "Examples/Changes/Inform about task"
		, label		= "Inform about task"
		, roles		= []
  		, mainTask	= informTask
  		}
	]

//Simple change which will run once and change the priority of all tasks to high
allImportant :: Dynamic
allImportant =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Just {TaskProperties| props & managerProps = {props.managerProps & priority = HighPriority}},Nothing, Just allImportant)

//Add a big red warning message prompt to the running task
addWarning :: String -> Dynamic
addWarning msg = 
	dynamic (change msg) :: A.a: Change a | iTask a
where
	change :: String TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change msg props t t0 = (Nothing, Just (((showStickyMessage (redText msg) >>| getDefaultValue) -||- t)), Just (addWarning msg))

redText msg = [DivTag [StyleAttr "color: red; font-size: 30px;"] [Text msg]]

//This will duplicate a running task n times
duplicate :: Int -> Dynamic
duplicate howMany =
	dynamic (change howMany) :: A.a: Change a | iTask a
where
	change :: Int TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change howMany p t t0 = (Nothing, Just (anyTask [t \\ i <- [1 .. howMany]]), Just (duplicate howMany) )

//inform wil infor a user that some process has ended.
inform :: User String -> Dynamic
inform user procName =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskProperties (Task a) (Task a) -> (Maybe TaskProperties, Maybe (Task a), Maybe Dynamic) | iTask a
	change props t t0 = (Nothing, Just (t >>= \res -> spawnProcess user.userName True (showMessageAbout ("Process " +++ procName +++ " ended!") res) >>| return res), Nothing)

changePrio :: Task Void
changePrio
	=				chooseProcess "Of which process do you want to change the priority?"			
	>>= \proc -> 	applyChangeToProcess proc allImportant CLTransient


changeWarningTask :: Task Void
changeWarningTask
	=				enterInformation "Type in warning you want to show to all:"
	>>= \warning ->	chooseProcess "What process do you want to change?"			
	>>= \proc ->	applyChangeToProcess proc (addWarning warning) (CLPersistent "warning")
	

duplicateTask :: Task Void
duplicateTask
	=				chooseProcess "What process do you want to duplicate?"
	>>= \proc ->	updateInformation "How many times?" 2		
	>>= \times ->	applyChangeToProcess proc (duplicate times) CLTransient

informTask :: Task Void
informTask
	=				chooseUser "Select user you want to inform:"
	>>= \user ->	chooseProcess "The result of which process do you want to pass?"
	>>= \procId ->	getProcess procId
	>>= \process ->	applyChangeToProcess procId (inform user (fromJust process).properties.managerProps.subject) CLTransient
	
	


//Utility
chooseProcess :: String -> Task ProcessId
chooseProcess question
	=				getProcessesWithStatus [Active]
	>>= \procs ->	enterChoice question procs
	>>= \proc ->	return proc.Process.processId