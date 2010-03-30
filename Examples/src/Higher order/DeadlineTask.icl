implementation module DeadlineTask

import iTasks
import CommonDomain
import StdClass, StdEnum, StdList

// (c) MJP 2007

// One can select a user to whom a task is delegated
// This user will get a certain amount of time to finish the task
// If the task is not finished on time, the task will be shipped back to the original user who has to do it instead
// It is also possible that the user becomes impatient and he can cancel the delegated task even though the deadline is not reached

npersons = 6

deadlineTaskExample :: [Workflow]
deadlineTaskExample
= [ { Workflow
	| name	= "Examples/Higher order/Deadline task"
	, label	= "Do task before deadline"
	, roles	= []
	, mainTask	= deadline trivialTask >>| return Void
	}
  ] 

trivialTask :: Task Int
trivialTask = enterInformation "Enter a number larger than 42" <| (\n -> if (n <= 42) (False,[Text ("Error " <+++ n <+++ " should be larger than 42")]) (True,[]))

deadline :: (Task a) -> Task a | iTask a
deadline task
=					chooseUser "Choose person you want to delegate work to:"
	>>= \whom ->	enterInformation "How long do you want to wait?" 
	>>= \time ->	(delegateTask whom.User.userName time task)
					-||-
					(showMessage "Cancel delegated work if you are getting impatient:" >>| return Nothing)
	>>= 			checkDone
where
	checkDone (Just value)
		= showMessageAbout "Result of task:" value >>| return value
	checkDone Nothing
		= showMessage "Task expired or canceled, you have to do it yourself!" >>| task

	delegateTask who time task
	= who  @: ("Timed Task",mytask)
	where
		mytask
		=			// wait for timeout and return nothing
					( waitForTimer time
					  >>| return Nothing
					)									
		 			-||-
		 			// do task and return its result
		  			( showStickyMessage ("You have to complete the task in " <+++ time <+++ " time")
		  			  ||- task 
					  >>= \v -> return (Just v)
					)				
