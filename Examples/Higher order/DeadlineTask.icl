implementation module DeadlineTask

import iTasks, iDataTrivial
import StdClass, StdEnum, StdList

// (c) MJP 2007

// One can select a user to whom a task is delegated
// This user will get a certain amount of time to finish the task
// If the task is not finished on time, the task will be shipped back to the original user who has to do it instead
// It is also possible that the user becomes impatient and he can cancel the delegated task even though the deadline is not reached

npersons = 6

deadlineTaskExample :: [Workflow]
deadlineTaskExample
= [ { name	= "Examples/Higher order/Deadline task"
	, label	= "Do task before deadline"
	, roles	= []
	, mainTask	= deadline trivialTask >>| return Void
	}
  ] 

trivialTask :: Task Int
trivialTask = editTask "OK" 0 <| (\n -> if (n <= 42) (False,[Text ("Error " <+++ n <+++ " should be larger than 42")]) (True,[]))

deadline :: (Task a) -> Task a | iData a
deadline task
=					[Text "Choose person you want to delegate work to:",BrTag [],BrTag []] 
					?>>	editTask "Set" (HtmlSelect (map (\i -> (toString i,toString i)) [1..npersons - 1]) (toString 1)) 
	>>= \(HtmlSelect _ whom) ->	[Text "How long do you want to wait?",BrTag [],BrTag []] 
					?>>	editTask "SetTime" (HtmlTime 0 0 0) 
	>>= \time ->	[Text "Cancel delegated work if you are getting impatient:",BrTag [],BrTag []] 
					?>> (delegateTask (toInt whom) time task
					-||-
					buttonTask "Cancel" (return Nothing))
	>>= 			CheckDone
where
	CheckDone (Just value)
	=	[Text ("Result of task: " +++ printToString value),BrTag [],BrTag []] 
			?>>	buttonTask "OK" (return value)
	CheckDone nothing
	=	[Text "Task expired or canceled, you have to do it yourself!",BrTag [],BrTag []] 
			?>>	buttonTask "OK" task

	delegateTask who time task
	= who  @: ("Timed Task",mytask)
	where
		mytask
		=			(			waitForTimerTask time 
					>>| 		return Nothing)									// return nothing
		 			-||-
		  			(			[Text ("You have to complete the task in " <+++ time <+++ " time"),BrTag [],BrTag []] 	// tell deadline
								?>> task 
					>>= \v -> 	return (Just v))				// do task and return its result
