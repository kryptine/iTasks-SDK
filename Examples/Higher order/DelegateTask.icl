implementation module DelegateTask

import iTasks, iDataTrivial

// (c) 2007 MJP

// Quite a difficult workflow exercise given to me by Erik Zuurbier (Thanks Erik).
// First a set of person id's is made to which a task can be delegated
// The task is actually shipped to the first person who accepts the task
// That person can stop the task whenever he wants
// Now again everybody in the set is asked again to accept the task
// The one who accepts can *continue* the work already done so far
// This process can be repeated as many times one likes until finally the task is finished

npersons = 5

:: UserID :== Int

delegateTaskExample :: [Workflow]
delegateTaskExample
= []
/*
= [	{ name	= "Examples/Higher order tasks/Delegate task"
	, label = "Delegated task"
	, roles	= []
	, mainTask = displayHtml foreverTask (delegateTask trivialTask (HtmlTime 0 3 0))
	}
  ]
*/
trivialTask :: Task Int
trivialTask =			editTask "Done1" 0 
			>>= \v1 ->	editTask "Done2" 0 
			>>= \v2 ->	editTask "Done3" 0 
			>>= \v3 -> 	editTask "Result" (v1 + v2 + v3)

/*
delegateTask :: (Task a) HtmlTime -> (Task a) | iData a
delegateTask task time 
=					[Text "Choose persons you want to delegate work to:",BrTag [],BrTag []] 
					?>>	determineSet [] 
	=>> \people -> 	delegateToSomeone task people 
	=>> \result -> 	[Text "Result: ", toHtml result] 
					?>> editTask "OK" Void 
	=>> \_ ->		return_V result
where
	delegateToSomeone :: (Task a) [UserID] -> Task a | iData a
	delegateToSomeone task people = newTask "delegateToSet" doDelegate
	where 
		doDelegate						
		 =								orTasks [("Waiting for " <+++ who, who @:: buttonTask "I Will Do It" (return_V who)) \\ who <- people]	
		 	=>> \who ->					who @:: stopTask2 who -!> task 
		 	=>> \(stopped,task) -> 	if (isJust stopped) (delegateToSomeone task people) task   
	
		stopTask 		= buttonTask "Stop" (return_V True)					  			

		stopTask2 who	= stopTask -||- (0 @:> ("Stop worker " <+++ who,stopTask))						// alternative : now also user 0 can stop the work
//		stopTask3		= stopTask -||- timerStop time -||- (0 @::> stopTask)	// alternative : now also a timer can stop the work	

		timerStop time	= waitForTimerTask time #>> return_V True
*/
determineSet :: [UserID] -> Task [UserID]
determineSet people = newTask "determineSet" determineSet`
where
	determineSet`	
	=					chooseTask [Text ("Current set:" +++ print people)] 
						[("Add Person", cancelTask choosePerson)
						,("Finished", return_V Nothing)
						]						
		=>> \result -> 	case result of
							(Just new)  -> determineSet (sort (removeDup [new:people])) 
							Nothing		-> if (people == []) (determineSet people) (return_V people)

	choosePerson =					editTask "Set" (HtmlSelect [(toString i,toString i) \\ i <- [1..npersons - 1]] (toString 1))
					=>> \(HtmlSelect options val) ->	return_V (Just (toInt val))

	cancelTask task = task -||- buttonTask "Cancel" (return_V createDefault)
	
	print []     = ""
	print [x:xs] = toString x +++ " " +++ print xs
