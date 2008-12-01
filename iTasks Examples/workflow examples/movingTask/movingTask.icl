module movingTask

import StdEnv, StdiTasks, iDataTrivial

// (c) MJP 2007

// Just a scratch file to test the different combinators in the iTasks library

derive gUpd []
derive gForm []

Start world = startTaskEngine (movingTask go <<@ LSTxtFile) world

movingTask labeltask
=					foreverTask newmove
where
	newmove 
	=				[Text "Assign a user to perform the task"] 
					?>> editTaskPred  0 (\v -> (v >= 0 && v < 5, [Text "illegal user id!"]))
		=>> \who ->	spawnWorkflow who True labeltask
		=>> 		inspect

	inspect wid
	=				chooseTask [Text "Go ahead impatient boss:",BrTag [],BrTag []] 
						[("get status",	getStatus wid)
						,("suspend",	suspend wid)
						,("activate",	activate wid)
						,("re assign",	reassign wid)
						,("delete it",	delete wid)
						,("wait for it",waitForIt wid)
						] 
					=>> \finished -> if finished (return_V Void) (inspect wid)

	getStatus wid
	=						getWorkflowStatus wid
		=>> \st	->			case st of
								WflFinished			-> [Text "It is finished"] ?>> OK
								WflDeleted			-> [Text "It is deleted"]  ?>> OK		
								(WflActive	user)	-> [Text ("User " <+++ user <+++ " is working on it")]  ?>> NOK		
								(WflSuspended user)	-> [Text ("It is suspended, user " <+++ user <+++ " was working on it")]  ?>> NOK		
	suspend wid
	=						suspendWorkflow wid
		=>> \ok ->			if ok
								([Text "workflow is suspended"] ?>> NOK)
								([Text "workflow could not be suspended"] ?>> NOK)
	activate wid
	=						activateWorkflow wid
		=>> \ok ->			if ok
								([Text "workflow is activated"] ?>> NOK)
								([Text "workflow could not be activated"] ?>> NOK)

	delete wid
	=						deleteWorkflow wid 
		#>> 				return_V True				

	reassign wid
	=						editTask "who's next ?" 0
		=>> \who ->			changeWorkflowUser who wid 
		#>> 				return_V False

	waitForIt wid
	=						[Text "Waiting for the result..."]
							?>> waitForWorkflow wid 
		=>> \(Just res) -> 	deleteWorkflow wid 
		#>> 				[Text "Finished, the result = ", toHtml res]?>> OK

	
go			= ("moving task",editTask "ok1" 0 =>> \v1 -> editTask "ok2" v1)

OK 	= button "OK" True
NOK = button "OK" False



