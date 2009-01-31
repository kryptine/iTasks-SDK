module movingTask

import StdEnv, iTasks, iDataTrivial
from ProcessDB import :: ProcessStatus, Suspended, Active, Deleted, Finished

// (c) MJP 2007

// Just a scratch file to test the different combinators in the iTasks library

derive gUpd []
derive gForm []

Start world = startEngine [myWorkflow] world

myWorkflow
=	{	name		= "movingTask"
	,	label		= "movingTask"
	,	roles		= []
	,	mainTask	= movingTask myTask
	}

movingTask labeltask
=					newmove
where
	newmove 
	=				[Text "Assign a user to perform the task"] 
					?>> editTask "set" 0 //  0 (\v -> (v >= 0 && v < 5, [Text "illegal user id!"]))
		=>> \who ->	spawnProcess who True labeltask
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
	=						getProcessStatus wid
		=>> \st	->			case st of
								Finished	-> [Text "It is finished"] ?>> OK
								Deleted		-> [Text "It is deleted"]  ?>> OK		
								Active		-> [Text ("User " <+++ /*user <+++ " */"is working on it")]  ?>> NOK		
								Suspended	-> [Text ("It is suspended, user " <+++ /*user <+++ */" was working on it")]  ?>> NOK		
	suspend wid
	=						suspendProcess wid
		=>> \ok ->			if ok
								([Text "workflow is suspended"] ?>> NOK)
								([Text "workflow could not be suspended"] ?>> NOK)
	activate wid
	=						activateProcess wid
		=>> \ok ->			if ok
								([Text "workflow is activated"] ?>> NOK)
								([Text "workflow could not be activated"] ?>> NOK)

	delete wid
	=						deleteProcess wid 
		#>> 				return_V True				

	reassign wid
	=						editTask "who's next ?" 0
		=>> \who ->			setProcessOwner who wid 
		#>> 				return_V False

	waitForIt wid
	=						[Text "Waiting for the result..."]
							?>> waitForProcess wid 
		=>> \(Just res) -> 	deleteProcess wid 
		#>> 				[Text "Finished, the result = ", toHtml res]?>> OK

	
:: QForm = 	{ toComp 			:: String
			, startDate 		:: HtmlDate
			, endDate 			:: HtmlDate
			, estimatedHours 	:: Int
			, description		:: HtmlTextarea
			, price				:: Real 	
			}
:: Person = { firstName			:: String
			 , surname			:: String
			 , dateOfBirth		:: HtmlDate
			 , gender			:: Gender
			 }
:: Gender = Male | Female

derive gForm 	QForm, Person, Gender
derive gUpd 	QForm, Person, Gender
derive gParse 	QForm, Person, Gender
derive gPrint 	QForm, Person, Gender
derive gerda 	QForm, Person, Gender

myTask :: (LabeledTask QForm)
myTask			= ("moving task",editTask "ok1" createDefault)

OK 	= button "OK" True
NOK = button "OK" False



