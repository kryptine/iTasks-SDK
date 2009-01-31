module movingTask

import StdEnv, iTasks, iDataTrivial
from ProcessDB import :: ProcessStatus, Suspended, Active, Deleted, Finished

// (c) MJP 2007

// Just a scratch file to test the different combinators in the iTasks library

derive gUpd []
derive gForm []

derive gForm 	QForm, Person, Gender
derive gUpd 	QForm, Person, Gender
derive gParse 	QForm, Person, Gender
derive gPrint 	QForm, Person, Gender
derive gerda 	QForm, Person, Gender

:: QForm = 	{ forCompany 		:: String
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

Start world = startEngine [myWorkflow] world

myWorkflow
=	{	name		= "movingTask"
	,	label		= "movingTask"
	,	roles		= []
	,	mainTask	= movingTask ("Task which can be moved", [Text "Please fill in quotation:"] ?>> fillInForm createDefault)
	}

fillInForm :: QForm -> (Task QForm)
fillInForm form	
= 					editTask "commit" form 
	=>> \form ->	chooseTask [Text "Is everything filled in correctly?", toHtml form] 
						 [("Yes, commit", return_V form) 
						 ,("No", fillInForm form)
						 ] 

OK 	= button "OK" True
NOK = button "OK" False


movingTask labeltask
=					newmove
where
	newmove 
	=				selectUser "Assign a user to perform the task"
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
	=						selectUser "Who is next?"
		=>> \who ->			setProcessOwner who wid 
		#>> 				return_V False

	waitForIt wid
	=						[Text "Waiting for the result..."]
							?>> waitForProcess wid 
		=>> \(Just res) -> 	deleteProcess wid 
		#>> 				[Text "Finished, the result = ", toHtml res]?>> OK

	
selectUser :: !String -> Task Int
selectUser prompt
	= 						getUsersIds
		=>> \userIds ->		getUserNamesTask userIds
		=>> \names ->		chooseTask_pdm [Text prompt] 0
								[(name, return_V userId) \\ userId <- userIds & name <- names]


