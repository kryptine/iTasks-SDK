implementation module MovingTask

import iTasks, iDataTrivial
from ProcessDB import :: ProcessStatus(..)

derive gForm 	QForm, Person, Gender
derive gUpd 	QForm, Person, Gender
derive gParse 	QForm, Person, Gender
derive gPrint 	QForm, Person, Gender

:: QForm = 	{ forCompany 		:: String
			, startDate 		:: HtmlDate
			, endDate 			:: HtmlDate
			, estimatedHours 	:: Int
			, description		:: HtmlTextarea
			, price				:: HtmlCurrency 	
			}
:: Person = { firstName			:: String
			, surname			:: String
			, dateOfBirth		:: HtmlDate
			, gender			:: Gender
			}
:: Gender = Male | Female

movingTaskExample :: [Workflow]
movingTaskExample
= [ {	name		= "Examples/Higher order/Moving task"
	,	label		= "Suspend,Activate or move a task"
	,	roles		= []
	,	mainTask	= movingTask ("Task which can be moved", trivialTask )
	}
  ]

trivialTask :: Task QForm
trivialTask = [Text "Please fill in quotation:"] ?>> fillInForm createDefault

fillInForm :: QForm -> Task QForm
fillInForm form	
= 					editTask "commit" form 
	=>> \form ->	chooseTask [Text "Is everything filled in correctly?", toHtml form] 
						 [("Yes, commit", return form) 
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
					=>> \finished -> if finished (return Void) (inspect wid)

	getStatus wid
	=						getProcessStatus wid
		=>> \st	->			getProcessOwner wid
		=>> \mbOwner ->		if (isNothing mbOwner) (return ["???"]) (getUserNames [(fromJust mbOwner)])
		=>> \names ->		case st of
								Finished	-> [Text "It is finished"] ?>> OK
								Deleted		-> [Text "It is deleted"]  ?>> OK		
								Active		-> [Text ("User " <+++ hd names <+++ " is working on it")]  ?>> NOK		
								Suspended	-> [Text ("It is suspended, user " <+++ hd names <+++ " was working on it")]  ?>> NOK		
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
		#>> 				return True				

	reassign wid
	=						selectUser "Who is next?"
		=>> \who ->			setProcessOwner who wid 
		#>> 				return False

	waitForIt wid
	=						[Text "Waiting for the result..."]
							?>> waitForProcess wid 
		=>> \(Just res) -> 	deleteProcess wid 
		#>> 				[Text "Finished, the result = ", toHtml res]?>> OK

	
selectUser :: !String -> Task Int
selectUser prompt
	= 						getUsers
		=>> \users ->		chooseTask_pdm [Text prompt] 0
								[(name, return userId) \\ (userId,name) <- users]

