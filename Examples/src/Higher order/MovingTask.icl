implementation module MovingTask

import iTasks
import CommonDomain

derive gVisualize 	QForm, Person, Gender
derive gUpdate	 	QForm, Person, Gender
derive gParse 		QForm, Person, Gender
derive gPrint 		QForm, Person, Gender

derive bimap (,), Maybe

:: QForm = 	{ forCompany 		:: String
			, startDate 		:: Date
			, endDate 			:: Date
			, estimatedHours 	:: Int
			, description		:: Note
			, price				:: Currency	
			}
:: Person = { firstName			:: String
			, surname			:: String
			, dateOfBirth		:: Date
			, gender			:: Gender
			}
:: Gender = Male | Female

movingTaskExample :: [Workflow]
movingTaskExample
= [ {Workflow
	|	name		= "Examples/Higher order/Moving task"
	,	roles		= []
	,	mainTask	= "Suspend,Activate or move a task" @>> movingTask ("Task which can be moved", trivialTask )
	}
  ]

trivialTask :: Task QForm
trivialTask = getDefaultValue >>= fillInForm

fillInForm :: QForm -> Task QForm
fillInForm form	
	= 				updateInformation "Please fill in quotation:" form 
	>>= \form ->	requestConfirmationAbout "Is everything filled in correctly?" form
	>>= \ok	->		if ok (return form) (fillInForm form) 

movingTask (label,task)
=					newmove
where
	newmove 
	=				selectUser "Assign a user to perform the task"
		>>= \who ->	spawnProcess who True (task <<@ label)
		>>= 		inspect

	inspect wid
	=					enterChoice "Go ahead impatient boss:" 
							[ getStatus wid <<@ "Get status"
							, suspend wid <<@ "Suspend"
							, activate wid <<@ "Activate"
							, reassign wid <<@ "Reassign"
							, delete wid <<@ "Delete task"
							, waitForIt wid <<@ "Wait for task"
							]
		>>= \action ->	action
		>>= \finished -> if finished (return Void) (inspect wid)

	getStatus wid
	=						getProcessStatus wid
		>>= \st	->			getProcessOwner wid
		>>= \mbOwner ->		if (isNothing mbOwner) (return ["???"]) (return [toString (fromJust mbOwner)])
		>>= \names ->		case st of
								Finished	-> showMessage "It is finished" >>| return True
								Deleted		-> showMessage "It is deleted" >>| return True		
								Active		-> showMessage ("User " <+++ hd names <+++ " is working on it") >>| return False		
								Suspended	-> showMessage ("It is suspended, user " <+++ hd names <+++ " was working on it") >>| return False		
	suspend wid
	=						suspendProcess wid
		>>|					showMessage "workflow is suspended"
		>>|					return False
								
	activate wid
	=						activateProcess wid
		>>|					showMessage "workflow is activated"
		>>|					return False

	delete wid
	=						deleteProcess wid 
		>>| 				return True				

	reassign wid
	=						selectUser "Who is next?"
		>>= \who ->			setProcessOwner who wid 
		>>| 				return False

	waitForIt wid
	=						showStickyMessage "Waiting for the result..." ||- waitForProcess wid
		>>= \(Just res) -> 	deleteProcess wid 
		>>| 				showMessageAbout "Finished, the result = " res
		>>|					return False

	
selectUser :: !String -> Task UserName
selectUser question
	= 						getUsers
		>>= \users ->		enterChoice question users
		>>= \user ->		return (toUserName user)

