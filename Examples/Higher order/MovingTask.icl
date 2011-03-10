implementation module MovingTask

import iTasks

derive class iTask	QForm, Person, Gender
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
	= [ workflow "Examples/Higher order/Moving task" "Demo of a dynamic alteration of task properties" (Title "Suspend,Activate or move a task" @>> movingTask ("Task which can be moved", trivialTask ))]

trivialTask :: Task QForm
trivialTask = getDefaultValue >>= fillInForm

fillInForm :: QForm -> Task QForm
fillInForm form	
	= 				updateInformation ("Quote information","Please fill in quotation:") form 
	>>= \form ->	requestConfirmationAbout ("Check","Is everything filled in correctly?") form
	>>= \ok	->		if ok (return form) (fillInForm form) 

movingTask (label,task)
=					newmove
where
	newmove 
	=				selectUser "Assign a user to perform the task"
		>>= \who ->	spawnProcess True False (container (DetachedTask {initManagerProperties & worker = who} noMenu) task <<@ Title label)
		>>= 		inspect

	inspect wid
	=					enterChoice ("Task options","Go ahead impatient boss:")
							[ getStatus wid <<@ Title "Get status"
							, suspend wid <<@ Title "Suspend"
							, activate wid <<@ Title "Activate"
							, reassign wid <<@ Title "Reassign"
							, delete wid <<@ Title "Delete task"
							, waitForIt wid <<@ Title "Wait for task"
							]
		>>= \action ->	action
		>>= \finished -> if finished (return Void) (inspect wid)

	getStatus wid
	=						getProcessStatus wid
		>>= \st	->			getProcessOwner wid
		>>= \mbOwner ->		if (isNothing mbOwner) (return ["???"]) (return [toString (fromJust mbOwner)])
		>>= \names ->		case st of
								Finished	-> showMessage ("Task finished","It is finished") True
								Deleted		-> showMessage ("Task deleted","It is deleted") True		
								Active		-> showMessage ("Task busy","User " <+++ hd names <+++ " is working on it") False		
								Suspended	-> showMessage ("Task suspended","It is suspended, user " <+++ hd names <+++ " was working on it") False		
	suspend wid
	=						suspendProcess wid
		>>|					showMessage ("Task suspended","workflow is suspended") False
								
	activate wid
	=						activateProcess wid
		>>|					showMessage ("Task activated","workflow is activated") False

	delete wid
	=						killProcess wid 
		>>| 				showMessage ("Task deleted","workflow is deleted") True				

	reassign wid
	=						selectUser "Who is next?"
		>>= \who ->			setProcessOwner who wid 
		>>| 				return False

	waitForIt wid
	=						showStickyMessage ("Waiting","Waiting for the result...") Void ||- waitForProcess True wid
		>>= \(Just res) -> 	deleteProcess wid 
		>>| 				showMessageAbout ("Finished","Finished, the result = ") res 
		>>|					return False

	
selectUser :: !String -> Task User
selectUser question
	= 						getUsers
		>>= \users ->		enterChoice question users

