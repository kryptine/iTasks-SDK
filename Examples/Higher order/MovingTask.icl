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
	= [ workflow "Examples/Higher order/Moving task" "Demo of a dynamic alteration of task properties" (Title "Suspend,Activate or move a task" @>> movingTask ("Task which can be moved", trivialTask))]

trivialTask :: Task QForm
trivialTask = fillInForm 

fillInForm :: Task QForm
fillInForm	
	= 				enterInformation ("Quote information","Please fill in quotation:") 
	>>= \form ->	requestConfirmationAbout ("Check","Is everything filled in correctly?") form
	>>= \ok	->		if ok (return form) fillInForm 

movingTask (label,task)
=					newmove
where
	newmove 
	=				selectUser "Assign a user to perform the task"
		>>= \who ->	spawnProcess False {initManagerProperties & worker = who} noMenu (task <<@ Title label)
		>>= 		inspect

	inspect pref
	=					enterChoice ("Task options","Go ahead impatient boss:")
							[ getStatus pref <<@ Title "Get status"
							, suspend pref <<@ Title "Suspend"
							, activate pref <<@ Title "Activate"
							, reassign pref <<@ Title "Reassign"
							, delete pref <<@ Title "Delete task"
							, waitForIt pref <<@ Title "Wait for task"
							]
		>>= \action ->	action
		>>= \finished -> if finished (return Void) (inspect pref)

	getStatus (pid,_,_)
	=						getProcessStatus pid
		>>= \st	->			getProcessOwner pid
		>>= \mbOwner ->		if (isNothing mbOwner) (return ["???"]) (return [toString (fromJust mbOwner)])
		>>= \names ->		case st of
								(Finished,_)		-> showMessage ("Task finished","It is finished") True
								(Deleted,_)			-> showMessage ("Task deleted","It is deleted") True		
								(Running,Active)	-> showMessage ("Task busy","User " <+++ hd names <+++ " is working on it") False		
								(Running,Suspended)	-> showMessage ("Task suspended","It is suspended, user " <+++ hd names <+++ " was working on it") False		
	suspend (pid,_,_)
	=						updateManagerProperties pid (\m -> {ManagerProperties | m & status = Suspended})
		>>|					showMessage ("Task suspended","workflow is suspended") False
								
	activate (pid,_,_)
	=						updateManagerProperties pid (\m -> {ManagerProperties | m & status = Active})
		>>|					showMessage ("Task activated","workflow is activated") False

	delete (pid,_,_)
	=						killProcess pid 
		>>| 				showMessage ("Task deleted","workflow is deleted") True				

	reassign (pid,_,_)
	=						selectUser "Who is next?"
		>>= \who ->			setProcessOwner who pid 
		>>| 				return False

	waitForIt (pid,_,sharedRes)
	=						showMessageA ("Waiting","Waiting for the result...") [] Void ||- wait ("Wait for task", "Wait for an external task to finish") sharedRes
		>>= \(Just res) -> 	deleteProcess pid 
		>>| 				showMessageAbout ("Finished","Finished, the result = ") res 
		>>|					return False

	
selectUser :: !String -> Task User
selectUser question = enterSharedChoice question users