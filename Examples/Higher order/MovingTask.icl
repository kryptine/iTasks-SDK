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
	= [ workflow "Examples/Higher order/Moving task" "Demo of a dynamic alteration of task properties" (Description "Suspend,Activate or move a task" @>> movingTask ("Task which can be moved", trivialTask))]

trivialTask :: Task QForm
trivialTask = fillInForm 

fillInForm :: Task QForm
fillInForm	
	= 				enterInformation ("Quote information","Please fill in quotation:") []
	>>= \form ->	showInformation ("Check","Is everything filled in correctly?") [About form] Void
	>?*				[ (ActionNo,	Always fillInForm)
					, (ActionYes,	Always (return form))
					] 

movingTask (label,task)
=					newmove
where
	newmove 
	=				selectUser "Assign a user to perform the task"
		>>= \who ->	appendTopLevelTask {initManagerProperties & worker = who} (task <<@ Description label)
		>>= 		inspect
	
	inspect pref
	=					enterChoice ("Task options","Go ahead impatient boss:") []
							[ getStatus pref <<@ Description "Get status"
							, suspend pref <<@ Description "Suspend"
							, activate pref <<@ Description "Activate"
							, reassign pref <<@ Description "Reassign"
							, delete pref <<@ Description "Delete task"
							//, waitForIt pref <<@ Description "Wait for task"
							]
		>>= \action ->	action
		>>= \finished -> if finished (return Void) (inspect pref)

	getStatus pid
	=						getProcessStatus pid
		>>= \st	->			getProcessOwner pid
		>>= \mbOwner ->		if (isNothing mbOwner) (return ["???"]) (return [toString (fromJust mbOwner)])
		>>= \names ->		case st of
								(Finished,_)		-> showInformation ("Task finished","It is finished") [] True
								(Deleted,_)			-> showInformation ("Task deleted","It is deleted") [] True		
								(Running,Active)	-> showInformation ("Task busy","User " <+++ hd names <+++ " is working on it") [] False		
								(Running,Suspended)	-> showInformation ("Task suspended","It is suspended, user " <+++ hd names <+++ " was working on it") [] False		
	suspend pid
	=						updateManagerProperties pid (\m -> {ManagerProperties | m & status = Suspended})
		>>|					showInformation ("Task suspended","workflow is suspended") [] False
								
	activate pid
	=						updateManagerProperties pid (\m -> {ManagerProperties | m & status = Active})
		>>|					showInformation ("Task activated","workflow is activated") [] False

	delete pid
	=						removeTask pid topLevelTasks 
		>>| 				showInformation ("Task deleted","workflow is deleted") [] True				

	reassign pid
	=						selectUser "Who is next?"
		>>= \who ->			setProcessOwner who pid 
		>>| 				return False
	
	/*waitForIt pid
	=						showInformationA ("Waiting","Waiting for the result...") noActionsMsg ||- wait ("Wait for task", "Wait for an external task to finish") (sharedProcResult pid)
		>>= \(Just res) -> 	deleteProcess pid 
		>>| 				showInformationAbout ("Finished","Finished, the result = ") res 
		>>|					return False*/

	
selectUser :: !String -> Task User
selectUser question = enterSharedChoice question [] users