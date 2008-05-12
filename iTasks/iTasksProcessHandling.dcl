definition module iTasksProcessHandling

// *********************************************************************************************************************************
// This module contains iTask combinators for creating iTask workflow processes
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdOverloaded, iTasksHandler

derive gForm 	Wid
derive gUpd 	Wid
derive gParse 	Wid
derive gPrint 	Wid
derive gerda 	Wid
derive read 	Wid
derive write 	Wid

// iTask workflow processes types:

:: Wid a											// reference to a workflow process

:: WorkflowStatus	= WflActive	UserId				// iTask workflow process is still being processed by indicated user
					| WflSuspended UserId			// it is assigned to indicated user but it is (temporally) suspended
					| WflFinished					// it is finshed
					| WflDeleted					// it does not exist anymore because it is deleted

instance ==			WorkflowStatus

/* iTask Workflow process management:
spawnWorkflow 		:: spawn an iTask workflow as a new separate process, Wid is a handle to that process, bool indicates whether it is active or suspended 
waitForWorkflow		:: wait until the indicated process is finished and obtain the resulting value; returns Nothing when process is deleted
getWorkflowStatus 	:: get status of workflow
deleteWorkflow 		:: delete iTask workflow; returns False if workflow does not exist anymore
suspendWorkflow 	:: suspend iTask workflow, all corresponding tasks will vanish temporally; returns False if workflow does not exist anymore
activateWorkflow 	:: activate the iTask workflow again; returns False if workflow does not exist anymore
changeWorkflowUser :: transfer the workflow task to the indicated user; returns False if workflow does not exist anymore

suspendMe 			:: suspend current workflow process; no effect on start task
deleteMe 			:: delete current workflow process;  no effect on start task
*/

spawnWorkflow 		:: !UserId !Bool !(LabeledTask a) 					-> Task (Wid a) 	| iData a
waitForWorkflow 	:: !(Wid a) 										-> Task (Maybe a )	| iData a
getWorkflowStatus 	:: !(Wid a) 										-> Task WorkflowStatus
activateWorkflow 	:: !(Wid a) 										-> Task Bool 	
suspendWorkflow 	:: !(Wid a) 										-> Task Bool 		
deleteWorkflow 		:: !(Wid a) 										-> Task Bool 		
changeWorkflowUser	:: !UserId !(Wid a) 								-> Task Bool 

suspendMe 			:: (Task Void)
deleteMe 			:: (Task Void)



// internally used...

showWorkflows 		:: !Bool !*TSt -> ([BodyTag],*TSt)
scheduleWorkflows 	:: !(Task a) -> (Task a) | iData a
