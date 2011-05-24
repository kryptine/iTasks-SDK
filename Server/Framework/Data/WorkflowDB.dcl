definition module WorkflowDB

import Maybe, TSt

class WorkflowDB st
where
	getWorkflowDescriptions			::								!*st -> (![WorkflowDescription], !*st)
	getAllowedWorkflowDescriptions	:: !User !(Maybe UserDetails)	!*st -> (![WorkflowDescription], !*st)
	getWorkflow						:: !WorkflowId					!*st -> (!Maybe Workflow, !*st)
	addWorkflow						:: !Workflow					!*st -> (!WorkflowDescription,!*st)
	
instance WorkflowDB IWorld
instance WorkflowDB TSt