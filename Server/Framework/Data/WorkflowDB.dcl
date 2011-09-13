definition module WorkflowDB

from Maybe			import :: Maybe
from SystemTypes	import :: User, :: UserDetails, :: IWorld
from Time			import :: Timestamp
from WorkflowAdmin	import :: Workflow, :: WorkflowId, :: WorkflowDescription
class WorkflowDB st
where
	getWorkflowDescriptions			::								!*st -> (![WorkflowDescription], !*st)
	getAllowedWorkflowDescriptions	:: !User !(Maybe UserDetails)	!*st -> (![WorkflowDescription], !*st)
	getWorkflow						:: !WorkflowId					!*st -> (!Maybe Workflow, !*st)
	addWorkflow						:: !Workflow					!*st -> (!WorkflowDescription,!*st)
	/**
	* Gets the timestamp of the last change of the workflow database.
	*
	* @param A unique database handle
	*
	* @return The timestamp
	* @retrun The database handle 
	*/
	lastChange :: !*st -> (!Timestamp,!*st)
	
instance WorkflowDB IWorld