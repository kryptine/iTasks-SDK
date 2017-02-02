definition module iTasks.API.Extensions.Admin.WorkflowAdmin
/**
* This extension provides workflows for managing the users of an iTask system.
*/
import iTasks

// A workflow specification
:: Workflow	=
	{ path				:: String					//* a unique name of this workflow
	, roles				:: [String]					//* the roles that are allowed to initate this workflow
	, description		:: String					//* a description of the workflow
	, managerProperties	:: TaskAttributes           //* the initial manager properties of the main task
	, task				:: WorkflowTaskContainer	//* the thread of the main task of the workflow
	}						
:: WorkflowTaskContainer
	= E.a:		WorkflowTask		(Task a)		& iTask a
	| E.a b:	ParamWorkflowTask	(a -> (Task b))	& iTask a & iTask b

:: WorklistRow =
    { taskNr	:: Maybe String
    , title		:: Maybe String
	, priority	:: Maybe String
	, createdBy	:: Maybe String
	, date		:: Maybe String
	, deadline	:: Maybe String
	, createdFor:: Maybe String
	}

derive class iTask WorklistRow

derive class iTask Workflow
		
derive gText	        WorkflowTaskContainer
derive gEditor			WorkflowTaskContainer
derive JSONEncode		WorkflowTaskContainer
derive JSONDecode		WorkflowTaskContainer
derive gDefault			WorkflowTaskContainer
derive gEq				WorkflowTaskContainer

// Available workflows
:: WorkflowFolderLabel :== String

workflows				:: Shared [Workflow]
allowedWorkflows		:: ReadOnlyShared [Workflow]
workflowByPath			:: !String -> Shared Workflow

/**
* Wraps any task as a workflow with no access restrictions
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param The task(container) (with or without parameter)
*/
workflow :: String String w -> Workflow | toWorkflow w
/**
*
* Wraps any task as a workflow that is only available to specified roles
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param A list of roles. The workflow will be available to users with any of the specified roles
* @param The task(container) (with or without parameter)
*/
restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w

class toWorkflow w :: String String [Role] !w -> Workflow

instance toWorkflow (Task a)						| iTask a
instance toWorkflow (WorkflowContainer a)			| iTask a
instance toWorkflow (a -> Task b)					| iTask a & iTask b
instance toWorkflow (ParamWorkflowContainer a b)	| iTask a & iTask b

:: WorkflowContainer a			= Workflow		TaskAttributes (Task a)
:: ParamWorkflowContainer a b	= ParamWorkflow	TaskAttributes (a -> Task b)

/**
* Default workflow management task.
* This task allows users to manage a catalogue of task definitions
* and let's them create instances of these tasks and work on instances.
*/
manageWorkflows :: ![Workflow] ->  Task ()

manageWorklist :: ![Workflow] -> Task ()

loginAndManageWorkList :: !String ![Workflow] -> Task ()


/**
* Dynamically adds a workflow to the system.
*
* @param Workflow: The workflow to add
* @return The description of the added workflow
* 
* @gin False
*/
addWorkflows :: ![Workflow] -> Task [Workflow]

isAllowedWorkflow :: !User !Workflow -> Bool

//Service tasks
viewTaskList	:: Task [TaskListItem ()]
viewTask		:: Task AttachmentStatus

//The default external services
externalTaskInterface :: [PublishedTask]

appendOnce :: TaskId (Task a) (SharedTaskList a) -> Task () | iTask a