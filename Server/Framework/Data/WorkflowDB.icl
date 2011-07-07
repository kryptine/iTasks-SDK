implementation module WorkflowDB

import StdClass, StdList, StdBool, StdMisc, Time, Util
from iTasks import serialize, deserialize, defaultStoreFormat

instance WorkflowDB IWorld
where
	getWorkflowDescriptions :: !*IWorld -> (![WorkflowDescription], !*IWorld)
	getWorkflowDescriptions iworld = readWorkflowStore iworld
	
	getAllowedWorkflowDescriptions :: !User !(Maybe UserDetails) !*IWorld -> (![WorkflowDescription], !*IWorld)
	getAllowedWorkflowDescriptions user mbUserDetails iworld
		# (workflows,iworld) = readWorkflowStore iworld
		= (filter (isAllowedWorkflow user mbUserDetails) workflows,iworld)
	
	getWorkflow :: !WorkflowId	!*IWorld -> (!Maybe Workflow, !*IWorld)
	getWorkflow id iworld
		# (mbTask,iworld)		= loadValue (TASK_DB id) iworld
		# (workflows,iworld)	= readWorkflowStore iworld
		# res = case (mbTask,filter (\{workflowId} -> workflowId == id) workflows) of
			(Just task,[{WorkflowDescription|path,roles,description,managerProperties}]) = Just
				{ path				= path
				, roles				= roles
				, task				= task
				, description		= description
				, managerProperties	= managerProperties
				}
			_ = Nothing
		= (res,iworld)
			 
	addWorkflow :: !Workflow !*IWorld -> (!WorkflowDescription,!*IWorld)
	addWorkflow {path,roles,task,description,managerProperties} iworld
		# (wid,iworld)	= getWid iworld
		# descr =		{ workflowId		= wid
						, path				= path
						, roles				= roles
						, description		= description
						, managerProperties	= managerProperties
						}
		# (_,iworld)	= workflowStore (\ws -> ws ++ [descr]) iworld
		# iworld		= storeValue (TASK_DB wid) task iworld
		= (descr,iworld)
	where
		getWid iworld
			# (mbWid,iworld)	= loadValue NEXT_ID_DB iworld
			# wid				= fromMaybe 1 mbWid
			# iworld			= storeValue NEXT_ID_DB (inc wid) iworld
			= (wid,iworld)
			
	lastChange :: !*IWorld -> (!Timestamp,!*IWorld)
	lastChange iworld
		# (mbTs,iworld) = getStoreTimestamp WORKFLOW_DB iworld
		= (fromMaybe (Timestamp 0) mbTs,iworld)

workflowStore :: !([WorkflowDescription] -> [WorkflowDescription]) !*IWorld -> (![WorkflowDescription],!*IWorld)
workflowStore f iworld
	# (workflows,iworld)	= readWorkflowStore iworld
	# workflows				= f workflows
	# iworld				= storeValue WORKFLOW_DB workflows iworld
	= (workflows,iworld)
			
readWorkflowStore :: !*IWorld -> (![WorkflowDescription],!*IWorld)
readWorkflowStore iworld
	# (mbWorkflows,iworld) = loadValue WORKFLOW_DB iworld
	= (fromMaybe [] mbWorkflows,iworld)

WORKFLOW_DB	:== "WorkflowDB"
NEXT_ID_DB	:== "NextWorkflowID"
TASK_DB id	:== "Workflow-" +++ toString id