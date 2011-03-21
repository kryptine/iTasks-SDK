implementation module WorkflowStarter

import iTasks, TSt, Text
from UserDB import qualified class UserDB(..)
from UserDB import qualified instance UserDB TSt
from StdFunc import o, seq

workflowStarter :: [Workflow]
workflowStarter = [workflow "Examples/Workflow starter" "This task rebuilds the client's panel for starting up new workflows." starter]

starter =
					createSharedStore Nothing
	>>= \ref.		chooseWorkflow ref ||- showDescription ref
	
chooseWorkflow ref =
					getWorkflowTreeNodes
	>>= \workflows.	updateSharedInformationA "Tasks" (treeBimap workflows) [] ref
where
	treeBimap workflows =	( \mbSel -> case mbSel of
								Just sel 	= mkTreeSel workflows sel
								Nothing		= mkTree workflows
							, \tree _ -> Just (getSelectedLeaf tree)
							)

showDescription ref =
										showMessageSharedA "Task description" view actions ref
	>>= \(action,Just (_,_,Hidden idx)). case action of
											Action "start-task" _ =
													startWorkflowByIndex idx
												>>|	showDescription ref
											_ =
													stop
where
	actions = [(Action "start-task" " Start task",startPred),(ActionQuit,always)]
	startPred (Valid mbSel)	= isJust mbSel
	startPred _				= False
	
	view (Just (_,Hidden desc,_))	= desc
	view Nothing					= ""

getWorkflowTreeNodes :: Task [TreeNode (!String,!Hidden String,!Hidden Int)]
getWorkflowTreeNodes = mkInstantTask "get a tree of workflows" getWorkflowTree`
where
	getWorkflowTree` tst
		# (workflows,tst) = getAllowedWorkflows tst
		= (TaskFinished (mkFlowTree workflows),tst)
		
	mkFlowTree workflows = seq (map insertWorkflow (zip2 workflows (indexList workflows))) []
		where
			insertWorkflow ({path,description},idx) nodeList = insertWorkflow` (split "/" path) nodeList
			where
				insertWorkflow` [] nodeList = nodeList
				insertWorkflow` [title] nodeList = nodeList ++ [Leaf (title,Hidden description,Hidden idx)]
				insertWorkflow` path=:[nodeP:pathR] [node=:(Node nodeL nodes):nodesR]
					| nodeP == nodeL	= [Node nodeL (insertWorkflow` pathR nodes):nodesR]
					| otherwise			= [node:insertWorkflow` path nodesR]
				insertWorkflow` path [leaf=:(Leaf _):nodesR] = [leaf:insertWorkflow` path nodesR]
				insertWorkflow` [nodeP:pathR] [] = [Node nodeP (insertWorkflow` pathR [])]

startWorkflow :: !Workflow -> Task Void
startWorkflow {thread,containerType} = mkInstantTask "create new task" (startWorkflow` thread containerType)
	
startWorkflow` thread containerType tst
	# (_,_,_,tst) = createTaskInstance thread True True containerType tst
	= (TaskFinished Void,tst)
	
startWorkflowByIndex :: !Int -> Task Void
startWorkflowByIndex idx = mkInstantTask "create new task by index in workflow list" startWorkflowByIndex`
where
	startWorkflowByIndex` tst
		# (workflows,tst)			= getAllowedWorkflows tst
		# {thread,containerType}	= workflows !! idx
		= startWorkflow` thread containerType tst

getAllowedWorkflows tst
	# (session,tst)		= getCurrentSession tst
	# (mbDetails,tst)	= 'UserDB'.getUserDetails session.Session.user tst
	# (workflows,tst)	= getWorkflows tst
	# workflows 		= filter (isAllowed (session.Session.user,mbDetails)) workflows
	= (workflows,tst)
where
	//Allow the root user
	isAllowed (RootUser,_)	_		= True
	//Allow workflows for which the user has permission
	isAllowed (_,Just details) wf	= or [isMember role (mb2list details.UserDetails.roles) \\ role <- wf.Workflow.roles] || isEmpty wf.Workflow.roles
	//Allow workflows without required roles
	isAllowed _ wf					= isEmpty wf.Workflow.roles
