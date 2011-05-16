implementation module Client

import iTasks, TSt, Text
from UserDB import qualified class UserDB(..)
from UserDB import qualified instance UserDB TSt
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB IWorld
from Shared import makeReadOnlyShared
from StdFunc import o, seq

derive bimap Maybe, (,)

clientExample :: [Workflow]
clientExample = [workflow "Examples/Client" "This task rebuilds the client." (Workflow initManagerProperties (staticMenu [Menu "Example" [MenuItem ActionQuit Nothing]]) client)]

client =
					createSharedStore Nothing
	>>= \ref.		(anyTask [chooseWorkflow ref <<@ treeLayout, showDescription ref <<@ descriptionLayout, processTable <<@ processTableLayout]) <<@ parallelLayout
	
chooseWorkflow ref =
					getWorkflowTreeNodes
	>>= \workflows.	updateSharedInformationA "Tasks" (treeBimap workflows) [] ref
	>>|				stop
where
	treeBimap workflows =	( \mbSel -> case mbSel of
								Just sel 	= mkTreeSel workflows sel
								Nothing		= mkTree workflows
							, \tree _ -> Just (getSelectedLeaf tree)
							)

showDescription ref =
							monitorA "Task description" view (const False) actions ref
	>>= \(Just action,r).	case action of
								Action "start-task" _ =
										startWorkflowByIndex (fromHidden (thd3 (fromJust r)))
									>>|	showDescription ref
								_ =
										stop
where
	actions = [(Action "start-task" " Start task",isJust),(ActionQuit,const True)]
	
	view (Just (_,Hidden desc,_))	= desc
	view Nothing					= ""
	
processTable =
		myProcesses
	>>=	updateSharedInformationA "process table" (Table o map toView,\_ _ -> Void) []
	>>|	stop
where
	toView {Process|properties=p=:{taskProperties,managerProperties,systemProperties,progress}} =
		{ title		= Display taskProperties.taskDescription.TaskDescription.title
		, priority	= formatPriority managerProperties.ManagerProperties.priority
		, progress	= formatProgress progress
		, date		= Display systemProperties.issuedAt
		, deadline	= Display managerProperties.ManagerProperties.deadline
		}

:: ProcessTableView =	{ title		:: !Display String
						, priority	:: !HtmlDisplay
						, progress	:: !HtmlDisplay
						, date		:: !Display Timestamp
						, deadline	:: !Display (Maybe DateTime)
						}
derive class iTask ProcessTableView

treeLayout {title,editorParts,buttons} =
	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer (editorContainer ++ buttonContainer) & title = Just title, iconCls = Just "icon-newwork"}
	, width		= FillParent 1 (FixedMinSize 100)
	, height	= FillParent 1 ContentSize
	, margins	= Nothing
	}
where
	editorContainer
		| isEmpty editorParts	= []
			| otherwise			= [	{ content	= TUILayoutContainer (defaultLayoutContainer editorParts)
									, width		= FillParent 1 ContentSize
									, height	= Wrap
									, margins	= Nothing
									}]
	buttonContainer
		| isEmpty buttons	= []
		| otherwise			= [	{ content	= TUILayoutContainer {defaultLayoutContainer buttons & orientation = Horizontal, hGravity = HGRight}
								, width		= FillParent 1 ContentSize
								, height	= Wrap
								, margins	= Nothing
								}]

descriptionLayout {title,editorParts,buttons} =
	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer (defaultContent editorParts buttons) & title = Just title, iconCls = Just "icon-description"}
	, width		= FillParent 1 (FixedMinSize 100)
	, height	= Fixed 150
	, margins	= Nothing
	}
	
parallelLayout {TUIParallel | items=i=:[tree,description,processTable]} =
	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [left,right] & orientation = Horizontal}
	, width		= FillParent 1 ContentSize
	, height	= FillParent 1 ContentSize
	, margins	= Nothing
	}
where
	left =	{ content	= TUILayoutContainer (defaultLayoutContainer [tree,description])
			, width		= Fixed 260
			, height	= FillParent 1 ContentSize
			, margins	= Nothing
			}
	right = { content	= TUILayoutContainer (defaultLayoutContainer [processTable,htmlDisplay "work tab panel"])
			, width		= FillParent 1 ContentSize
			, height	= FillParent 1 ContentSize
			, margins	= Nothing
			}
			
processTableLayout {editorParts} =
	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer editorParts & frame = True}
	, width		= FillParent 1 ContentSize
	, height	= Fixed 200
	, margins	= Nothing
	}

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
startWorkflow {thread,managerProperties,menu} = mkInstantTask "create new task" (startWorkflow` thread managerProperties menu)
	
startWorkflow` thread managerProperties menu tst
	# (_,_,_,tst) = createTaskInstance thread True True managerProperties menu tst
	= (TaskFinished Void,tst)
	
startWorkflowByIndex :: !Int -> Task Void
startWorkflowByIndex idx = mkInstantTask "create new task by index in workflow list" startWorkflowByIndex`
where
	startWorkflowByIndex` tst
		# (workflows,tst)					= getAllowedWorkflows tst
		# {thread,managerProperties,menu}	= workflows !! idx
		= startWorkflow` thread managerProperties menu tst

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
	
myProcesses =
				getCurrentUser
	>>=	\user.	return (makeReadOnlyShared ('ProcessDB'.getProcessesForUser user [Running] [Active]))
