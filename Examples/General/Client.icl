implementation module Client

import iTasks, Text
from UserDB import qualified class UserDB(..)
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB IWorld
from Shared import makeReadOnlyShared
from StdFunc import o, seq
from Util import mb2list

import StdMisc

derive bimap Maybe, (,)

clientExample :: [Workflow]
clientExample = [workflow "Examples/Client" "This task rebuilds the client." (Workflow initManagerProperties (staticMenu [Menu "Example" [MenuItem ActionQuit Nothing]]) client)]

client = parallelLayout @>> parallel "Client" Nothing (\_ _ -> Void)
	[ ShowAs BodyTask (\s _ -> chooseWorkflow s <<@ treeLayout)
	, ShowAs BodyTask (\s _ -> showDescription s <<@ descriptionLayout)
	, ShowAs BodyTask (\_ _ -> processTable <<@ processTableLayout)
	]
	
chooseWorkflow ref =
					getWorkflowTreeNodes
	>>= \workflows.	updateSharedInformation "Tasks" [View (treeBimap workflows)] ref >>+ noActions
where
	treeBimap workflows =	( \mbSel -> case mbSel of
								Just sel 	= mkTreeSel workflows sel
								Nothing		= mkTree workflows
							, \tree _ -> Just (getSelectedLeaf tree)
							)

showDescription ref =
								monitor "Task description" [Get view] ref
	>>* \{modelValue = mbR}.	UserActions	[ (Action "start-task" "Start task",	fmap (\r ->
																				stop//startWorkflowByIndex (fromHidden (thd3 r))
																			>>|	showDescription ref
																		) mbR)
								, (ActionQuit,							Just stop)
								]
where						
	view (Just (_,Hidden desc,_))	= desc
	view Nothing					= ""
	
processTable =
		get currentUser
	>>=	\user. updateSharedInformation "process table" [View (Table o map toView,\_ _ -> Void)] (currentProcessesForUser user) >>+ noActions
where
	toView {Process|properties=p=:{taskProperties,managerProperties,systemProperties}} =
		{ title		= Display taskProperties.taskDescription.TaskDescription.title
		, priority	= formatPriority managerProperties.ManagerProperties.priority
		, date		= Display systemProperties.issuedAt
		, deadline	= Display managerProperties.ManagerProperties.deadline
		}

:: ProcessTableView =	{ title		:: !Display String
						, priority	:: !HtmlDisplay
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
									, height	= (WrapContent 0)
									, margins	= Nothing
									}]
	buttonContainer
		| isEmpty buttons	= []
		| otherwise			= [	{ content	= TUILayoutContainer {defaultLayoutContainer buttons & orientation = Horizontal, hGravity = HGRight}
								, width		= FillParent 1 ContentSize
								, height	= (WrapContent 0)
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
getWorkflowTreeNodes = abort "TODO" //mkInstantTask "get a tree of workflows" getWorkflowTree`
/*
where
	getWorkflowTree` iworld
		# (workflows,iworld) = getAllowedWorkflows iworld
		= (TaskFinished (mkFlowTree workflows),iworld)	
	mkFlowTree workflows = seq (map insertWorkflow (zip2 workflows (indexList workflows))) []
		where
			insertWorkflow ({Workflow|path,description},idx) nodeList = insertWorkflow` (split "/" path) nodeList
			where
				insertWorkflow` [] nodeList = nodeList
				insertWorkflow` [title] nodeList = nodeList ++ [Leaf (title,Hidden description,Hidden idx)]
				insertWorkflow` path=:[nodeP:pathR] [node=:(Node nodeL nodes):nodesR]
					| nodeP == nodeL	= [Node nodeL (insertWorkflow` pathR nodes):nodesR]
					| otherwise			= [node:insertWorkflow` path nodesR]
				insertWorkflow` path [leaf=:(Leaf _):nodesR] = [leaf:insertWorkflow` path nodesR]
				insertWorkflow` [nodeP:pathR] [] = [Node nodeP (insertWorkflow` pathR [])]
*/
startWorkflow :: !Workflow -> Task Void
startWorkflow {thread,managerProperties,menu} = abort "TODO"

startWorkflowByIndex :: !Int -> Task Void
startWorkflowByIndex idx = abort "TODO" 
/*
= mkInstantTask "create new task by index in workflow list" startWorkflowByIndex`
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
*/
