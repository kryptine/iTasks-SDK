implementation module Client

import iTasks, Text
from UserDB import qualified class UserDB(..)
from ProcessDB import qualified class ProcessDB(..)
from ProcessDB import qualified instance ProcessDB IWorld
from StdFunc import o, seq
from Util import mb2list, timestampToGmDateTime

import StdMisc

derive bimap Maybe, (,)

clientExample :: [Workflow]
clientExample = [workflow "Examples/Client" "This task rebuilds the client." (Workflow initManagerProperties client)]

client = parallelLayout @>> parallel "Client" {selectedProcess = Nothing} (\_ _ -> Void)
	[ (BodyTask, \_ -> chooseWorkflow <<@ treeLayout)
	, (BodyTask, \_ -> showDescription <<@ descriptionLayout)
	, (BodyTask, processTable)
	, (BodyTask, \list -> workTabPanel list <<@ workTabPanelLayout)
	, (HiddenTask, controlClient)
	]

chooseWorkflow = showInformation "choose workflow" [] "choose workflow" >>| return Continue
	/*>>|				getWorkflowTreeNodes
	>>= \workflows.	updateSharedInformation "Tasks" [UpdateView (treeBimap workflows)] ref Void >>+ noActions
where
	treeBimap workflows =	( GetShared \mbSel -> case mbSel of
								Just sel 	= mkTreeSel workflows sel
								Nothing		= mkTree workflows
							, PutbackShared \tree _ _ -> Just (getSelectedLeaf tree)
							)*/

showDescription = showInformation "show description" [] "show description" >>| return Continue
									/*showSharedInformation "Task description" [ShowView (GetShared view)] ref Void
	>>* \{modelValue = (mbR,_)}.	UserActions	[ (Action "Start task",	fmap (\r ->
																					stop//startWorkflowByIndex (fromHidden (thd3 r))
																				>>|	showDescription ref
																			) mbR)
									]
where						
	view (Just (_,Hidden desc,_))	= desc
	view Nothing					= ""*/
	
processTable taskList =
		get currentUser
	>>=	\user. processTableLayout @>> updateSharedInformation "process table" [UpdateView (GetLocalAndShared mkTable, Putback \(FillControlSize (Table _ cells mbSel)) _ (_,state) -> (Just mbSel,Just {state & selectedProcess = fmap (getProcId cells) mbSel}))] (currentProcessesForUser user |+< state) Nothing >>+ noActions
	>>| return Continue
where
	state = taskListState taskList
	mkTable mbSel (procs,_) = FillControlSize (Table ["Title", "Priority", "Date", "Deadline"] (map mkRow procs) mbSel)
	mkRow {Process|properties=p=:{taskProperties,managerProperties,systemProperties},processId} =
		[ html taskProperties.taskDescription.TaskDescription.title
		, formatPriority managerProperties.ManagerProperties.priority
		, visualizeAsHtmlDisplay (timestampToGmDateTime systemProperties.issuedAt)
		, visualizeAsHtmlDisplay managerProperties.ManagerProperties.deadline
		, Text (toString processId)
		]
	getProcId cells idx = case cells !! idx !! 4 of
		Text procId	= toInt procId
		_ = abort "getProcId"

workTabPanel taskList = parallel "Work tab panel" Void (\_ _ -> Void) [(HiddenTask, controlWorkTabs (taskListState taskList))] >>| return Continue

controlWorkTabs state taskList = forever (													(showSharedInformation "waiting for trigger" [] state Void >? (\({selectedProcess},_) -> isJust selectedProcess))
											>>= \({selectedProcess=s=:(Just proc)},_) ->	appendTask (BodyTask, workTab proc) taskList
											>>|												update (\state -> {state & selectedProcess = Nothing}) state
											>>|												return Continue
										)
											
workTab procId _ = workOn procId <<@ workTabLayout >? (\taskState -> taskState =!= WOActive) >>| return Continue
										
controlClient _ = showInformation "waiting for event" [] Void >?* [(ActionQuit, Always (return Stop))]

:: ClientState =
	{ selectedProcess	:: !Maybe ProcessId
	}
derive class iTask ClientState

treeLayout {title,editorParts,actions} 
	# (buttons,actions) = defaultButtons actions
	= (
	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer (editorContainer ++ buttonContainer buttons) & title = Just title, iconCls = Just "icon-newwork"}
	, width		= FillParent 1 (FixedMinSize 100)
	, height	= FillParent 1 ContentSize
	, margins	= Nothing
	}, actions)
where
	editorContainer
		| isEmpty editorParts	= []
			| otherwise			= [	{ content	= TUILayoutContainer (defaultLayoutContainer editorParts)
									, width		= FillParent 1 ContentSize
									, height	= (WrapContent 0)
									, margins	= Nothing
									}]
	buttonContainer buttons
		| isEmpty buttons	= []
		| otherwise			= [	{ content	= TUILayoutContainer {defaultLayoutContainer buttons & orientation = Horizontal, hGravity = HGRight}
								, width		= FillParent 1 ContentSize
								, height	= (WrapContent 0)
								, margins	= Nothing
								}]

descriptionLayout {title,editorParts,actions}
	# (buttons,actions) = defaultButtons actions
	=
	({ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer (defaultContent editorParts buttons) & title = Just title, iconCls = Just "icon-description"}
	, width		= FillParent 1 (FixedMinSize 100)
	, height	= Fixed 150
	, margins	= Nothing
	},actions)

parallelLayout :: TUIParallel -> (TUIDef,[TaskAction])	
parallelLayout {TUIParallel | items=i=:[(Just tree,_), (Just description,_), (Just processTable,_), (Just workTabPanel,_), (_,controlActions)]} =
	({ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [left,right] & orientation = Horizontal}
	, width		= FillParent 1 ContentSize
	, height	= FillParent 1 ContentSize
	, margins	= Nothing
	},controlActions)
where
	left =	{ content	= TUILayoutContainer (defaultLayoutContainer [tree,description])
			, width		= Fixed 260
			, height	= FillParent 1 ContentSize
			, margins	= Nothing
			}
	right = { content	= TUILayoutContainer (defaultLayoutContainer [processTable,workTabPanel])
			, width		= FillParent 1 ContentSize
			, height	= FillParent 1 ContentSize
			, margins	= Nothing
			}

processTableLayout :: TUIInteraction -> (TUIDef,[TaskAction])	
processTableLayout {editorParts,actions} =
	({ content	= TUILayoutContainer (defaultLayoutContainer editorParts)
	, width		= FillParent 1 ContentSize
	, height	= Fixed 200
	, margins	= Nothing
	},actions)
	
workTabPanelLayout :: ParallelLayouter
workTabPanelLayout = \{TUIParallel|title,items} ->
	let (tuis,actions) = unzip items in
		({ content	= TUITabContainer {TUITabContainer | items = [{content = TUITab {TUITab|title = title +++ " " +++ toString n, iconCls = (Just "icon-input-task"), items = tui}, margins = Nothing, width = Auto, height = Auto} \\ tui <- catMaybes tuis & n <- [1..]]}
		 , width	= Auto
		 , height	= Auto
		 , margins	= Nothing
		 }, flatten actions)
		 
workTabLayout :: InteractionLayouter
workTabLayout = \interaction = ({TUIDef | hd interaction.editorParts & width = FillParent 1 ContentSize},interaction.TUIInteraction.actions)

getWorkflowTreeNodes :: Task [TreeNode (!String,!Hidden String,!Hidden Int)]
getWorkflowTreeNodes = return defaultValue //mkInstantTask "get a tree of workflows" getWorkflowTree`
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
startWorkflow {thread,managerProperties} = abort "TODO"

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
