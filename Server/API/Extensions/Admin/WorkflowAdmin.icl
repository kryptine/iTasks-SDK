implementation module WorkflowAdmin

import iTasks
import StdMisc, Tuple, Text, Shared
from StdFunc import seq
from Util import mb2list, timestampToGmDateTime

// SPECIALIZATIONS
derive gVisualizeText	Workflow
derive gVisualizeEditor	Workflow
derive gHeaders			Workflow
derive gGridRows		Workflow
derive gUpdate 			Workflow
derive gDefaultMask		Workflow
derive gVerify			Workflow
derive JSONEncode		Workflow
derive JSONDecode		Workflow
derive gEq				Workflow

gVisualizeText{|WorkflowTaskContainer|} _ _	= []
gVisualizeEditor{|WorkflowTaskContainer|} _ vst = noVisualization vst
gHeaders{|WorkflowTaskContainer|} = (undef, ["Workflow task container"])
gGridRows{|WorkflowTaskContainer|} _ _ = Nothing
gUpdate{|WorkflowTaskContainer|} mode ust = basicUpdate mode (\Void x -> x) (WorkflowTask defTask) ust
where
	defTask :: Task Void
	defTask = abort "default task container"

gDefaultMask{|WorkflowTaskContainer|}_ = [Touched []]

gVerify{|WorkflowTaskContainer|} _ vst = alwaysValid vst

JSONEncode{|WorkflowTaskContainer|} c		= [dynamicJSONEncode c]
JSONDecode{|WorkflowTaskContainer|} [c:r]	= (dynamicJSONDecode c,r)
JSONDecode{|WorkflowTaskContainer|} r		= (Nothing,r)
gEq{|WorkflowTaskContainer|} _ _			= True

// Internal state type
:: ClientState =
	{ selectedProcess	:: !Maybe ProcessId
	, selectedWorkflow	:: !Maybe (!WorkflowId, !String)
	}
	
derive class iTask ClientState

// SHARES
// Available workflows

workflows :: Shared [Workflow]
workflows = sharedStore "Workflows" []

workflowById :: !WorkflowId -> Shared Workflow
workflowById index = mapShared (toPrj,fromPrj) workflows
where
	toPrj flows = flows !! index
	fromPrj flow flows = updateAt index flow flows
	
allowedWorkflows :: ReadOnlyShared [Workflow]
allowedWorkflows = mapSharedRead filterAllowed (workflows |+| (currentUser |+| currentUserDetails))
where
	filterAllowed (workflows,(user,mbDetails)) = filter (isAllowedWorkflow user mbDetails) workflows
	
workflowTree :: ReadOnlyShared (Tree (Either WorkflowFolderLabel (WorkflowId,Workflow)))
workflowTree = mapShared (mkFlowTree,\Void w -> w) workflows

allowedWorkflowTree :: ReadOnlyShared (Tree (Either WorkflowFolderLabel (WorkflowId,Workflow)))
allowedWorkflowTree = mapSharedRead mkFlowTree allowedWorkflows

mkFlowTree :: ![Workflow] -> Tree (Either WorkflowFolderLabel (WorkflowId,Workflow))
mkFlowTree workflows = Tree (seq [insertWorkflow i w \\ w <- workflows & i <- [0..]] [])
where
	insertWorkflow i descr=:{Workflow|path} nodeList = insertWorkflow` (split "/" path) nodeList
	where
		insertWorkflow` [] nodeList = nodeList
		insertWorkflow` [title] nodeList = nodeList ++ [Leaf (Right (i,descr))]
		insertWorkflow` path=:[nodeP:pathR] [node=:(Node (Left nodeL) nodes):nodesR]
			| nodeP == nodeL	= [Node (Left nodeL) (insertWorkflow` pathR nodes):nodesR]
			| otherwise			= [node:insertWorkflow` path nodesR]
		insertWorkflow` path [leaf=:(Leaf _):nodesR] = [leaf:insertWorkflow` path nodesR]
		insertWorkflow` [nodeP:pathR] [] = [Node (Left nodeP) (insertWorkflow` pathR [])]
		
// MANAGEMENT TASKS

manageWorkflows :: ![Workflow] ->  Task Void
manageWorkflows iflows = installInitialWorkflows iflows >>| forever (doAuthenticated workflowDashboard)

installInitialWorkflows ::[Workflow] -> Task Void
installInitialWorkflows [] = return Void
installInitialWorkflows iflows
	= get workflows
	>>= \flows -> case flows of
		[]	= allTasks [addWorkflow flow \\ flow <- iflows] >>| return Void
		_	= return Void

doAuthenticated :: (Task a) -> Task (Maybe a) | iTask a
doAuthenticated task
	//=	(appIdentity ||- enterInformation "Log in" []) <<@ tweak
	=	enterInformation "Log in" [] <<@ tweak
	>>= \credentials ->
		authenticateUser (toString credentials.Credentials.username) (toString credentials.Credentials.password)
	>>= \mbUser -> case mbUser of
		Nothing
			= viewInformation "Log in failed" [] Nothing
		Just user
			=	workAs user task
			>>= transform Just
where
	appIdentity :: Task Void
	appIdentity = (viewSharedInformation "Application identity" [] applicationName Void >>+ noActions)
	
	tweak :: LayoutTweak
	tweak = \(def,actions) -> ({TUIDef|def & margins = topMargin 100, width = Just (WrapContent 0)},actions)
	
workflowDashboard :: Task Void
workflowDashboard = mainLayout @>> parallel "Workflow Dashboard" {selectedProcess = Nothing, selectedWorkflow = Nothing} (\_ _ -> Void)
	[ (Embedded,	\list	-> infoBar 								)
	, (Embedded,	\list	-> chooseWorkflow (taskListState list)	<<@ treeLayout)
	, (Embedded,	\list	-> viewDescription (taskListState list)	)
	, (Embedded,	\list	-> workTabPanel list					<<@ tabLayout)
	, (Embedded,	\list	-> processTable list					<<@ processTableLayout)
	, (Embedded,	\_		-> controlClient)
	]

infoBar :: Task ParallelControl
infoBar =	(viewSharedInformation "Info" [DisplayView (GetShared view)] currentUser Void <<@ infoBarLayout >>+ (\_ -> UserActions [(ActionRefresh, Just Continue),(Action "Log out",Just Stop)]) )
		<! 	(\res -> case res of Stop = True; Continue = False) 
where
	view user = "Welcome " +++ toString user
	
chooseWorkflow :: !(Shared ClientState) -> Task ParallelControl
chooseWorkflow state = updateSharedInformation "Tasks" [UpdateView (GetCombined mkTree, SetCombined putback)] (state >+| allowedWorkflowTree) Nothing >>+ noActions
where
	mkTree sel (_,flows) = mkTreeChoice (fmap mapF flows) (fmap Just sel)
	where
		mapF (Left label)									= (label, Nothing)
		mapF (Right (index,{Workflow|path,description}))	= (last (split "/" path), Just (index,description))
	putback tree _ (state,_) = (Just selection, Just {state & selectedWorkflow = selection})
	where
		selection = getSelection tree

viewDescription :: !(Shared ClientState) -> Task ParallelControl
viewDescription state = forever (
		viewSharedInformation "Task description" [DisplayView (GetShared view)] state Void <<@ descriptionLayout
	>?*	[(Action "Start workflow", Sometimes \{modelValue=m=:({selectedWorkflow},_)} -> if (isJust selectedWorkflow) (Just (addWorkflow (fromJust selectedWorkflow))) Nothing)])
where			
	view {selectedWorkflow} = case selectedWorkflow of
		Nothing			= ""
		Just (_,descr)	= descr
		
	addWorkflow (wid,_) =
							get (workflowById wid)
		>>=	\wf ->			get currentUser
		>>= \user ->		appendTask (Detached {noMeta & worker = Just user}, \_ -> (fromContainer wf.Workflow.task)) topLevelTasks

	fromContainer (WorkflowTask t) = t >>| return Continue
	fromContainer (ParamWorkflowTask tf) = (enterInformation "Enter parameters" [] >>= tf >>| return Continue)
	
processTable :: !(TaskList ClientState) -> Task ParallelControl	
processTable taskList = updateSharedInformation "process table" [UpdateView (GetCombined mkTable, SetCombined putback)] (processes |+< state) Nothing >>+ noActions
where
	state = taskListState taskList
	// list of active processes for current user without current one (to avoid work on dependency cycles)
	processes = mapSharedRead (\(procs,ownPid) -> filter (show ownPid) (pflatten procs)) (processesForCurrentUser |+| currentProcessId)
	where
		show ownPid {processId,progressMeta} = processId <> ownPid && progressMeta.status == Running
	
	mkTable mbSel (procs,_) = Table ["Title", "Priority", "Date", "Deadline"] (map mkRow procs) mbSel
	mkRow {TaskInstanceMeta|processId,taskMeta,progressMeta,managementMeta} =
		[ html taskMeta.TaskMeta.title
		, formatPriority managementMeta.priority
		, Text (visualizeAsText AsDisplay progressMeta.issuedAt)
		, Text (visualizeAsText AsDisplay managementMeta.completeBefore)
		, Text (toString processId)
		]
		
	pflatten procs = flatten [[p:pflatten p.subInstances] \\ p <- procs]
	
	putback (Table _ cells mbSel) _ (_,state) = (Just mbSel,Just {state & selectedProcess = fmap (getProcId cells) mbSel})
	getProcId cells idx = case cells !! idx !! 4 of
		Text procId	= fromString procId
		_ = abort "getProcId"

workTabPanel :: !(TaskList ClientState) -> Task ParallelControl
workTabPanel taskList = parallel "Work tab panel" [] (\_ _ -> Continue) [(Embedded, controlWorkTabs (taskListState taskList))]

controlWorkTabs :: !(Shared ClientState) !(TaskList [ProcessId]) -> Task ParallelControl
controlWorkTabs state taskList = forever (
					chooseActionDyn openTabTrigger (state >+< openProcs) <<@ Hide
	>>= \proc ->	appendTask (Embedded, \_ -> workTab proc openProcs  <<@ singleControlLayout) taskList
	>>|				update (\state -> {state & selectedProcess = Nothing}) state 
	>>|				update (\procs -> [proc:procs]) openProcs )
where
	openProcs = taskListState taskList
	
	openTabTrigger ({selectedProcess},procs) = case selectedProcess of
		Just selectedProcess | not (isMember selectedProcess procs)
			= StopInteraction selectedProcess
		_
			= UserActions []

workTab :: !ProcessId !(Shared [ProcessId]) -> Task ParallelControl											
workTab procId openProcs =
		update (\procs -> [procId:procs]) openProcs
	>>|	(workOn procId >>+ \{modelValue} -> if (modelValue =!= WOActive) (StopInteraction Void) (UserActions [(ActionClose, Just Void)]))
	>>|	update(filter ((<>) procId)) openProcs 
	>>|	return Continue

controlClient :: Task ParallelControl										
controlClient = chooseAction [(ActionQuit, Stop)]

addWorkflow :: !Workflow -> Task Workflow
addWorkflow workflow
	=	update (\flows -> flows ++ [workflow]) workflows
	>>|	return workflow

// LAYOUTS
mainLayout {TUIParallel | items=i=:[(_,_,_,Just infoBar,_), (_,_,_,Just tree,_), (_,_,_,Just description,_),(_,_,_,Just workTabPanel,_), (_,_,_,Just processTable,_), (_,_,_,_,controlActions):_]} =
	({ content	= content
	, width		= Just (FillParent 1 (FixedMinSize 0))
	, height	= Just (FillParent 1 (FixedMinSize 0))
	, margins	= Nothing
	},controlActions)
where
	content = TUIContainer {TUIContainer | defaultLayoutContainer [left,right] & direction = Horizontal}
	/*
	content = TUIBorderContainer {TUIBorderContainer | direction = Horizontal
								 , itemA = {TUIBorderItem| title = Nothing, iconCls = Nothing, item = left}
								 , itemB = {TUIBorderItem| title = Nothing, iconCls = Nothing, item = right} 
								 , initSplit = 260, collapsible = True}
	*/
	left =	{ content	= TUIPanel (defaultLayoutPanel [tree,description])
			, width		= Just (Fixed 260)
			//, width		= FillParent 1 (FixedMinSize 100)
			, height	= Just (FillParent 1 (FixedMinSize 0))
			, margins	= Nothing
			}
	right = { content	= TUIPanel (defaultLayoutPanel [infoBar,workArea])
			, width		= Just (FillParent 1 (FixedMinSize 0))
			, height	= Just (FillParent 1 (FixedMinSize 0))
			, margins	= Nothing
			}
	
	/*
	workArea = {content = TUIBorderContainer {TUIBorderContainer | direction = Vertical
								  , itemA = {TUIBorderItem | title = Nothing, iconCls = Nothing, item = fillParent processTable}
								  , itemB = {TUIBorderItem | title = Nothing, iconCls = Nothing, item = fillParent workTabPanel}
								  , initSplit = 200
								  , collapsible = True
								  }
				,width		= FillParent 1 (FixedMinSize 0)
				,height		= FillParent 1 (FixedMinSize 0)
				,margins	= Nothing
				}
	*/
	workArea =	{content	= TUIContainer (defaultLayoutContainer [processTable, fill workTabPanel])
				,width		= Just (FillParent 1 (FixedMinSize 0))
				,height		= Just (FillParent 1 (FixedMinSize 0))
				,margins	= Nothing
				}
				
mainLayout p = defaultParallelLayout p

infoBarLayout :: TUIInteraction -> (TUIDef,[TaskAction])
infoBarLayout {title,editorParts,actions}
	# (buttons,actions) = defaultButtons actions
	= ({ content	= TUIContainer {TUIContainer|defaultLayoutContainer [{hd editorParts & width = Just (WrapContent 0), margins = Nothing}:buttons]
								& direction = Horizontal, halign = AlignRight, valign = AlignMiddle, baseCls = Just "x-panel-header"}
	  , width		= Just (FillParent 1 (ContentSize))
	  , height	= Just (Fixed 30)
	  , margins	= Nothing
	}, actions)

treeLayout {title,editorParts,actions} = (	{ content	= TUIPanel {TUIPanel | defaultLayoutPanel [{hd editorParts & width = Just (FillParent 1 ContentSize), height = Just (FillParent 1 ContentSize)}] & title = title, iconCls = Just "icon-newwork", frame = False}
											, width		= Just (FillParent 1 (FixedMinSize 100))
											, height	= Just (FillParent 1 (FixedMinSize 0))
											, margins	= Nothing
											}, actions)

descriptionLayout {title,editorParts,actions} = (	{ content	= TUIPanel {TUIPanel | defaultLayoutPanel (defaultContent editorParts (fst (defaultButtons actions))) & title = title, iconCls = Just "icon-description", frame = False}
													, width		= Just (FillParent 1 (FixedMinSize 100))
													, height	= Just (Fixed 150)
													, margins	= Nothing
													}, actions)

processTableLayout interaction
	= ({hd interaction.editorParts & width = Just (FillParent 1 ContentSize), height = Just (Fixed 150), margins = (sameMargins 0)},interaction.TUIInteraction.actions)	 
singleControlLayout interaction
	= ({hd interaction.editorParts & width = Just (FillParent 1 ContentSize), height = Just (FillParent 1 ContentSize)},interaction.TUIInteraction.actions)

// UTIL FUNCTIONS

workflow :: String String w -> Workflow | toWorkflow w
workflow path description task = toWorkflow path description [] task

restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w
restrictedWorkflow path description roles task = toWorkflow path description roles task

inputWorkflow :: String String String (a -> Task b) -> Workflow | iTask a & iTask b
inputWorkflow name desc inputdesc tfun
	= workflow name desc (enterInformation inputdesc [] >>= tfun)  
	
instance toWorkflow (Task a) | iTask a
where
	toWorkflow path description roles task = toWorkflow path description roles (Workflow noMeta task)
	
instance toWorkflow (WorkflowContainer a) | iTask a
where
	toWorkflow path description roles (Workflow managerP task) = mkWorkflow path description roles (WorkflowTask task) managerP

instance toWorkflow (a -> Task b) | iTask a & iTask b
where
	toWorkflow path description roles paramTask = toWorkflow path description roles (ParamWorkflow noMeta paramTask)
	
instance toWorkflow (ParamWorkflowContainer a b) | iTask a & iTask b
where
	toWorkflow path description roles (ParamWorkflow managerP paramTask) = mkWorkflow path description roles (ParamWorkflowTask paramTask) managerP
	
mkWorkflow path description roles taskContainer managerProps =
	{ Workflow
	| path	= path
	, roles	= roles
	, task = taskContainer
	, description = description
	, managerProperties = managerProps
	}

isAllowedWorkflow :: !User !(Maybe UserDetails) !Workflow -> Bool
//Allow the root user
isAllowedWorkflow RootUser _ _						= True
//Allow workflows without required roles
isAllowedWorkflow _ _ {Workflow|roles=r=:[]}		= True
//Allow workflows for which the user has permission
isAllowedWorkflow _ (Just details) {Workflow|roles}	= or [isMember role (mb2list details.UserDetails.roles) \\ role <- roles]
//Don't allow workflows in other cases
isAllowedWorkflow _ _ _								= False