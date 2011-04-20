implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdFunc
from Types import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractiveTaskType(..)

htmlDisplay :: !(Maybe String) !String -> TUIDef
htmlDisplay mbLabel html =	{ content	= TUIControl TUIHtmlDisplay
											{ TUIControl
											| name			= ""
											, value			= JSONString html
											, fieldLabel	= mbLabel
											, optional		= True
											, errorMsg		= ""
											, hintMsg		= ""
											, eventValue	= Nothing
											, taskId		= ""
											}
							, width		= Wrap
							, height	= Wrap
							}

defaultLayoutContainer :: ![TUIDef] -> TUILayoutContainer
defaultLayoutContainer items =	{ items			= items
								, cls			= Nothing
								, orientation	= Vertical
								, title			= Nothing
								, frame			= False
								, iconCls		= Nothing
								, hGravity		= HGLeft
								, vGravity		= VGTop
								, padding		= Nothing
								}

defaultInteractiveLayout :: InteractiveLayoutMerger
defaultInteractiveLayout = \{title,description,mbContext,editor,buttons,type,isControlTask} -> defaultPanelDescr
	title
	(defaultInteractiveIcon type isControlTask)
	description
	(defaultContent mbContext editor buttons)
	Auto
	
fullWidthInteractiveLayout :: InteractiveLayoutMerger
fullWidthInteractiveLayout = \{title,description,mbContext,editor,buttons,type,isControlTask} -> defaultPanelDescr
	title
	(defaultInteractiveIcon type isControlTask)
	description
	(defaultContent mbContext editor buttons)
	(FillParent 1 ContentSize)
	
defaultContent :: !(Maybe TUIDef) ![TUIDef] ![TUIDef] -> [TUIDef]
defaultContent mbContext editor buttons = content
where
	content = case maybeToList mbContext ++ editorContainer ++ buttonContainer of
		[]					= []
		content				= [defaultContentPanel content]
	editorContainer
		| isEmpty editor	= []
		| otherwise			= [	{ content	= TUIFormContainer {TUIFormContainer | items = editor, fieldLabel = Nothing, optional = False}
								, width		= FillParent 1 ContentSize
								, height	= Wrap
								}]
	buttonContainer
		| isEmpty buttons	= []
		| otherwise			= [	{ content	= TUILayoutContainer {defaultLayoutContainer buttons & orientation = Horizontal, hGravity = HGRight}
								, width		= FillParent 1 ContentSize
								, height	= Wrap
								}]

defaultParallelLayout :: ParallelLayoutMerger
defaultParallelLayout = \{TUIParallel|title,description,items} -> defaultPanelDescr title "icon-parallel-task" description items Wrap

minimalParallelLayout :: ParallelLayoutMerger
minimalParallelLayout = \{TUIParallel|title,description,items} ->	{ content	= TUILayoutContainer (defaultLayoutContainer items)
																	, width		= Auto
																	, height	= Auto
																	}

defaultResultLayout :: ResultLayoutMerger
defaultResultLayout = \{TUIResult|title,description,result} -> defaultPanelDescr title "icon-task-result" description [defaultContentPanel [result]] Auto

defaultPanelDescr :: !PanelTitle !PanelIcon !TUIDef ![TUIDef] !TUISize -> TUIDef
defaultPanelDescr title iconCls description form width = defaultPanel title iconCls [defaultDescriptionPanel description:form] width

defaultPanel :: !PanelTitle !PanelIcon ![TUIDef] !TUISize -> TUIDef
defaultPanel title iconCls content width =	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer content & title = Just title, iconCls = Just iconCls}
											, width		= width
											, height	= Auto
											}

defaultDescriptionPanel :: !TUIDef -> TUIDef
defaultDescriptionPanel descr =		{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [descr] & frame = True}
									, width		= FillParent 1 ContentSize
									, height	= Wrap
									}

defaultContentPanel :: ![TUIDef] -> TUIDef
defaultContentPanel content =		{ content	= TUILayoutContainer {defaultLayoutContainer content & padding = Just 5}
									, width		= FillParent 1 ContentSize
									, height	= Wrap
									}

defaultInteractiveIcon :: !InteractiveTaskType !Bool -> PanelIcon	
defaultInteractiveIcon type isControlTask
	| isControlTask	= "icon-control-task"
	= case type of
		Information	= "icon-interaction-task"
		Message		= "icon-message-task"
		Instruction	= "icon-instruction-task"
		Monitor		= "icon-monitor-task"