implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdFunc
from Types import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractionTaskType(..), :: OutputTaskType(..)

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
							, margins	= Nothing
							}

defaultLayoutContainer :: ![TUIDef] -> TUILayoutContainer
defaultLayoutContainer items =	{ items			= items
								, orientation	= Vertical
								, title			= Nothing
								, frame			= False
								, iconCls		= Nothing
								, hGravity		= HGLeft
								, vGravity		= VGTop
								, padding		= Nothing
								}

sameMargins :: !TUIFixedSize -> TUIMargins
sameMargins m =	{ top		= m
				, right		= m
				, bottom	= m
				, left		= m
				}

defaultInteractionLayout :: InteractionLayoutMerger
defaultInteractionLayout = \{title,description,editorParts,buttons,type,isControlTask,localInteraction} -> defaultPanelDescr
	title
	(defaultInteractionIcon type isControlTask localInteraction)
	description
	(defaultContent editorParts buttons Auto)
	
fullWidthInteractionLayout :: InteractionLayoutMerger
fullWidthInteractionLayout = \{title,description,editorParts,buttons,type,isControlTask,localInteraction} -> defaultPanelDescr
	title
	(defaultInteractionIcon type isControlTask localInteraction)
	description
	(defaultContent editorParts buttons (FillParent 1 ContentSize))
	
defaultContent :: ![TUIDef] ![TUIDef] !TUISize -> [TUIDef]
defaultContent editor buttons width = [defaultContentPanel (editorContainer ++ buttonContainer)]
where
	// also add editor container if editor is empty, it's needed as spacer such that buttons are placed at the bottom of the panel
	editorContainer			= [	{ content	= TUIFormContainer {TUIFormContainer | items = editor, fieldLabel = Nothing, optional = False}
								, width		= width
								, height	= FillParent 1 ContentSize
								, margins	= Nothing
								}]
	buttonContainer
		| isEmpty buttons	= []
		| otherwise			= [	{ content	= TUILayoutContainer {defaultLayoutContainer buttons & orientation = Horizontal, hGravity = HGRight}
								, width		= FillParent 1 ContentSize
								, height	= Wrap
								, margins	= Nothing
								}]

defaultParallelLayout :: ParallelLayoutMerger
defaultParallelLayout = \{TUIParallel|title,description,items} -> defaultPanelDescr title "icon-parallel-task" description items

minimalParallelLayout :: ParallelLayoutMerger
minimalParallelLayout = \{TUIParallel|title,description,items} ->	{ content	= TUILayoutContainer (defaultLayoutContainer items)
																	, width		= Auto
																	, height	= Auto
																	, margins	= Nothing
																	}

defaultResultLayout :: ResultLayoutMerger
defaultResultLayout = \{TUIResult|title,description,result} -> defaultPanelDescr title "icon-task-result" description [defaultContentPanel [result]]

defaultPanelDescr :: !PanelTitle !PanelIcon !TUIDef ![TUIDef] -> TUIDef
defaultPanelDescr title iconCls description form = defaultPanel title iconCls [defaultDescriptionPanel description:form]

defaultPanel :: !PanelTitle !PanelIcon ![TUIDef] -> TUIDef
defaultPanel title iconCls content =	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer content & title = Just title, iconCls = Just iconCls}
										, width		= Auto
										, height	= Auto
										, margins	= Just (sameMargins 10)
										}

defaultDescriptionPanel :: !TUIDef -> TUIDef
defaultDescriptionPanel descr =		{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [descr] & frame = True}
									, width		= FillParent 1 ContentSize
									, height	= Wrap
									, margins	= Nothing
									}

defaultContentPanel :: ![TUIDef] -> TUIDef
defaultContentPanel content =		{ content	= TUILayoutContainer {defaultLayoutContainer content & padding = Just 5}
									, width		= FillParent 1 ContentSize
									, height	= FillParent 1 ContentSize
									, margins	= Nothing
									}

defaultInteractionIcon :: !(Maybe InteractionTaskType) !Bool !Bool -> PanelIcon
defaultInteractionIcon type isControlTask localInteraction
	| isControlTask	= "icon-control-task"
	= case type of
		Nothing = ""
		Just type = case type of
			OutputTask _ | not localInteraction	= "icon-monitor-task"
			OutputTask PassiveOutput			= "icon-message-task"
			OutputTask ActiveOutput				= "icon-instruction-task"
			UpdateTask							= "icon-update-task"
			InputTask							= "icon-input-task"