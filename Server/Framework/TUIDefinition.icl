implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdFunc, HTML
from Types import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractionTaskType(..), :: OutputTaskType(..)

htmlDisplay :: !html -> TUIDef | toString html
htmlDisplay html =	{ content	= TUIControl TUIHtmlDisplay
									{ TUIControl
									| name			= ""
									, value			= JSONString (toString html)
									, eventValue	= Nothing
									, taskId		= ""
									}
					, width		= Auto
					, height	= Auto
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
defaultInteractionLayout = \{title,description,editorParts,buttons,type,isControlTask,localInteraction,warning} -> defaultPanelDescr
	title
	(defaultInteractionIcon type isControlTask localInteraction)
	description
	warning
	(Fixed 700)
	(defaultContent editorParts buttons)
	
fullWidthInteractionLayout :: InteractionLayoutMerger
fullWidthInteractionLayout = \{title,description,editorParts,buttons,type,isControlTask,localInteraction,warning} -> defaultPanelDescr
	title
	(defaultInteractionIcon type isControlTask localInteraction)
	description
	warning
	(FillParent 1 ContentSize)
	(defaultContent editorParts buttons)
	
defaultContent :: ![TUIDef] ![TUIDef] -> [TUIDef]
defaultContent editor buttons = [defaultContentPanel (editorContainer ++ buttonContainer)]
where
	// also add editor container if editor is empty, it's needed as spacer such that buttons are placed at the bottom of the panel
	editorContainer			= [	{ content	= TUILayoutContainer (defaultLayoutContainer editor)
								, width		= FillParent 1 ContentSize
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
defaultParallelLayout = \{TUIParallel|title,description,items} -> defaultPanelDescr title "icon-parallel-task" description Nothing Wrap items

minimalParallelLayout :: ParallelLayoutMerger
minimalParallelLayout = \{TUIParallel|title,description,items} ->	{ content	= TUILayoutContainer (defaultLayoutContainer items)
																	, width		= Auto
																	, height	= Auto
																	, margins	= Nothing
																	}

defaultResultLayout :: ResultLayoutMerger
defaultResultLayout = \{TUIResult|title,description,result} -> defaultPanelDescr title "icon-task-result" description Nothing (Fixed 700) [defaultContentPanel [result]]

defaultPanelDescr :: !PanelTitle !PanelIcon !String !(Maybe String) !TUISize ![TUIDef] -> TUIDef
defaultPanelDescr title iconCls description mbWarning width form = defaultPanel title iconCls width [defaultDescriptionPanel description mbWarning:form]

defaultPanel :: !PanelTitle !PanelIcon !TUISize ![TUIDef] -> TUIDef
defaultPanel title iconCls width content =	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer content & title = Just title, iconCls = Just iconCls}
											, width		= width
											, height	= Auto
											, margins	= Just (sameMargins 10)
											}

defaultDescriptionPanel :: !String !(Maybe String) -> TUIDef
defaultDescriptionPanel descr mbWarning =	{ content	= TUILayoutContainer {TUILayoutContainer | defaultLayoutContainer [htmlDisplay descr:warning] & frame = True}
											, width		= FillParent 1 ContentSize
											, height	= Wrap
											, margins	= Nothing
											}
where
	warning = maybe [] (\w -> [htmlDisplay (DivTag [ClassAttr "x-invalid-icon"] [Text w])]) mbWarning

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
columnLayout :: !Int ![TUIDef] -> TUIDef
columnLayout nCols items
	# cols = repeatn nCols []
	# cols = columnLayout` items cols
	# cols = map (\col -> {content = TUILayoutContainer {defaultLayoutContainer col & orientation = Vertical}, width = Wrap, height = Wrap, margins = Nothing}) cols
	= {content = TUILayoutContainer {defaultLayoutContainer cols & orientation = Horizontal}, width = Wrap, height = Wrap, margins = Nothing}
where
	columnLayout` items cols = case splitAt nCols items of
		([],_)	= map reverse cols
		(row,r)	= columnLayout` r (map (\(item,col) -> [item:col]) (zip2 row cols))