implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdFunc
from Types import :: Document, :: DocumentId, :: Hotkey, :: TaskId

htmlDisplay :: !(Maybe String) !String -> TUIDef
htmlDisplay mbLabel html = TUIControl TUIHtmlDisplay
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

simpleContainer :: ![TUIDef] -> TUIContainer
simpleContainer items =	{ items				= items
						, fieldLabel		= Nothing
						, optional			= False
						, cls				= Nothing
						, layout			= Vertical
						, restrictedWidth	= False
						}

defaultInteractiveLayout :: InteractiveLayoutMerger
defaultInteractiveLayout = (\{title,description,mbContext,editor,buttons} -> defaultPanel
		title
		description
		(maybeToList mbContext ++ editor ++ [TUIContainer {simpleContainer buttons & layout = Horizontal HRight}])
	)
	
fullWidthInteractiveLayout :: InteractiveLayoutMerger
fullWidthInteractiveLayout = (\(TUIContainer c) -> TUIContainer {c & restrictedWidth = False}) o defaultInteractiveLayout

defaultParallelLayout :: ParallelLayoutMerger
defaultParallelLayout = (\{TUIParallel|title,description,items} -> TUIContainer (simpleContainer [defaultTitlePanel title, defaultDescriptionPanel description, defaultContentPanel items]))

minimalParallelLayout :: ParallelLayoutMerger
minimalParallelLayout = \{TUIParallel|title,description,items} -> TUIContainer (simpleContainer items)

defaultResultLayout :: ResultLayoutMerger
defaultResultLayout = (\{TUIResult|title,description,result} -> defaultPanel title description [result])

defaultPanel :: !TUIDef !TUIDef ![TUIDef] -> TUIDef
defaultPanel title description form = TUIContainer {simpleContainer [defaultTitlePanel title, defaultDescriptionPanel description, defaultContentPanel form] & restrictedWidth = True}

defaultTitlePanel :: !TUIDef -> TUIDef
defaultTitlePanel title = TUIContainer {TUIContainer | simpleContainer [title] & cls = Just "TTCSubject"}

defaultDescriptionPanel :: !TUIDef -> TUIDef
defaultDescriptionPanel descr = TUIContainer {TUIContainer | simpleContainer [descr] & cls = Just "TTCDescription"}

defaultContentPanel :: ![TUIDef] -> TUIDef
defaultContentPanel content = TUIContainer {TUIContainer | simpleContainer content & cls = Just "TTCPanel"}
