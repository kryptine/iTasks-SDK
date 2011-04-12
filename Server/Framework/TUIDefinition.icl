implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdMisc
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
