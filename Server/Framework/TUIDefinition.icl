implementation module TUIDefinition

import JSON, StdList, StdBool, GenEq, StdMisc
from Types import :: Document, :: DocumentId, :: Hotkey, :: Key

//JSON Encoding of TUI definitions is directly encoded as JSON data.
derive JSONEncode TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Key, Hotkey
derive JSONEncode TUIBasicControl, TUICurrencyControl, TUIDocumentControl, TUIConstructorControl
derive JSONEncode TUIButtonControl, TUIListItemControl, TUIChoiceControl, TUIAppletControl
derive JSONEncode TUITupleContainer, TUIRecordContainer, TUIListContainer, TUIHtmlContainer, TUIGridControl, TUIGridColumn, TUITreeControl, TUITree

//TODO: Separate control elements from form-widgets
JSONEncode{|TUIDef|} (TUIButton r)				= addXType "itasks.ttc.Button" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUIMenuButton r)			= addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuItem r)			= addXType "itasks.ttc.MenuItem" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuSeparator)			= [JSONRaw "{\"xtype\":\"menuseparator\"}"]
JSONEncode{|TUIDef|} (TUICustom r)				= JSONEncode{|*|} r

JSONEncode{|TUIDef|} (TUIStringControl r)		= addXType "itasks.tui.String" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICharControl r)			= addXType "itasks.tui.Char" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIIntControl r)			= addXType "itasks.tui.Int" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRealControl r)			= addXType "itasks.tui.Real" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIBoolControl r)			= addXType "itasks.tui.Bool" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIChoiceControl r)		= addXType "itasks.tui.Choice" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUINoteControl r)			= addXType "itasks.tui.Note" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDateControl r)			= addXType "itasks.tui.Date" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITimeControl r)			= addXType "itasks.tui.Time" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHiddenControl r)		= addXType "itasks.tui.Hidden" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIFormButtonControl r)	= addXType "itasks.tui.FormButton" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICurrencyControl r)		= addXType "itasks.tui.Currency" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIUserControl r)			= addXType "itasks.tui.Username" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIPasswordControl r)		= addXType "itasks.tui.Password" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDocumentControl r)	 	= addXType "itasks.tui.Document" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIConstructorControl r)	= addXType "itasks.tui.Constructor" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListItemControl r) 	= addXType "itasks.tui.list.Item" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIAppletControl r)		= addXType "itasks.tui.Applet" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIGridControl r)			= addXType "itasks.tui.Grid" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITreeControl r)			= addXType "itasks.tui.Tree" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUITupleContainer r)		= addXType "itasks.tui.Tuple" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRecordContainer r)		= addXType "itasks.tui.Record" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListContainer r) 		= addXType "itasks.tui.List" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHtmlContainer r)		= addXType "itasks.tui.Html" (JSONEncode{|*|} r)

addXType :: !String ![JSONNode] -> [JSONNode]
addXType xtype [JSONObject fields: xs]	= [JSONObject [("xtype", JSONString xtype):fields] : xs]
addXType xtype nodes					= nodes
	
getTUIId :: !TUIDef -> Maybe TUIId
getTUIId (TUIStringControl d)		= Just d.TUIBasicControl.id
getTUIId (TUICharControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIIntControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIRealControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIBoolControl d)			= Just d.TUIBasicControl.id
getTUIId (TUINoteControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIDateControl d)			= Just d.TUIBasicControl.id
getTUIId (TUITimeControl d)			= Just d.TUIBasicControl.id
getTUIId (TUICurrencyControl d)		= Just d.TUICurrencyControl.id
getTUIId (TUIUserControl d)			= Just d.TUIBasicControl.id
getTUIId (TUIPasswordControl d)		= Just d.TUIBasicControl.id
getTUIId (TUIDocumentControl d)		= Just d.TUIDocumentControl.id
getTUIId (TUIConstructorControl d)	= Just d.TUIConstructorControl.id
getTUIId (TUIListItemControl d)		= Just d.TUIListItemControl.id
getTUIId (TUITupleContainer d)		= Just d.TUITupleContainer.id
getTUIId (TUIRecordContainer d)		= Just d.TUIRecordContainer.id
getTUIId (TUIListContainer d)		= Just d.TUIListContainer.id
getTUIId (TUILabel)					= Nothing
getTUIId (TUICustom d)				= Nothing
getTUIId _							= abort "unknown TUI Definition"
