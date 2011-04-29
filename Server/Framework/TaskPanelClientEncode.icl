implementation module TaskPanelClientEncode

import StdMisc, StdList, JSON, Types, TaskPanel

clientEncodeTaskPanel :: !TaskPanel -> JSONNode
clientEncodeTaskPanel p = toJSON p

derive JSONEncode TTCInteractiveContainer
derive JSONEncode TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Key, Hotkey
derive JSONEncode TUIConstructorControl
derive JSONEncode TUIButtonControl, TUIListItem, TUIChoiceControl
derive JSONEncode TUIFormContainer, TUILayoutContainer, TUIRecordContainer, TUIListContainer
derive JSONEncode TUIGridContainer, TUIGridColumn, TUITree, TUIControl
derive JSONEncode TUIOrientation, TUISize, TUIHGravity, TUIVGravity, TUIMinSize, TUIMargins

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (TaskDone)					= [JSONString "done"]
JSONEncode{|TaskPanel|} (TaskRedundant)				= [JSONString "redundant"]
JSONEncode{|TaskPanel|} (TaskNotDone)				= [JSONString "notdone"]
JSONEncode{|TaskPanel|} (TTCInteractiveContainer x)	= JSONEncode{|*|} x

JSONEncode{|TUIDef|}		{content,width,height,margins} = merge (JSONEncode{|*|} content) [JSONObject [("width",toJSON width),("height",toJSON height),("margins",toJSON margins)]]

JSONEncode{|TUIDefContent|} (TUIControl c b)			= merge (JSONEncode{|*|} c) (JSONEncode{|*|} b)
JSONEncode{|TUIDefContent|} (TUIButton r)				= addXType "itasks.tui.Button" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIListItem r) 			= addXType "itasks.tui.list.Item" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIGridContainer r)		= addXType "itasks.tui.Grid" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIFormContainer r)		= addXType "itasks.tui.FormContainer" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUILayoutContainer r)		= addXType "itasks.tui.LayoutContainer" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIRecordContainer r)		= addXType "itasks.tui.Record" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIListContainer r) 		= addXType "itasks.tui.List" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIMenuButton r)			= addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIMenuItem r)				= addXType "itasks.ttc.MenuItem" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIMenuSeparator)			= justXType "menuseparator"
JSONEncode{|TUIDefContent|} (TUICustom r)				= [r]

JSONEncode{|TUIControlType|} TUIStringControl			= justXType "itasks.tui.String"
JSONEncode{|TUIControlType|} TUICharControl				= justXType "itasks.tui.Char"
JSONEncode{|TUIControlType|} TUIIntControl				= justXType "itasks.tui.Int"
JSONEncode{|TUIControlType|} TUIRealControl				= justXType "itasks.tui.Real"
JSONEncode{|TUIControlType|} TUIBoolControl				= justXType "itasks.tui.Bool"
JSONEncode{|TUIControlType|} TUINoteControl				= justXType "itasks.tui.Note"
JSONEncode{|TUIControlType|} TUIDateControl				= justXType "itasks.tui.Date"
JSONEncode{|TUIControlType|} TUITimeControl				= justXType "itasks.tui.Time"
JSONEncode{|TUIControlType|} TUIUserControl				= justXType "itasks.tui.Username"
JSONEncode{|TUIControlType|} TUIPasswordControl			= justXType "itasks.tui.Password"
JSONEncode{|TUIControlType|} (TUIChoiceControl r)		= addXType "itasks.tui.Choice" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} TUICurrencyControl			= justXType "itasks.tui.Currency"
JSONEncode{|TUIControlType|} TUIHtmlDisplay				= justXType "itasks.tui.Html"
JSONEncode{|TUIControlType|} (TUIButtonControl r)		= addXType "itasks.tui.FormButton" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUIDocumentControl doc)	= addXType "itasks.tui.Document" [JSONObject [("document",toJSON doc)]]
JSONEncode{|TUIControlType|} (TUIORYXControl url)		= addXType "itasks.tui.Oryx" [JSONObject [("stencilsetURL",JSONString url)]]
JSONEncode{|TUIControlType|} (TUITreeControl tree)		= addXType "itasks.tui.Tree" [JSONObject [("tuiTree",toJSON tree)]]
JSONEncode{|TUIControlType|} (TUIConstructorControl r)	= addXType "itasks.tui.Constructor" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUICustomControl xtype)	= justXType xtype

addXType :: !String ![JSONNode] -> [JSONNode]
addXType xtype [JSONObject fields: xs]	= [JSONObject [("xtype", JSONString xtype):fields] : xs]
addXType _ _							= abort "cannot add xtype"

justXType :: !String -> [JSONNode]
justXType xtype = [JSONObject [("xtype",JSONString xtype)]]

merge :: ![JSONNode] ![JSONNode] -> [JSONNode]
merge [JSONObject obja] [JSONObject objb]	= [JSONObject (obja ++ objb)]
merge _ _									= abort "two JSON objects required"
	
