implementation module TaskPanelClientEncode

import StdMisc, StdList, JSON, Types, TaskPanel

clientEncodeTaskPanel :: !TaskPanel -> JSONNode
clientEncodeTaskPanel p = toJSON p

derive JSONEncode TTCInteractiveContainer, InteractiveTaskType, TTCResultContainer, TTCParallelContainer
derive JSONEncode TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Key, Hotkey
derive JSONEncode TUIDocumentControl, TUIConstructorControl
derive JSONEncode TUIButtonControl, TUIListItem, TUIChoiceControl
derive JSONEncode TUIContainer, TUIRecordContainer, TUIListContainer
derive JSONEncode TUIGridContainer, TUIGridColumn, TUITree, TUIControl

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (TaskDone)					= [JSONString "done"]
JSONEncode{|TaskPanel|} (TaskRedundant)				= [JSONString "redundant"]
JSONEncode{|TaskPanel|} (TaskNotDone)				= [JSONString "notdone"]
JSONEncode{|TaskPanel|} (TTCInteractiveContainer x)	= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCResultContainer x)		= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCParallelContainer x)	= JSONEncode{|*|} x

JSONEncode{|TUIDef|} (TUIControl c b)			= merge (JSONEncode{|*|} c) (JSONEncode{|*|} b)
where
	merge [JSONObject obja] [JSONObject objb]	= [JSONObject (obja ++ objb)]
	merge _ _									= abort "two JSON objects required"
JSONEncode{|TUIDef|} (TUIButton r)				= addXType "itasks.tui.Button" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUIMenuButton r)			= addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuItem r)			= addXType "itasks.ttc.MenuItem" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuSeparator)			= [JSONRaw "{\"xtype\":\"menuseparator\"}"]

JSONEncode{|TUIDef|} (TUIConstructorControl r)	= addXType "itasks.tui.Constructor" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListItem r) 			= addXType "itasks.tui.list.Item" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIGridContainer r)		= addXType "itasks.tui.Grid" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUIContainer r)			= addXType "itasks.tui.Container" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRecordContainer r)		= addXType "itasks.tui.Record" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListContainer r) 		= addXType "itasks.tui.List" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICustom r)				= [r]

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
JSONEncode{|TUIControlType|} (TUICurrencyControl cur)	= addXType "itasks.tui.Currency" [JSONObject [("currencyLabel",JSONString cur)]]
JSONEncode{|TUIControlType|} TUIHtmlDisplay				= justXType "itasks.tui.Html"
JSONEncode{|TUIControlType|} (TUIButtonControl r)		= addXType "itasks.tui.FormButton" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUIDocumentControl r)		= addXType "itasks.tui.Document" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUIORYXControl url)		= addXType "itasks.tui.Oryx" [JSONObject [("stencilsetURL",JSONString url)]]
JSONEncode{|TUIControlType|} (TUITreeControl tree)		= addXType "itasks.tui.Tree" [JSONObject [("tuiTree",toJSON tree)]]
JSONEncode{|TUIControlType|} (TUICustomControl xtype r)	= addXType xtype [JSONObject r]

JSONEncode{|TUILayout|} Vertical = [JSONString "form"]
JSONEncode{|TUILayout|} (Horizontal al) = [JSONObject [("type",JSONString "hbox"),("pack",JSONString alignStr)]]
where
	alignStr = case al of
		HLeft	= "start"
		HCenter	= "center"
		HRight	= "end"

addXType :: !String ![JSONNode] -> [JSONNode]
addXType xtype [JSONObject fields: xs]	= [JSONObject [("xtype", JSONString xtype):fields] : xs]
addXType _ _							= abort "cannot add xtype"

justXType :: !String -> [JSONNode]
justXType xtype = [JSONObject [("xtype",JSONString xtype)]]
