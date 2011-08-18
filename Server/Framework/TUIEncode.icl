implementation module TUIEncode

import StdMisc, StdList, JSON, SystemTypes, TUIDefinition, TUIDiff

encodeTUIDefinition :: !TUIDef -> JSONNode
encodeTUIDefinition d = toJSON d

encodeTUIUpdates :: ![TUIUpdate] -> JSONNode
encodeTUIUpdates u = toJSON u

derive JSONEncode TUIUpdate
derive JSONEncode TUIButton, TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey
derive JSONEncode TUIButtonControl, TUIListItem, TUIChoiceControl
derive JSONEncode TUILayoutContainer, TUITabContainer, TUITab, TUIListContainer
derive JSONEncode TUIGridControl, TUITree, TUIControl


JSONEncode{|TUIDef|} {content,width,height,margins}
	= merge (JSONEncode{|*|} content) (sizeAttributes width height margins)

JSONEncode{|TUIDefContent|} (TUIControl c b)			= merge (JSONEncode{|*|} c) (JSONEncode{|*|} b)
JSONEncode{|TUIDefContent|} (TUIButton r)				= addXType "itasks.tui.Button" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIListItem r) 			= addXType "itasks.tui.list.Item" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUILayoutContainer r)		= addXType "itasks.tui.LayoutContainer" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUITabContainer r)			= addXType "itasks.tui.TabContainer" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUITab r)					= addXType "itasks.tui.Tab" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIListContainer r) 		= addXType "itasks.tui.List" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIMenuButton r)			= addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIMenuItem r)				= addXType "itasks.tui.MenuItem" (JSONEncode{|*|} r)
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
JSONEncode{|TUIControlType|} (TUIComboControl options)	= addXType "itasks.tui.Combo" [JSONObject [("options",toJSON options)]]
JSONEncode{|TUIControlType|} TUICurrencyControl			= justXType "itasks.tui.Currency"
JSONEncode{|TUIControlType|} (TUIHtmlDisplay tooltip)	= addXType "itasks.tui.Html" [JSONObject [("tooltip",toJSON tooltip)]]
JSONEncode{|TUIControlType|} (TUIButtonControl r)		= addXType "itasks.tui.FormButton" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUIDocumentControl doc)	= addXType "itasks.tui.Document" [JSONObject [("document",toJSON doc)]]
JSONEncode{|TUIControlType|} (TUIORYXControl url)		= addXType "itasks.tui.Oryx" [JSONObject [("stencilsetURL",JSONString url)]]
JSONEncode{|TUIControlType|} (TUIGridControl r)			= addXType "itasks.tui.Grid" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUITreeControl tree)		= addXType "itasks.tui.Tree" [JSONObject [("tuiTree",toJSON tree)]]
JSONEncode{|TUIControlType|} (TUICustomControl xtype)	= justXType xtype

JSONEncode{|TUIOrientation|} Horizontal = [JSONString "horizontal"]
JSONEncode{|TUIOrientation|} Vertical = [JSONString "vertical"]
JSONEncode{|TUIHGravity|} HGLeft = [JSONString "left"]
JSONEncode{|TUIHGravity|} HGCenter = [JSONString "center"]
JSONEncode{|TUIHGravity|} HGRight = [JSONString "right"]
JSONEncode{|TUIVGravity|} VGTop = [JSONString "top"]
JSONEncode{|TUIVGravity|} VGCenter = [JSONString "middle"]
JSONEncode{|TUIVGravity|} VGBottom = [JSONString "bottom"]
JSONEncode{|TUIMargins|} {TUIMargins|top,right,bottom,left} = [JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)]

sizeAttributes width height margins
	= [JSONObject (widthfields ++ heightfields ++ marginfields)]
where
	widthfields = sizefields "width" "minWidth" "hwrap" "hflex" width
	heightfields = sizefields "height" "minHeight" "vwrap" "vflex" height
	
	sizefields fixedField minField wrapField flexField size = case size of
		Fixed pixels				= [(fixedField,JSONInt  pixels)]
		WrapContent	minsize			= [(wrapField,JSONBool True),(minField, JSONInt minsize)]
		FillParent weight minsize	= [(flexField,JSONInt weight)] ++ case minsize of
											ContentSize			= [(wrapField,JSONBool True)]
											FixedMinSize pixels	= [(minField, JSONInt pixels)]
		Auto						= []

	marginfields = case margins of
		Nothing = []
		Just m	= [("margins",toJSON m)]
		
		

addXType :: !String ![JSONNode] -> [JSONNode]
addXType xtype [JSONObject fields: xs]	= [JSONObject [("xtype", JSONString xtype):fields] : xs]
addXType _ _							= abort "cannot add xtype"

justXType :: !String -> [JSONNode]
justXType xtype = [JSONObject [("xtype",JSONString xtype)]]

merge :: ![JSONNode] ![JSONNode] -> [JSONNode]
merge [JSONObject obja] [JSONObject objb]	= [JSONObject (obja ++ objb)]
merge _ _									= abort "two JSON objects required"

derive JSONEncode TUISize, TUIMinSize

