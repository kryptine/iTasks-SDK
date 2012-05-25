implementation module TUIEncode

import StdMisc, StdList, JSON_NG, SystemTypes, TUIDefinition, TUIDiff

encodeTUIDefinition :: !TUIDef -> JSONNode
encodeTUIDefinition d = toJSON d

//TUI DEFINITIONS
derive JSONEncode TUIButton, TUIIcon, TUIHtml, Hotkey
derive JSONEncode TUIButtonControl
derive JSONEncode TUIContainer, TUIPanel, TUIWindow, TUITabContainer, TUIBorderContainer, TUIListContainer
derive JSONEncode TUIGridControl, TUITree, TUIEditControl, TUIShowControl, TUISliderControl
derive JSONEncode TUITasklet

JSONEncode{|TUIDef|} {content,width,height,margins}
	= merge (JSONEncode{|*|} content) (sizeAttributes width height margins)

JSONEncode{|TUIDefContent|} (TUIEditControl c b)		= merge (encodeControlType editprefix c) (JSONEncode{|*|} b)
JSONEncode{|TUIDefContent|} (TUIShowControl c b)		= merge (encodeControlType showprefix c) (JSONEncode{|*|} b)
JSONEncode{|TUIDefContent|} (TUIPanel r)				= addXType "itasks_panel" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIContainer r)			= addXType "itasks_container" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIWindow r)				= addXType "itasks_window" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUITabContainer r)			= addXType "itasks_tab_container" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUITabItem r)				= JSONEncode{|*|} r
JSONEncode{|TUIDefContent|} (TUIBorderContainer r)		= addXType "iborderc" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIBorderItem r)			= addXType "iborderi" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIListContainer r) 		= addXType "itasks_list_container" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIListItem r)				= JSONEncode{|*|} r
JSONEncode{|TUIDefContent|} (TUIRadioChoice r)			= JSONEncode{|*|} r
JSONEncode{|TUIDefContent|} (TUICheckChoice r)			= JSONEncode{|*|} r
JSONEncode{|TUIDefContent|} (TUIIcon r)					= addXType "itasks_icon" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIHtml r)					= addXType "itasks_html" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIButton r)				= addXType "itasks_button" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIMenuButton r) 			= JSONEncode{|*|} r
JSONEncode{|TUIDefContent|} (TUIMenuItem r) 			= JSONEncode{|*|} r
JSONEncode{|TUIDefContent|} (TUITasklet r)				= addXType "itasks_tasklet" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUITaskletPlaceholder tid)	= [JSONObject [("xtype",JSONString "itasks_widget_placeholder"),("taskId",toJSON tid)]]

JSONEncode{|TUIDefContent|} (TUICustom r)				= [r]

JSONEncode{|TUIListItem|}  {TUIListItem|items,index}
	= [JSONObject [("xtype",JSONString "itasks_list_item"),("index",JSONInt index),("items", toJSON items)]]

JSONEncode{|TUITabItem|} {TUITabItem|taskId,listId,items,title,iconCls,padding,menus,closeAction}
	= [JSONObject (filterNull [("xtype",JSONString "itasks_tab_item"),("taskId",toJSON taskId),("listId",toJSON listId),("items",toJSON items),("title",toJSON title),("iconCls",toJSON iconCls)
				  ,("padding",toJSON padding),("menus",toJSON menus),("closeAction",toJSON closeAction)])]

JSONEncode{|TUIBorderItem|} {TUIBorderItem|title,iconCls,item}
	= [JSONObject [("xtype",JSONString "iborderi"),("title",toJSON title),("iconCls",toJSON iconCls),("items",toJSON item)]]

JSONEncode{|TUIMenuButton|} {TUIMenuButton|text,target,action,disabled,iconCls,menu}
	= 	[JSONObject (filterNull [("xtype",JSONString "itasks_menu_button"),("text",toJSON text),("target",toJSON target),("action",toJSON action)
					,("disabled",toJSON disabled),("iconCls",toJSON iconCls),("menu",toJSON menu)])]
JSONEncode{|TUIMenuItem|} {TUIMenuItem|text,target,action,disabled,iconCls,hotkey,menu}
	= 	[JSONObject	(filterNull [("xtype",JSONString "itasks_menu_item"),("text",toJSON text),("target",toJSON target),("action",toJSON action)
					,("disabled",toJSON disabled),("iconCls",toJSON iconCls),("hotkey",toJSON hotkey),("menu",toJSON menu)])
		]
JSONEncode{|TUIMenu|} {TUIMenu|items}
	= [JSONObject [("xtype",JSONString "menu"),("items", JSONArray (map toJSON items))]]

JSONEncode{|TUIRadioChoice|} {TUIRadioChoice|items,taskId,name,index,checked}
	= [JSONObject [("xtype",JSONString "itasks_radiochoice"),("items", JSONArray (map toJSON items))
					,("taskId",toJSON taskId),("name", JSONString name),("index",JSONInt index),("checked",JSONBool checked)]]

JSONEncode{|TUICheckChoice|} {TUICheckChoice|items,taskId,name,index,checked}
	= [JSONObject [("xtype",JSONString "itasks_checkchoice"),("items", JSONArray (map toJSON items))
					,("taskId",toJSON taskId),("name", JSONString name),("index",JSONInt index),("checked",JSONBool checked)]]

editprefix :== "itasks_edit_"
showprefix :== "itasks_show_"

encodeControlType prefix TUIStringControl				= justXType (prefix +++ "string")
encodeControlType prefix TUICharControl					= justXType (prefix +++ "char") 
encodeControlType prefix TUIIntControl					= justXType (prefix +++ "int")
encodeControlType prefix TUIRealControl					= justXType (prefix +++ "real")
encodeControlType prefix TUIBoolControl					= justXType (prefix +++ "bool")
encodeControlType prefix TUINoteControl					= justXType (prefix +++ "note")
encodeControlType prefix TUIDateControl					= justXType (prefix +++ "date")
encodeControlType prefix TUITimeControl					= justXType (prefix +++ "time")
encodeControlType prefix TUIPasswordControl				= justXType (prefix +++ "password")
encodeControlType prefix TUIUserControl					= justXType (prefix +++ "string")
encodeControlType prefix TUICurrencyControl				= justXType (prefix +++ "currency")
encodeControlType prefix (TUISliderControl r)			= addXType (prefix +++ "slider") (JSONEncode{|*|} r)
encodeControlType prefix (TUIDocumentControl r)			= justXType (prefix +++ "document")
encodeControlType prefix (TUIButtonControl r)			= addXType "itasks_button" (JSONEncode{|*|} r)
encodeControlType prefix (TUIComboControl r)			= [JSONObject [("xtype",JSONString "itasks_combo"),("options",toJSON r)]]
encodeControlType prefix (TUIGridControl r)				= addXType "itasks_grid" (JSONEncode{|*|} r)
encodeControlType prefix (TUITreeControl r)				= [JSONObject [("xtype",JSONString "itasks_tree"),("tree",toJSON r)]]
encodeControlType prefix (TUICustomControl xtype)		= justXType xtype

JSONEncode{|TUIDirection|} Horizontal	= [JSONString "horizontal"]
JSONEncode{|TUIDirection|} Vertical		= [JSONString "vertical"]
JSONEncode{|TUIHAlign|} AlignLeft		= [JSONString "left"]
JSONEncode{|TUIHAlign|} AlignCenter		= [JSONString "center"]
JSONEncode{|TUIHAlign|} AlignRight		= [JSONString "right"]
JSONEncode{|TUIVAlign|} AlignTop		= [JSONString "top"]
JSONEncode{|TUIVAlign|} AlignMiddle		= [JSONString "middle"]
JSONEncode{|TUIVAlign|} AlignBottom		= [JSONString "bottom"]
JSONEncode{|TUIMargins|} {TUIMargins|top,right,bottom,left} = [JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)]

sizeAttributes width height margins
	= [JSONObject (widthfields ++ heightfields ++ marginfields)]
where
	widthfields = sizefields "width" "minWidth" "hwrap" "hflex" width
	heightfields = sizefields "height" "minHeight" "vwrap" "vflex" height
	
	sizefields fixedField minField wrapField flexField size = case size of
		Nothing								= []
		Just (Fixed pixels)					= [(fixedField,JSONInt  pixels)]
		Just (WrapContent 0)				= [(wrapField,JSONBool True)] //Don't include minField if it is 0
		Just (WrapContent minsize)			= [(wrapField,JSONBool True),(minField, JSONInt minsize)]
		Just (FillParent weight minsize)	= [(flexField,JSONInt weight)] ++ case minsize of
												ContentSize			= [(wrapField,JSONBool True)]
												FixedMinSize 0		= [] //Don't include minField if it is 0
												FixedMinSize pixels	= [(minField, JSONInt pixels)]
		
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

filterNull :: [(!String,JSONNode)] -> [(!String,!JSONNode)]
filterNull fields = [(field,node) \\ (field,node) <- fields | node <> JSONNull]

encodeTUIUpdates :: ![TUIUpdate] -> JSONNode
encodeTUIUpdates updates = JSONArray (flatten (map encodeTUIUpdate updates))

encodeTUIUpdate :: TUIUpdate -> [JSONNode]
encodeTUIUpdate (TUISetValue path value)		= [node path "setEditValue"		[value]]
encodeTUIUpdate (TUISetTaskId path taskId)		= [node path "setTaskId"	 	[JSONString taskId]]
encodeTUIUpdate (TUISetName path name)			= [node path "setName"			[JSONString name]]
encodeTUIUpdate (TUISetEnabled path enabled)	= [node path "setDisabled"		[JSONBool (not enabled)]]
encodeTUIUpdate (TUISetTitle path (title,icon))	= [node path "setTitle"			[JSONString title: maybe [] (\x -> [JSONString x]) icon]]
encodeTUIUpdate (TUISetSize path (Just (Fixed width)) (Just (Fixed height)))
												= [node path "setSize"			[JSONInt width, JSONInt height]]
encodeTUIUpdate (TUISetSize _ _ _)				= [] //Only set size with fixed values
encodeTUIUpdate (TUISetActiveTab path index)	= [node path "setActiveTab"		[JSONInt index]]
encodeTUIUpdate (TUIReplace path index def)		= [node path "replace" 			[JSONInt index,toJSON def]]
encodeTUIUpdate (TUIUpdate path def)			= [node path "update"			[toJSON def]]
encodeTUIUpdate	(TUIAdd path index def)			= [node path "insert"			[JSONInt index, toJSON def]]
encodeTUIUpdate (TUIRemove path index)			= [node path "remove"			[JSONInt index]]

node path method arguments
	= JSONObject [("path",JSONString path),("method",JSONString method),("arguments",JSONArray arguments)]

