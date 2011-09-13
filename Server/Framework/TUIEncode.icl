implementation module TUIEncode

import StdMisc, StdList, JSON, SystemTypes, TUIDefinition, TUIDiff

encodeTUIDefinition :: !TUIDef -> JSONNode
encodeTUIDefinition d = toJSON d

//TUI DEFINITIONS
derive JSONEncode TUIButton, Hotkey
derive JSONEncode TUIButtonControl, TUIListItem, TUIChoiceControl
derive JSONEncode TUIContainer, TUIPanel, TUITabContainer, TUIBorderContainer, TUIListContainer
derive JSONEncode TUIGridControl, TUITree, TUIControl

JSONEncode{|TUIDef|} {content,width,height,margins}
	= merge (JSONEncode{|*|} content) (sizeAttributes width height margins)

JSONEncode{|TUIDefContent|} (TUIControl c b)			= merge (JSONEncode{|*|} c) (JSONEncode{|*|} b)
JSONEncode{|TUIDefContent|} (TUIButton r)				= addXType "ibutton" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIContainer r)			= addXType "icontainer" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIPanel r)				= addXType "ipanel" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUITabContainer r)			= addXType "itabc" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUITabItem r)				= addXType "itabi" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIBorderContainer r)		= addXType "iborderc" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIBorderItem r)			= addXType "iborderi" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIListContainer r) 		= addXType "ilistc" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIListItem r) 			= addXType "ilisti" (JSONEncode{|*|} r)
JSONEncode{|TUIDefContent|} (TUIMenuButton r) 			= JSONEncode{|*|} r
JSONEncode{|TUIDefContent|} (TUIMenuItem r) 			= JSONEncode{|*|} r

JSONEncode{|TUIDefContent|} (TUICustom r)				= [r]

JSONEncode{|TUITabItem|} {TUITabItem|title,iconCls,items,menus,closeAction}
	= [JSONObject [("xtype",JSONString "itabi"),("title",toJSON title),("iconCls",toJSON iconCls)
				  ,("items",toJSON items),("menus",toJSON menus),("closeAction",toJSON closeAction)]]

JSONEncode{|TUIBorderItem|} {TUIBorderItem|title,iconCls,item}
	= [JSONObject [("xtype",JSONString "iborderi"),("title",toJSON title),("iconCls",toJSON iconCls),("items",toJSON item)]]

JSONEncode{|TUIMenuButton|} {TUIMenuButton|text,target,action,disabled,iconCls,menu}
	= 	[JSONObject (filterNull [("xtype",JSONString "imenub"),("text",toJSON text),("target",toJSON target),("action",toJSON action)
					,("disabled",toJSON disabled),("iconCls",toJSON iconCls),("menu",toJSON menu)])]
JSONEncode{|TUIMenuItem|} {TUIMenuItem|text,target,action,disabled,iconCls,hotkey,menu}
	= 	[JSONObject	(filterNull [("xtype",JSONString "imenui"),("text",toJSON text),("target",toJSON target),("action",toJSON action)
					,("disabled",toJSON disabled),("iconCls",toJSON iconCls),("hotkey",toJSON hotkey),("menu",toJSON menu)])
		]
JSONEncode{|TUIMenu|} {TUIMenu|items}
	= [JSONObject [("xtype",JSONString "menu"),("items", JSONArray (map toJSON items))]]

JSONEncode{|TUIControlType|} TUIStringControl			= justXType "istring"
JSONEncode{|TUIControlType|} TUICharControl				= justXType "ichar"
JSONEncode{|TUIControlType|} TUIIntControl				= justXType "iint"
JSONEncode{|TUIControlType|} TUIRealControl				= justXType "ireal"
JSONEncode{|TUIControlType|} TUIBoolControl				= justXType "ibool"
JSONEncode{|TUIControlType|} TUINoteControl				= justXType "inote"
JSONEncode{|TUIControlType|} TUIDateControl				= justXType "idate"
JSONEncode{|TUIControlType|} TUITimeControl				= justXType "itime"
JSONEncode{|TUIControlType|} TUIUserControl				= justXType "istring"
JSONEncode{|TUIControlType|} TUIPasswordControl			= justXType "ipassword"
JSONEncode{|TUIControlType|} TUICurrencyControl			= justXType "icurrency"
JSONEncode{|TUIControlType|} (TUIHtmlDisplay tooltip)	= addXType "ihtml" [JSONObject [("tooltip",toJSON tooltip)]]
JSONEncode{|TUIControlType|} (TUIChoiceControl r)		= addXType "ichoice" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUIComboControl options)	= addXType "icombo" [JSONObject [("options",toJSON options)]]
JSONEncode{|TUIControlType|} (TUIButtonControl r)		= addXType "ibutton" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUIDocumentControl doc)	= addXType "itasks.tui.Document" [JSONObject [("document",toJSON doc)]]
JSONEncode{|TUIControlType|} (TUIORYXControl url)		= addXType "itasks.tui.Oryx" [JSONObject [("stencilsetURL",JSONString url)]]
JSONEncode{|TUIControlType|} (TUIGridControl r)			= addXType "igrid" (JSONEncode{|*|} r)
JSONEncode{|TUIControlType|} (TUITreeControl tree)		= addXType "itree" [JSONObject [("tree",toJSON tree)]]
JSONEncode{|TUIControlType|} (TUICustomControl xtype)	= justXType xtype

JSONEncode{|TUIDirection|} Horizontal	= [JSONString "horizontal"]
JSONEncode{|TUIDirection|} Vertical		= [JSONString "vertical"]
JSONEncode{|TUIHGravity|} HGLeft		= [JSONString "left"]
JSONEncode{|TUIHGravity|} HGCenter		= [JSONString "center"]
JSONEncode{|TUIHGravity|} HGRight		= [JSONString "right"]
JSONEncode{|TUIVGravity|} VGTop			= [JSONString "top"]
JSONEncode{|TUIVGravity|} VGCenter		= [JSONString "middle"]
JSONEncode{|TUIVGravity|} VGBottom		= [JSONString "bottom"]
JSONEncode{|TUIMargins|} {TUIMargins|top,right,bottom,left} = [JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)]

sizeAttributes width height margins
	= [JSONObject (widthfields ++ heightfields ++ marginfields)]
where
	widthfields = sizefields "width" "minWidth" "hwrap" "hflex" width
	heightfields = sizefields "height" "minHeight" "vwrap" "vflex" height
	
	sizefields fixedField minField wrapField flexField size = case size of
		Fixed pixels				= [(fixedField,JSONInt  pixels)]
		WrapContent 0				= [(wrapField,JSONBool True)] //Don't include minField if it is 0
		WrapContent	minsize			= [(wrapField,JSONBool True),(minField, JSONInt minsize)]
		FillParent weight minsize	= [(flexField,JSONInt weight)] ++ case minsize of
											ContentSize			= [(wrapField,JSONBool True)]
											FixedMinSize 0		= [] //Don't include minField if it is 0
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

filterNull :: [(!String,JSONNode)] -> [(!String,!JSONNode)]
filterNull fields = [(field,node) \\ (field,node) <- fields | node <> JSONNull]


derive JSONEncode TUISize, TUIMinSize

encodeTUIUpdates :: ![TUIUpdate] -> JSONNode
encodeTUIUpdates updates = JSONArray (flatten (map encodeTUIUpdate updates))

encodeTUIUpdate :: TUIUpdate -> [JSONNode]
encodeTUIUpdate (TUISetValue path value)		= [node path "setValue"		[value]]
encodeTUIUpdate (TUISetTaskId path taskId)		= [node path "setTaskId"	 	[JSONString taskId]]
encodeTUIUpdate (TUISetName path name)			= [node path "setName"		[JSONString name]]
encodeTUIUpdate (TUISetEnabled path enabled)	= [node path "setDisabled"	[JSONBool (not enabled)]]
encodeTUIUpdate (TUISetTitle path (title,icon))	= [node path "setTitle"		[JSONString title: maybe [] (\x -> [JSONString x]) icon]]
encodeTUIUpdate (TUISetSize path (Fixed width) (Fixed height))
												= [node path "setSize"		[JSONInt width, JSONInt height]]
encodeTUIUpdate (TUISetSize _ _ _)				= [] //Only set size with fixed values
encodeTUIUpdate (TUIReplace path index def)		= [node path "remove" [JSONInt index], node path "add" [JSONInt index,toJSON def]]
encodeTUIUpdate (TUIUpdate path def)			= [node path "update"		[toJSON def]]
encodeTUIUpdate	(TUIAdd path index def)			= [node path "add"			[JSONInt index, toJSON def]]
encodeTUIUpdate (TUIRemove path index)			= [node path "remove"		[JSONInt index]]

node path method arguments
	= JSONObject [("path",JSONString path),("method",JSONString method),("arguments",JSONArray arguments)]

