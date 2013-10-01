implementation module iTasks.Framework.UIDefinition

import Text.JSON, StdList, StdBool, StdTuple, GenEq, StdFunc, Text.HTML, Text, Data.Map, Data.List
from iTasks.API.Core.SystemTypes import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount(..), :: Action, :: Hotkey
	
defaultSizeOpts	:: UISizeOpts
defaultSizeOpts = {width = Nothing, minWidth = Nothing, height = Nothing, minHeight = Nothing, margins = Nothing}

defaultItemsOpts :: [UIControl] -> UIItemsOpts
defaultItemsOpts items = {items = items, direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing, baseCls=Nothing,bodyCls=Nothing}

defaultContainer :: ![UIControl] -> UIControl
defaultContainer items = UIContainer defaultSizeOpts (defaultItemsOpts items)

defaultPanel :: ![UIControl] -> UIControl
defaultPanel items = UIPanel defaultSizeOpts (defaultItemsOpts items) {UIPanelOpts|title=Nothing,frame=False,tbar=Nothing,hotkeys=Nothing,iconCls=Nothing}

defaultWindow :: ![UIControl] -> UIWindow
defaultWindow items = UIWindow defaultSizeOpts (defaultItemsOpts items) {UIWindowOpts|title=Nothing,tbar=Nothing,closeTaskId=Nothing,focusTaskId=Nothing,hotkeys=Nothing,iconCls=Nothing}

stringDisplay :: !String -> UIControl
stringDisplay value = UIViewString defaultSizeOpts {UIViewOpts|value = Just value}

uiDefAttributes	:: UIDef -> UIAttributes
uiDefAttributes (UIControlStack {UIControlStack|attributes})	    = attributes
uiDefAttributes (UIAttributeSet attributes)					        = attributes
uiDefAttributes (UISubUI {UISubUI|attributes})	                    = attributes
uiDefAttributes (UISubUIStack {UISubUIStack|attributes})	        = attributes
uiDefAttributes _													= newMap

uiDefControls :: UIDef -> [UIControl]
uiDefControls (UIControlStack {UIControlStack|controls})		    = map fst controls
uiDefControls (UISubUI {UISubUI|content})	                        = content.UIItemsOpts.items
uiDefControls (UIFinal (UIViewport content _))						= content.UIItemsOpts.items
uiDefControls _														= []

uiDefAnnotatedControls :: UIDef -> [(UIControl,UIAttributes)]
uiDefAnnotatedControls (UIControlStack {UIControlStack|controls})   = controls
uiDefAnnotatedControls (UISubUI {UISubUI|content})	                = [(c,newMap)\\c <- content.UIItemsOpts.items]
uiDefAnnotatedControls (UIFinal (UIViewport content _))			    = [(c,newMap)\\c <- content.UIItemsOpts.items]
uiDefAnnotatedControls _										    = []

uiDefActions :: UIDef -> [UIAction]
uiDefActions (UIActionSet actions)	        = actions
uiDefActions (UISubUI {UISubUI|actions})	= actions
uiDefActions _								= []

uiDefDirection :: UIDef -> UIDirection
uiDefDirection (UISubUI {UISubUI|content})	                    = content.UIItemsOpts.direction
uiDefDirection (UIFinal (UIViewport content _))	                = content.UIItemsOpts.direction
uiDefDirection _											    = Vertical

uiDefWindows :: UIDef -> [UIWindow]
uiDefWindows (UISubUI {UISubUI|windows})		                    = windows
uiDefWindows _													    = []

uiDefSetAttribute :: String String UIDef -> UIDef
uiDefSetAttribute key value (UIAttributeSet attributes)
	= UIAttributeSet (put key value attributes)
uiDefSetAttribute key value (UIControlStack stack=:{UIControlStack|attributes})
	= UIControlStack {UIControlStack|stack & attributes = put key value attributes}
uiDefSetAttribute key value (UISubUI sub=:{UISubUI|attributes})
	= UISubUI {UISubUI|sub & attributes = put key value attributes}
uiDefSetAttribute key value (UISubUIStack stack=:{UISubUIStack|attributes})
	= UISubUIStack {UISubUIStack|stack & attributes = put key value attributes}
uiDefSetAttribute key value def = def

uiDefSetDirection :: UIDirection UIDef -> UIDef
uiDefSetDirection direction (UISubUI sub) = UISubUI {UISubUI|sub & content = {UIItemsOpts|sub.content & direction = direction}}
uiDefSetDirection direction def = def

uiDefSetPadding :: Int Int Int Int UIDef -> UIDef
uiDefSetPadding top right bottom left (UISubUI sub) = UISubUI {UISubUI|sub & content = {UIItemsOpts|sub.content & padding = Just {top=top,right=right,bottom=bottom,left=left}}}
uiDefSetPadding _ _ _ _ def = def

uiDefSetBaseCls :: String UIDef -> UIDef
uiDefSetBaseCls baseCls (UISubUI sub) = UISubUI {UISubUI|sub & content = {UIItemsOpts|sub.content & baseCls = Just baseCls}}
uiDefSetBaseCls _ def = def

encodeUIDefinition :: !UIDef -> JSONNode
encodeUIDefinition (UIFinal (UIViewport iopts opts))	= enc "itwc_viewport" [toJSON iopts, encViewportOpts opts]
encodeUIDefinition def									= enc "itwc_viewport" [toJSON (defaultItemsOpts (uiDefControls def))]

encodeUIControl :: !UIControl -> JSONNode
encodeUIControl (UIViewString sopts vopts)				= enc "itwc_view_string" [toJSON sopts,encViewOpts vopts]
encodeUIControl (UIViewHtml sopts vopts)				= enc "itwc_view_html" [toJSON sopts, encViewOpts vopts]
encodeUIControl (UIViewDocument sopts vopts)			= enc "itwc_view_document" [toJSON sopts, encViewOpts vopts]
encodeUIControl (UIViewCheckbox sopts vopts)			= enc "itwc_view_checkbox" [toJSON sopts, encViewOpts vopts]
encodeUIControl (UIViewSlider sopts vopts opts)			= enc "itwc_view_slider" [toJSON sopts, encViewOpts vopts, toJSON opts]
encodeUIControl (UIViewProgress sopts vopts opts)		= enc "itwc_view_progress" [toJSON sopts, encViewOpts vopts, toJSON opts]
encodeUIControl (UIEditString sopts eopts)				= enc "itwc_edit_string" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditNote sopts eopts)				= enc "itwc_edit_note" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditPassword sopts eopts)			= enc "itwc_edit_password" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditInt sopts eopts)					= enc "itwc_edit_int" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditDecimal sopts eopts)				= enc "itwc_edit_decimal" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditCheckbox sopts eopts)			= enc "itwc_edit_checkbox" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditSlider sopts eopts opts)			= enc "itwc_edit_slider" [toJSON sopts, encEditOpts eopts, toJSON opts]
encodeUIControl (UIEditDate sopts eopts)				= enc "itwc_edit_date" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditTime sopts eopts)				= enc "itwc_edit_time" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditDocument sopts eopts)			= enc "itwc_edit_document" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditOryx sopts eopts opts)		    = enc "itwc_edit_oryx" [toJSON sopts, encEditOpts eopts, toJSON opts]
encodeUIControl (UIEditCode sopts eopts opts)			= enc "itwc_edit_code" [toJSON sopts, encEditOpts eopts, toJSON opts]
encodeUIControl (UIEditButton sopts eopts opts)			= enc "itwc_editbutton" [toJSON sopts, encEditOpts eopts, toJSON opts]
encodeUIControl (UIDropdown sopts copts)				= enc "itwc_choice_dropdown" [toJSON sopts, toJSON copts]
encodeUIControl (UIRadioGroup sopts copts)				= enc "itwc_choice_radiogroup" [toJSON sopts, toJSON copts]
encodeUIControl (UICheckboxGroup sopts copts)			= enc "itwc_choice_checkboxgroup" [toJSON sopts, toJSON copts]
encodeUIControl (UIGrid sopts copts opts)				= enc "itwc_choice_grid" [toJSON sopts, toJSON copts, toJSON opts]
encodeUIControl (UITree sopts copts opts)				= enc "itwc_choice_tree" [toJSON sopts, toJSON copts, toJSON opts]
encodeUIControl (UIActionButton sopts aopts opts)		= enc "itwc_actionbutton" [toJSON sopts, toJSON aopts, toJSON opts]
encodeUIControl (UIMenuButton sopts opts)				= enc "itwc_menubutton" [toJSON sopts, toJSON opts]
encodeUIControl (UILabel sopts opts)					= enc "itwc_label" [toJSON sopts, toJSON opts]
encodeUIControl (UIIcon sopts opts)						= enc "itwc_icon" [toJSON sopts, toJSON opts]
encodeUIControl (UIContainer sopts iopts)			    = enc "itwc_container" [toJSON sopts, toJSON iopts]
encodeUIControl (UIPanel sopts iopts opts)				= enc "itwc_panel" [toJSON sopts, toJSON iopts, toJSON opts]
encodeUIControl (UIFieldSet sopts iopts opts)			= enc "itwc_fieldset" [toJSON sopts, toJSON iopts, toJSON opts]
encodeUIControl (UITabSet sopts opts)					= enc "itwc_tabset" [toJSON sopts, encTabSetOpts opts]
encodeUIControl (UITasklet sopts opts)					= enc "itwc_tasklet" [toJSON sopts, toJSON opts]
encodeUIControl (UITaskletPH sopts opts)				= enc "itwc_tasklet_placeholder" [toJSON sopts, toJSON opts]
encodeUIControl (UIEditlet sopts opts)					= enc "itwc_edit_editlet" [toJSON sopts, toJSON opts]

encodeUIWindow :: !UIWindow -> JSONNode
encodeUIWindow (UIWindow sopts iopts opts)				= enc "itwc_window" [toJSON sopts, toJSON iopts, toJSON opts]

encodeUITab :: !UITab -> JSONNode
encodeUITab (UITab iopts opts) 							= enc "itwc_tabitem" [toJSON iopts,toJSON opts]

derive JSONEncode UISizeOpts, UIViewOpts, UIChoiceOpts, UIActionOpts, UIItemsOpts
derive JSONEncode UISliderOpts, UIProgressOpts, UICodeOpts, UIGridOpts, UITreeOpts, UIButtonOpts, UITreeNode, UILabelOpts
derive JSONEncode UIIconOpts, UIOryxOpts
derive JSONEncode UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UITabOpts
derive JSONEncode UITaskletOpts, UITaskletPHOpts, UIEditletOpts

JSONEncode{|UISideSizes|} {top,right,bottom,left}
	= [JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)]

JSONEncode{|UISize|} (ExactSize s)		= [JSONInt s]
JSONEncode{|UISize|} WrapSize			= [JSONString "wrap"] 
JSONEncode{|UISize|} FlexSize			= [JSONString "flex"] 

JSONEncode{|UIMinSize|} (ExactMin s)	= [JSONInt s]
JSONEncode{|UIMinSize|} WrapMin			= [JSONString "wrap"]

JSONEncode{|UIVAlign|} AlignTop			= [JSONString "top"]
JSONEncode{|UIVAlign|} AlignMiddle		= [JSONString "middle"]
JSONEncode{|UIVAlign|} AlignBottom		= [JSONString "bottom"]

JSONEncode{|UIHAlign|} AlignLeft		= [JSONString "left"]
JSONEncode{|UIHAlign|} AlignCenter		= [JSONString "center"]
JSONEncode{|UIHAlign|} AlignRight		= [JSONString "right"]

JSONEncode{|UIDirection|} Vertical		= [JSONString "vertical"]
JSONEncode{|UIDirection|} Horizontal	= [JSONString "horizontal"]

JSONEncode{|UIMenuButtonOpts|} {UIMenuButtonOpts|text,iconCls,disabled,menu}
	= [JSONObject (text` ++ [("disabled",JSONBool disabled),("menu",menu`)] ++ iconCls`)]
where
	text`		= maybe [] (\s -> [("text",JSONString s)]) text
	iconCls`	= maybe [] (\s -> [("iconCls",JSONString s)]) iconCls
	menu`= JSONObject [("xtype",JSONString "itwc_menu"),("items",JSONArray (map toJSON menu))]

JSONEncode{|UIMenuItem|} (UIActionMenuItem aopts opts)	= [enc "itwc_actionmenuitem" [toJSON aopts,toJSON opts]]
JSONEncode{|UIMenuItem|} (UISubMenuItem opts) 			= [enc "itwc_submenuitem" [toJSON opts]]

JSONEncode{|UIControl|} control = [encodeUIControl control]

JSONEncode{|UIDef|} uidef = [encodeUIDefinition uidef]

enc :: String [JSONNode] -> JSONNode
enc xtype opts = JSONObject [("xtype",JSONString xtype):optsfields]
where
	optsfields = flatten [fields \\ JSONObject fields <- opts]

//Special cases
encViewOpts :: (UIViewOpts a) -> JSONNode | encodeUIValue a
encViewOpts {UIViewOpts|value}
	= JSONObject [("value",encodeUIValue value)]

encEditOpts :: UIEditOpts  -> JSONNode
encEditOpts {UIEditOpts|taskId,editorId,value}
	= JSONObject ([("taskId",JSONString taskId),("editorId",JSONString editorId)] ++ maybe [] (\v -> [("value",v)]) value)

encViewportOpts :: UIViewportOpts -> JSONNode
encViewportOpts {UIViewportOpts|title,hotkeys,windows}
	= JSONObject (
		[("xtype",JSONString "itwc_viewport"),("windows",JSONArray [encodeUIWindow w \\ w <- windows])]	++
		maybe [] (\t -> [("title",JSONString t)]) title ++
		maybe [] (\k -> [("hotkeys",toJSON k)]) hotkeys
		)
encTabSetOpts :: UITabSetOpts -> JSONNode
encTabSetOpts {UITabSetOpts|items}
	= JSONObject [("items",JSONArray [encodeUITab i \\ i <- items])]

class encodeUIValue a :: a -> JSONNode
instance encodeUIValue String			where encodeUIValue v = JSONString v
instance encodeUIValue Int				where encodeUIValue v = JSONInt v
instance encodeUIValue Real				where encodeUIValue v = JSONReal v
instance encodeUIValue Bool				where encodeUIValue v = JSONBool v
instance encodeUIValue Document			where encodeUIValue v = toJSON v
instance encodeUIValue Date				where encodeUIValue v = toJSON v
instance encodeUIValue Time				where encodeUIValue v = toJSON v
instance encodeUIValue HtmlTag			where encodeUIValue v = JSONString (toString v)
instance encodeUIValue ProgressAmount
where
	encodeUIValue  ProgressUndetermined = JSONString "undetermined"
	encodeUIValue (ProgressRatio ratio)	= JSONReal ratio

instance encodeUIValue JSONNode			where encodeUIValue v = toJSON v
instance encodeUIValue (Maybe a) | encodeUIValue a
where
	encodeUIValue Nothing = JSONNull
	encodeUIValue (Just a) = encodeUIValue a
