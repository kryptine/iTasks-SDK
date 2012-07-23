implementation module UIDefinition

import JSON_NG, StdList, StdBool, StdTuple, GenEq_NG, StdFunc, HTML, Text, List_NG
from SystemTypes import :: Document, :: DocumentId, :: Date, :: Time
	
defaultSizeOpts	:: UISizeOpts
defaultSizeOpts = {width = Nothing, minWidth = Nothing, height = Nothing, minHeight = Nothing, margins = Nothing}

defaultLayoutOpts :: UILayoutOpts
defaultLayoutOpts = {direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing}

defaultContainer :: ![UIControl] -> UIControl
defaultContainer items = UIContainer defaultSizeOpts defaultLayoutOpts items {UIContainerOpts|purpose=Nothing,baseCls=Nothing,bodyCls=Nothing}

defaultPanel :: ![UIControl] -> UIControl
defaultPanel items = UIPanel defaultSizeOpts defaultLayoutOpts items {UIPanelOpts|title=Nothing,frame=False,tbar=[],purpose=Nothing,iconCls=Nothing,baseCls=Nothing,bodyCls=Nothing}

defaultWindow :: ![UIControl] -> UIControl
defaultWindow items = UIWindow defaultSizeOpts defaultLayoutOpts items {UIWindowOpts|title=Nothing,frame=False,tbar=[],purpose=Nothing,iconCls=Nothing,baseCls=Nothing,bodyCls=Nothing}

stringDisplay :: !String -> UIControl
stringDisplay value = UIViewString defaultSizeOpts {UIViewOpts|value = Just value}

encodeUIDefinition :: !UIControl -> JSONNode
encodeUIDefinition (UIViewString sopts vopts)			= enc "itwc_view_string" [toJSON sopts,toJSON vopts] []
encodeUIDefinition (UIViewHtml sopts vopts)				= enc "itwc_view_html" [toJSON sopts, toJSON vopts] []
encodeUIDefinition (UIViewDocument sopts vopts)			= enc "itwc_view_document" [toJSON sopts, toJSON vopts] []
encodeUIDefinition (UIViewCheckbox sopts vopts)			= enc "itwc_view_checkbox" [toJSON sopts, toJSON vopts] []
encodeUIDefinition (UIViewSlider sopts vopts opts)		= enc "itwc_view_slider" [toJSON sopts, toJSON vopts, toJSON opts] []
encodeUIDefinition (UIViewProgress sopts vopts opts)	= enc "itwc_view_progress" [toJSON sopts, toJSON vopts, toJSON opts] []
encodeUIDefinition (UIEditString sopts eopts)			= enc "itwc_edit_string" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditNote sopts eopts)				= enc "itwc_edit_note" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditPassword sopts eopts)			= enc "itwc_edit_password" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditInt sopts eopts)				= enc "itwc_edit_int" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditDecimal sopts eopts)			= enc "itwc_edit_decimal" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditCheckbox sopts eopts)			= enc "itwc_edit_checkbox" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditSlider sopts eopts opts)		= enc "itwc_edit_slider" [toJSON sopts, toJSON eopts, toJSON opts] []
encodeUIDefinition (UIEditDate sopts eopts)				= enc "itwc_edit_date" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditTime sopts eopts)				= enc "itwc_edit_time" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditDocument sopts eopts)			= enc "itwc_edit_document" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIEditButton sopts eopts)			= enc "itwc_editbutton" [toJSON sopts, toJSON eopts] []
encodeUIDefinition (UIDropdown sopts copts)				= enc "itwc_choice_dropdown" [toJSON sopts, toJSON copts] []
encodeUIDefinition (UIGrid sopts copts opts)			= enc "itwc_choice_grid" [toJSON sopts, toJSON copts, toJSON opts] []
encodeUIDefinition (UITree sopts copts)					= enc "itwc_choice_tree" [toJSON sopts, toJSON copts] []
encodeUIDefinition (UIActionButton sopts aopts opts)	= enc "itwc_actionbutton" [toJSON sopts, toJSON aopts, toJSON opts] []
encodeUIDefinition (UIMenuButton sopts opts)			= enc "itwc_menubutton" [toJSON sopts, toJSON opts] []
encodeUIDefinition (UILabel sopts opts)					= enc "itwc_label" [toJSON sopts, toJSON opts] []
encodeUIDefinition (UIIcon sopts opts)					= enc "itwc_icon" [toJSON sopts, toJSON opts] []
encodeUIDefinition (UITab sopts opts)					= enc "itwc_tab" [toJSON sopts, toJSON opts] []
encodeUIDefinition (UITasklet sopts opts)				= enc "itwc_tasklet" [toJSON sopts, toJSON opts] []
encodeUIDefinition (UIContainer sopts lopts items opts)	= enc "itwc_container" [toJSON sopts, toJSON lopts, toJSON opts] items
encodeUIDefinition (UIPanel sopts lopts items opts)		= enc "itwc_panel" [toJSON sopts, toJSON lopts, toJSON opts] items
encodeUIDefinition (UIFieldSet sopts lopts items opts)	= enc "itwc_fieldset" [toJSON sopts, toJSON lopts, toJSON opts] items
encodeUIDefinition (UIWindow sopts lopts items opts)	= enc "itwc_window" [toJSON sopts, toJSON lopts, toJSON opts] items

derive JSONEncode UISizeOpts, UIViewOpts, UIEditOpts, UIChoiceOpts, UIActionOpts, UILayoutOpts
derive JSONEncode UISliderOpts, UIProgressOpts, UIGridOpts, UIActionButtonOpts, UITreeNode, UILabelOpts
derive JSONEncode UIIconOpts, UITabOpts, UITaskletOpts
derive JSONEncode UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts

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
	= [JSONObject [("text",JSONString text),("disabled",JSONBool disabled),("menu",menu`):icon]]
where
	icon = [("iconCls",JSONString cls) \\ Just cls <- [iconCls]]
	menu`= JSONObject [("xtype",JSONString "itwc_menu"),("items",JSONArray (map toJSON menu))]

JSONEncode{|UIMenuItem|} (UIActionMenuItem aopts opts)	= [enc "itwc_actionmenuitem" [toJSON aopts,toJSON opts] []]
JSONEncode{|UIMenuItem|} (UISubMenuItem opts) 			= [enc "itwc_submenuitem" [toJSON opts] []]

JSONEncode{|UIControl|} control = [encodeUIDefinition control]

enc :: String [JSONNode] [UIControl] -> JSONNode
enc xtype opts items = JSONObject [("xtype",JSONString xtype):optsfields ++ itemsfield]
where
	optsfields = flatten [fields \\ JSONObject fields <- opts]
	itemsfield = case items of
		[]	= []
		_	= [("items",JSONArray (map encodeUIDefinition items))]
