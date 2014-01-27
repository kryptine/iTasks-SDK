implementation module iTasks.Framework.UIDefinition

import Text.JSON, StdList, StdBool, StdTuple, GenEq, StdFunc, Text.HTML, Text, Data.Map, Data.List
from iTasks.API.Core.SystemTypes import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount(..), :: Action, :: Hotkey
	
defaultSizeOpts :: UISizeOpts
defaultSizeOpts = {UISizeOpts|width = Nothing, minWidth = Nothing, maxWidth = Nothing, height = Nothing, minHeight = Nothing, maxHeight = Nothing, margins = Nothing}

defaultHSizeOpts :: UIHSizeOpts
defaultHSizeOpts = {UIHSizeOpts|width = Nothing, minWidth = Nothing, maxWidth = Nothing, margins = Nothing}

defaultFSizeOpts :: UIFSizeOpts
defaultFSizeOpts = {UIFSizeOpts|margins = Nothing}

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

hasFSizeOpts :: !UIControl -> Bool
hasFSizeOpts (UIViewCheckbox sOpts vOpts)	= True
hasFSizeOpts (UIEditCheckbox sOpts eOpts)	= True
hasFSizeOpts (UIIcon sOpts opts)			= True
hasFSizeOpts _                              = False

getFSizeOpts :: (UIFSizeOpts -> a) UIControl -> a
getFSizeOpts f (UIViewCheckbox sOpts vOpts)			= f sOpts
getFSizeOpts f (UIEditCheckbox sOpts eOpts)			= f sOpts
getFSizeOpts f (UIIcon sOpts opts)					= f sOpts

setFSizeOpts :: (UIFSizeOpts -> UIFSizeOpts) UIControl -> UIControl
setFSizeOpts f (UIViewCheckbox sOpts vOpts)			= (UIViewCheckbox (f sOpts) vOpts)
setFSizeOpts f (UIEditCheckbox sOpts eOpts)			= (UIEditCheckbox (f sOpts) eOpts)
setFSizeOpts f (UIIcon sOpts opts)					= (UIIcon (f sOpts) opts)

hasSizeOpts :: !UIControl -> Bool
hasSizeOpts (UIViewString	sOpts vOpts)		= True
hasSizeOpts (UIViewHtml sOpts vOpts)			= True
hasSizeOpts (UIEditNote sOpts eOpts)			= True
hasSizeOpts (UIEditButton	sOpts eOpts opts)	= True
hasSizeOpts (UIRadioGroup sOpts cOpts)			= True
hasSizeOpts (UICheckboxGroup sOpts cOpts)		= True
hasSizeOpts (UIGrid sOpts cOpts opts)			= True
hasSizeOpts (UITree sOpts cOpts opts)			= True
hasSizeOpts (UIActionButton sOpts aOpts opts)	= True
hasSizeOpts (UIMenuButton	sOpts opts)			= True
hasSizeOpts (UITasklet sOpts opts)				= True
hasSizeOpts (UIEditlet sOpts opts)              = True
hasSizeOpts (UIContainer sOpts iOpts)	        = True
hasSizeOpts (UIPanel sOpts iOpts opts)			= True
hasSizeOpts (UIFieldSet sOpts iOpts opts)		= True
hasSizeOpts (UITabSet sOpts opts)				= True
hasSizeOpts _                                   = False

getSizeOpts :: (UISizeOpts -> a) UIControl -> a
getSizeOpts f (UIViewString	sOpts vOpts)			= f sOpts
getSizeOpts f (UIViewHtml sOpts vOpts)				= f sOpts
getSizeOpts f (UIEditNote sOpts eOpts)				= f sOpts
getSizeOpts f (UIEditButton	sOpts eOpts opts)		= f sOpts
getSizeOpts f (UIRadioGroup sOpts cOpts)			= f sOpts
getSizeOpts f (UICheckboxGroup sOpts cOpts)			= f sOpts
getSizeOpts f (UIGrid sOpts cOpts opts)				= f sOpts
getSizeOpts f (UITree sOpts cOpts opts)				= f sOpts
getSizeOpts f (UIActionButton sOpts aOpts opts)		= f sOpts
getSizeOpts f (UIMenuButton	sOpts opts)				= f sOpts
getSizeOpts f (UITasklet sOpts opts)				= f sOpts
getSizeOpts f (UIEditlet sOpts opts)				= f sOpts
getSizeOpts f (UIContainer sOpts iOpts)	        	= f sOpts
getSizeOpts f (UIPanel sOpts iOpts opts)			= f sOpts
getSizeOpts f (UIFieldSet sOpts iOpts opts)			= f sOpts
getSizeOpts f (UITabSet sOpts opts)					= f sOpts

setSizeOpts :: (UISizeOpts -> UISizeOpts) UIControl -> UIControl
setSizeOpts f (UIViewString	sOpts vOpts)			= (UIViewString	(f sOpts) vOpts)
setSizeOpts f (UIViewHtml sOpts vOpts)				= (UIViewHtml (f sOpts) vOpts)
setSizeOpts f (UIEditNote sOpts eOpts)				= (UIEditNote (f sOpts) eOpts)
setSizeOpts f (UIEditButton	sOpts eOpts opts)		= (UIEditButton	(f sOpts) eOpts opts)
setSizeOpts f (UIRadioGroup sOpts cOpts)			= (UIRadioGroup (f sOpts) cOpts)
setSizeOpts f (UICheckboxGroup sOpts cOpts)			= (UICheckboxGroup (f sOpts) cOpts)
setSizeOpts f (UIGrid sOpts cOpts opts)				= (UIGrid (f sOpts) cOpts opts)
setSizeOpts f (UITree sOpts cOpts opts)				= (UITree (f sOpts) cOpts opts)
setSizeOpts f (UIActionButton sOpts aOpts opts)		= (UIActionButton (f sOpts) aOpts opts)	
setSizeOpts f (UIMenuButton	sOpts opts)				= (UIMenuButton	(f sOpts) opts)	
setSizeOpts f (UITasklet sOpts opts)				= (UITasklet (f sOpts) opts)
setSizeOpts f (UIEditlet sOpts opts)				= (UIEditlet (f sOpts) opts)
setSizeOpts f (UIContainer sOpts iOpts)	        	= (UIContainer (f sOpts) iOpts)
setSizeOpts f (UIPanel sOpts iOpts opts)			= (UIPanel (f sOpts) iOpts opts)
setSizeOpts f (UIFieldSet sOpts iOpts opts)			= (UIFieldSet (f sOpts) iOpts opts)
setSizeOpts f (UITabSet sOpts opts)					= (UITabSet (f sOpts) opts)

hasHSizeOpts :: !UIControl -> Bool
hasHSizeOpts (UIViewDocument sOpts vOpts)	    = True
hasHSizeOpts (UIViewSlider sOpts vOpts opts)	= True
hasHSizeOpts (UIViewProgress sOpts vOpts opts)	= True
hasHSizeOpts (UIEditString	sOpts eOpts)		= True
hasHSizeOpts (UIEditPassword sOpts eOpts)		= True
hasHSizeOpts (UIEditInt sOpts eOpts)			= True
hasHSizeOpts (UIEditDecimal sOpts eOpts)		= True
hasHSizeOpts (UIEditSlider sOpts eOpts opts)	= True
hasHSizeOpts (UIEditDate sOpts eOpts)			= True
hasHSizeOpts (UIEditTime sOpts eOpts)			= True
hasHSizeOpts (UIEditDocument sOpts eOpts)		= True
hasHSizeOpts (UIDropdown sOpts cOpts)			= True
hasHSizeOpts (UILabel sOpts opts)				= True
hasHSizeOpts _                                  = False

getHSizeOpts :: (UIHSizeOpts -> a) UIControl -> a
getHSizeOpts f (UIViewDocument sOpts vOpts)	    = f sOpts
getHSizeOpts f (UIViewSlider sOpts vOpts opts)	= f sOpts
getHSizeOpts f (UIViewProgress sOpts vOpts opts)= f sOpts
getHSizeOpts f (UIEditString	sOpts eOpts)	= f sOpts
getHSizeOpts f (UIEditPassword sOpts eOpts)		= f sOpts
getHSizeOpts f (UIEditInt sOpts eOpts)			= f sOpts
getHSizeOpts f (UIEditDecimal sOpts eOpts)		= f sOpts
getHSizeOpts f (UIEditSlider sOpts eOpts opts)	= f sOpts
getHSizeOpts f (UIEditDate sOpts eOpts)			= f sOpts
getHSizeOpts f (UIEditTime sOpts eOpts)			= f sOpts
getHSizeOpts f (UIEditDocument sOpts eOpts)		= f sOpts
getHSizeOpts f (UIDropdown sOpts cOpts)			= f sOpts
getHSizeOpts f (UILabel sOpts opts)				= f sOpts

setHSizeOpts :: (UIHSizeOpts -> UIHSizeOpts) UIControl -> UIControl
setHSizeOpts f (UIViewDocument sOpts vOpts)	    = (UIViewDocument (f sOpts) vOpts)
setHSizeOpts f (UIViewSlider sOpts vOpts opts)	= (UIViewSlider (f sOpts) vOpts opts)
setHSizeOpts f (UIViewProgress sOpts vOpts opts)= (UIViewProgress (f sOpts) vOpts opts)
setHSizeOpts f (UIEditString sOpts eOpts)	    = (UIEditString	(f sOpts) eOpts)
setHSizeOpts f (UIEditPassword sOpts eOpts)		= (UIEditPassword (f sOpts) eOpts)
setHSizeOpts f (UIEditInt sOpts eOpts)			= (UIEditInt (f sOpts) eOpts)
setHSizeOpts f (UIEditDecimal sOpts eOpts)		= (UIEditDecimal (f sOpts) eOpts)
setHSizeOpts f (UIEditSlider sOpts eOpts opts)	= (UIEditSlider (f sOpts) eOpts opts)
setHSizeOpts f (UIEditDate sOpts eOpts)			= (UIEditDate (f sOpts) eOpts)
setHSizeOpts f (UIEditTime sOpts eOpts)			= (UIEditTime (f sOpts) eOpts)
setHSizeOpts f (UIEditDocument sOpts eOpts)		= (UIEditDocument (f sOpts) eOpts)
setHSizeOpts f (UIDropdown sOpts cOpts)			= (UIDropdown (f sOpts) cOpts)
setHSizeOpts f (UILabel sOpts opts)				= (UILabel (f sOpts) opts)

setSize :: !UISize !UISize !UIControl -> UIControl
setSize width height ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & width = Just width, height = Just height}) ctrl
    | hasHSizeOpts ctrl    = setHSizeOpts (\opts -> {UIHSizeOpts| opts & width = Just width}) ctrl
                           = ctrl

setWidth :: !UISize !UIControl -> UIControl
setWidth width ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & width = Just width}) ctrl
    | hasHSizeOpts ctrl    = setHSizeOpts (\opts -> {UIHSizeOpts| opts & width = Just width}) ctrl
                           = ctrl

setHeight :: !UISize !UIControl -> UIControl
setHeight height ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & height = Just height}) ctrl
                           = ctrl

setMinSize :: !UIBound !UIBound !UIControl -> UIControl
setMinSize minWidth minHeight ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just minWidth, minHeight = Just minHeight}) ctrl
    | hasHSizeOpts ctrl    = setHSizeOpts (\opts -> {UIHSizeOpts| opts & minWidth = Just minWidth}) ctrl
                           = ctrl

setMinWidth :: !UIBound !UIControl -> UIControl
setMinWidth minWidth ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just minWidth}) ctrl
    | hasHSizeOpts ctrl    = setHSizeOpts (\opts -> {UIHSizeOpts| opts & minWidth = Just minWidth}) ctrl
                           = ctrl

setMinHeight :: !UIBound !UIControl -> UIControl
setMinHeight minHeight ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & minHeight = Just minHeight}) ctrl
                           = ctrl

setMaxSize :: !UIBound !UIBound !UIControl -> UIControl
setMaxSize maxWidth maxHeight ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & maxWidth = Just maxWidth, maxHeight = Just maxHeight}) ctrl
    | hasHSizeOpts ctrl    = setHSizeOpts (\opts -> {UIHSizeOpts| opts & maxWidth = Just maxWidth}) ctrl
                           = ctrl

setMaxWidth :: !UIBound !UIControl -> UIControl
setMaxWidth maxWidth ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & maxWidth = Just maxWidth}) ctrl
    | hasHSizeOpts ctrl    = setHSizeOpts (\opts -> {UIHSizeOpts| opts & maxWidth = Just maxWidth}) ctrl
                           = ctrl

setMaxHeight :: !UIBound !UIControl -> UIControl
setMaxHeight maxHeight ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & maxHeight = Just maxHeight}) ctrl
                           = ctrl

fill :: !UIControl -> UIControl
fill ctrl = setSize FlexSize FlexSize ctrl

fillHeight :: !UIControl -> UIControl
fillHeight ctrl = setHeight FlexSize ctrl

fillWidth :: !UIControl -> UIControl
fillWidth ctrl = setWidth FlexSize ctrl

fixedHeight	:: !Int !UIControl -> UIControl
fixedHeight size ctrl = setHeight (ExactSize size) ctrl

fixedWidth :: !Int !UIControl -> UIControl
fixedWidth size ctrl = setWidth (ExactSize size) ctrl

wrapHeight :: !UIControl -> UIControl
wrapHeight ctrl = setHeight WrapSize ctrl

wrapWidth :: !UIControl -> UIControl
wrapWidth ctrl = setWidth WrapSize ctrl

setMargins :: !Int !Int !Int !Int !UIControl -> UIControl
setMargins top right bottom left ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts -> {UISizeOpts| opts & margins = Just margins}) ctrl
    | hasFSizeOpts ctrl    = setFSizeOpts (\opts -> {UIFSizeOpts| opts & margins = Just margins}) ctrl
    | hasHSizeOpts ctrl    = setHSizeOpts (\opts -> {UIHSizeOpts| opts & margins = Just margins}) ctrl
                           = ctrl
where
    margins = {top = top, right = right, bottom = bottom, left = left}

setMargin :: (UISideSizes -> UISideSizes) !UIControl ->UIControl
setMargin f ctrl
    | hasSizeOpts ctrl     = setSizeOpts (\opts=:{UISizeOpts|margins} -> {UISizeOpts| opts & margins = Just (f (fromMaybe {top = 0, right = 0, bottom = 0, left = 0} margins))}) ctrl
    | hasFSizeOpts ctrl    = setFSizeOpts (\opts=:{UIFSizeOpts|margins} -> {UIFSizeOpts| opts & margins = Just (f (fromMaybe {top = 0, right = 0, bottom = 0, left = 0} margins))}) ctrl
    | hasHSizeOpts ctrl    = setHSizeOpts (\opts=:{UIHSizeOpts|margins} -> {UIHSizeOpts| opts & margins = Just (f (fromMaybe {top = 0, right = 0, bottom = 0, left = 0} margins))}) ctrl
                           = ctrl

setTopMargin :: !Int !UIControl -> UIControl
setTopMargin top ctrl = setMargin (\m -> {m & top = top}) ctrl

setRightMargin	:: !Int !UIControl -> UIControl
setRightMargin right ctrl = setMargin (\m -> {m & right = right}) ctrl

setBottomMargin	:: !Int !UIControl -> UIControl
setBottomMargin bottom ctrl = setMargin (\m -> {m & bottom = bottom}) ctrl

setLeftMargin :: !Int !UIControl -> UIControl
setLeftMargin left ctrl = setMargin (\m -> {m & left = left}) ctrl

setPadding :: !Int !Int !Int !Int !UIControl -> UIControl
setPadding top right bottom left (UIContainer sOpts iOpts)
	= UIContainer sOpts {UIItemsOpts|iOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}}
setPadding top right bottom left (UIPanel sOpts iOpts opts)
	= UIPanel sOpts {UIItemsOpts|iOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} opts
setPadding top right bottom left ctrl = ctrl

setTitle :: !String !UIControl -> UIControl
setTitle title (UIPanel sOpts iOpts opts)		= UIPanel sOpts iOpts {UIPanelOpts|opts & title = Just title}
setTitle title (UIFieldSet sOpts iOpts opts)	= UIFieldSet sOpts iOpts {UIFieldSetOpts|opts & title = title}
setTitle title ctrl								= ctrl

setFramed :: !Bool !UIControl -> UIControl
setFramed frame (UIPanel sOpts iOpts opts)	= UIPanel sOpts iOpts {UIPanelOpts|opts & frame = frame}
setFramed frame ctrl						= ctrl

setIconCls :: !String !UIControl -> UIControl
setIconCls iconCls (UIActionButton sOpts aOpts opts)	= UIActionButton sOpts aOpts {UIButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIMenuButton sOpts opts)			= UIMenuButton sOpts {UIMenuButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIIcon sOpts opts)					= UIIcon sOpts {UIIconOpts|opts & iconCls = iconCls}
setIconCls iconCls (UIPanel sOpts iOpts opts) 			= UIPanel sOpts iOpts {UIPanelOpts|opts & iconCls = Just iconCls}
setIconCls iconCls ctrl									= ctrl

setBaseCls :: !String !UIControl -> UIControl
setBaseCls baseCls (UIContainer sOpts iOpts)	    = UIContainer sOpts {UIItemsOpts|iOpts & baseCls = Just baseCls}
setBaseCls baseCls (UIPanel sOpts iOpts opts)		= UIPanel sOpts {UIItemsOpts|iOpts & baseCls = Just baseCls} opts
setBaseCls baseCls ctrl								= ctrl

setDirection :: !UIDirection !UIControl -> UIControl
setDirection dir (UIContainer sOpts iOpts)	    = UIContainer sOpts {UIItemsOpts|iOpts & direction = dir}
setDirection dir (UIPanel sOpts iOpts opts)		= UIPanel sOpts {UIItemsOpts|iOpts & direction = dir} opts
setDirection dir ctrl							= ctrl

setHalign :: !UIHAlign !UIControl -> UIControl
setHalign align (UIContainer sOpts iOpts)	    = UIContainer sOpts {iOpts & halign = align}
setHalign align (UIPanel sOpts iOpts opts)		= UIPanel sOpts {iOpts & halign = align} opts
setHalign align ctrl							= ctrl

setValign :: !UIVAlign !UIControl -> UIControl
setValign align (UIContainer sOpts iOpts)	    = UIContainer sOpts {iOpts & valign = align}
setValign align (UIPanel sOpts iOpts opts)		= UIPanel sOpts {iOpts & valign = align} opts
setValign align ctrl							= ctrl

setTBar :: ![UIControl] !UIControl -> UIControl
setTBar tbar (UIPanel sOpts iOpts opts)			= UIPanel sOpts iOpts {UIPanelOpts|opts & tbar = Just tbar}
setTBar tbar ctrl								= ctrl

getMargins :: !UIControl -> (Maybe UISideSizes)
getMargins ctrl
    | hasSizeOpts ctrl     = getSizeOpts (\{UISizeOpts|margins} -> margins) ctrl
    | hasFSizeOpts ctrl    = getFSizeOpts (\{UIFSizeOpts|margins} -> margins) ctrl
    | hasHSizeOpts ctrl    = getHSizeOpts (\{UIHSizeOpts|margins} -> margins) ctrl
                           = Nothing

uiDefAttributes	:: UIDef -> UIAttributes
uiDefAttributes {UIDef|content=UIControlStack {UIControlStack|attributes}}	= attributes
uiDefAttributes {UIDef|content=UIAttributeSet attributes}					= attributes
uiDefAttributes {UIDef|content=UISubUI {UISubUI|attributes}}	            = attributes
uiDefAttributes {UIDef|content=UISubUIStack {UISubUIStack|attributes}}	    = attributes
uiDefAttributes _													        = newMap

uiDefControls :: UIDef -> [UIControl]
uiDefControls {UIDef|content=UIControlStack {UIControlStack|controls}}	= map fst controls
uiDefControls {UIDef|content=UISubUI {UISubUI|content}}	                = content.UIItemsOpts.items
uiDefControls {UIDef|content=UIFinal (UIViewport content _)}			= content.UIItemsOpts.items
uiDefControls _														    = []

uiDefAnnotatedControls :: UIDef -> [(UIControl,UIAttributes)]
uiDefAnnotatedControls {UIDef|content=UIControlStack {UIControlStack|controls}} = controls
uiDefAnnotatedControls {UIDef|content=UISubUI {UISubUI|content}}	            = [(c,newMap)\\c <- content.UIItemsOpts.items]
uiDefAnnotatedControls {UIDef|content=UIFinal (UIViewport content _)}			= [(c,newMap)\\c <- content.UIItemsOpts.items]
uiDefAnnotatedControls _										                = []

uiDefActions :: UIDef -> [UIAction]
uiDefActions {UIDef|content=UIActionSet actions}	    = actions
uiDefActions {UIDef|content=UISubUI {UISubUI|actions}}	= actions
uiDefActions _								            = []

uiDefDirection :: UIDef -> UIDirection
uiDefDirection {UIDef|content=UISubUI {UISubUI|content}}	    = content.UIItemsOpts.direction
uiDefDirection {UIDef|content=UIFinal (UIViewport content _)}	= content.UIItemsOpts.direction
uiDefDirection _											    = Vertical

uiDefWindows :: UIDef -> [UIWindow]
uiDefWindows {UIDef|windows}		                            = windows
uiDefWindows _													= []

uiDefSetAttribute :: String String UIDef -> UIDef
uiDefSetAttribute key value {UIDef|content=UIAttributeSet attributes,windows}
	= {UIDef|content=UIAttributeSet (put key value attributes),windows=windows}
uiDefSetAttribute key value {UIDef|content=UIControlStack stack=:{UIControlStack|attributes},windows}
	= {UIDef|content=UIControlStack {UIControlStack|stack & attributes = put key value attributes},windows=windows}
uiDefSetAttribute key value {UIDef|content=UISubUI sub=:{UISubUI|attributes},windows}
	= {UIDef|content=UISubUI {UISubUI|sub & attributes = put key value attributes},windows=windows}
uiDefSetAttribute key value {UIDef|content=UISubUIStack stack=:{UISubUIStack|attributes},windows}
	= {UIDef|content=UISubUIStack {UISubUIStack|stack & attributes = put key value attributes},windows=windows}
uiDefSetAttribute key value def = def

uiDefSetDirection :: UIDirection UIDef -> UIDef
uiDefSetDirection direction {UIDef|content=(UISubUI sub),windows}
    = {UIDef|content=UISubUI {UISubUI|sub & content = {UIItemsOpts|sub.UISubUI.content & direction = direction}},windows=windows}
uiDefSetDirection direction def = def

uiDefSetHalign :: UIHAlign UIDef -> UIDef
uiDefSetHalign align {UIDef|content=UISubUI sub,windows}
    = {UIDef|content=UISubUI {UISubUI|sub & content = {UIItemsOpts|sub.UISubUI.content & halign = align}},windows=windows}
uiDefSetHalign align def = def

uiDefSetValign :: UIVAlign UIDef -> UIDef
uiDefSetValign align {UIDef|content=UISubUI sub,windows}
    = {UIDef|content=UISubUI {UISubUI|sub & content = {UIItemsOpts|sub.UISubUI.content & valign = align}},windows=windows}
uiDefSetValign align def = def

uiDefSetPadding :: Int Int Int Int UIDef -> UIDef
uiDefSetPadding top right bottom left {UIDef|content=UISubUI sub,windows}
    = {UIDef|content=UISubUI {UISubUI|sub & content = {UIItemsOpts|sub.UISubUI.content & padding = Just {top=top,right=right,bottom=bottom,left=left}}},windows=windows}
uiDefSetPadding _ _ _ _ def = def

uiDefSetBaseCls :: String UIDef -> UIDef
uiDefSetBaseCls baseCls {UIDef|content=UISubUI sub,windows}
    = {UIDef|content=UISubUI {UISubUI|sub & content = {UIItemsOpts|sub.UISubUI.content & baseCls = Just baseCls}},windows=windows}
uiDefSetBaseCls _ def = def

encodeUIDefinition :: !UIDef -> JSONNode
encodeUIDefinition {UIDef|content=UIFinal (UIViewport iopts opts),windows}
    = enc "itwc_viewport" [toJSON iopts, encViewportOpts opts]
encodeUIDefinition def
    = enc "itwc_viewport" [toJSON (defaultItemsOpts (uiDefControls def))]

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
encodeUIControl (UIEditlet sopts opts)					= enc "itwc_edit_editlet" [toJSON sopts, removeEditletValue (toJSON opts)]

removeEditletValue (JSONObject fields) = JSONObject [field \\ field=:(name,_) <- fields | name <> "value"]

encodeUIWindow :: !UIWindow -> JSONNode
encodeUIWindow (UIWindow sopts iopts opts)				= enc "itwc_window" [toJSON sopts, toJSON iopts, toJSON opts]

encodeUITab :: !UITab -> JSONNode
encodeUITab (UITab iopts opts) 							= enc "itwc_tabitem" [toJSON iopts,toJSON opts]

derive JSONEncode UIViewOpts, UIChoiceOpts, UIActionOpts, UIItemsOpts
derive JSONEncode UISliderOpts, UIProgressOpts, UIGridOpts, UITreeOpts, UIButtonOpts, UITreeNode, UILabelOpts
derive JSONEncode UIIconOpts
derive JSONEncode UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UITabOpts
derive JSONEncode UITaskletOpts, UIEditletOpts

JSONEncode{|UISizeOpts|} {UISizeOpts|width,minWidth,maxWidth,height,minHeight,maxHeight,margins}
    = [JSONObject [field \\ field <- [("itwcWidth",toJSON width)
                                     ,("itwcMinWidth",toJSON minWidth)
                                     ,("itwcMaxWidth",toJSON maxWidth)
                                     ,("itwcHeight",toJSON height)
                                     ,("itwcMinHeight",toJSON minHeight)
                                     ,("itwcMaxHeight",toJSON maxHeight)
                                     ,("margins",toJSON margins)
                                     ] | snd field =!= JSONNull]
      ]
JSONEncode{|UIHSizeOpts|} {UIHSizeOpts|width,minWidth,maxWidth,margins}
    = [JSONObject [field \\ field <- [("itwcWidth",toJSON width)
                                     ,("itwcMinWidth",toJSON minWidth)
                                     ,("itwcMaxWidth",toJSON maxWidth)
                                     ,("margins",toJSON margins)
                                     ] | snd field =!= JSONNull]
      ]
JSONEncode{|UIFSizeOpts|} {UIFSizeOpts|margins}
    = [JSONObject [field \\ field <- [("margins",toJSON margins)] | snd field =!= JSONNull]]

JSONEncode{|UISideSizes|} {top,right,bottom,left}
	= [JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)]

JSONEncode{|UISize|} (ExactSize s)		= [JSONInt s]
JSONEncode{|UISize|} WrapSize			= [JSONString "wrap"]
JSONEncode{|UISize|} FlexSize			= [JSONString "flex"]

JSONEncode{|UIBound|} (ExactBound s)	= [JSONInt s]
JSONEncode{|UIBound|} WrapBound		    = [JSONString "wrap"]

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
encViewportOpts {UIViewportOpts|title,hotkeys}
	= JSONObject (
		[("xtype",JSONString "itwc_viewport")]	++
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
