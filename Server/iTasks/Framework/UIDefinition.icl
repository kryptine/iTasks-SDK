implementation module iTasks.Framework.UIDefinition

import Text.JSON, StdList, StdBool, StdTuple, GenEq, StdFunc, Text.HTML, Text
from Data.Map import :: Map, :: Size
import qualified Data.Map as DM
import qualified Data.List as DL
from iTasks.API.Core.Types import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount(..), :: Action, :: Hotkey
import Text.HTML
	
emptyUI :: UIDef
emptyUI = {UIDef|content=UIFinal (UIViewport (defaultItemsOpts []) {UIViewportOpts|title=Nothing,menu=Nothing,hotkeys=Nothing}),windows = []}

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

defaultFieldSet :: !(Maybe String) ![UIControl]	-> UIControl
defaultFieldSet title items = UIFieldSet defaultSizeOpts (defaultItemsOpts items) {UIFieldSetOpts|title=title}

defaultPanel :: ![UIControl] -> UIControl
defaultPanel items = UIPanel defaultSizeOpts (defaultItemsOpts items) {UIPanelOpts|title=Nothing,iconCls=Nothing,frame=False,hotkeys=Nothing}

defaultWindow :: ![UIControl] -> UIWindow
defaultWindow items = UIWindow defaultSizeOpts (defaultItemsOpts items) {UIWindowOpts|windowType=FloatingWindow,title=Nothing,iconCls=Nothing,menu=Nothing,hotkeys=Nothing,vpos=Nothing,hpos=Nothing,closeTaskId=Nothing,focusTaskId=Nothing}

stringDisplay :: !String -> UIControl
stringDisplay value = UIViewString defaultSizeOpts {UIViewOpts|value = Just (escapeStr value)}

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
hasSizeOpts (UIListChoice sOpts cOpts)			= True
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
getSizeOpts f (UIListChoice sOpts cOpts)			= f sOpts
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
setSizeOpts f (UIListChoice sOpts cOpts)			= (UIListChoice (f sOpts) cOpts)
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
hasHSizeOpts (UIEditDateTime sOpts eOpts)		= True
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
getHSizeOpts f (UIEditDateTime sOpts eOpts)		= f sOpts
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
setHSizeOpts f (UIEditDateTime sOpts eOpts)		= (UIEditDateTime (f sOpts) eOpts)
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
setTitle title (UIPanel sOpts iOpts opts)		= UIPanel sOpts iOpts {UIPanelOpts|opts & title = Just (escapeStr title)}
setTitle title (UIFieldSet sOpts iOpts opts)	= UIFieldSet sOpts iOpts {UIFieldSetOpts|opts & title = Just (escapeStr title)}
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

getMargins :: !UIControl -> (Maybe UISideSizes)
getMargins ctrl
    | hasSizeOpts ctrl     = getSizeOpts (\{UISizeOpts|margins} -> margins) ctrl
    | hasFSizeOpts ctrl    = getFSizeOpts (\{UIFSizeOpts|margins} -> margins) ctrl
    | hasHSizeOpts ctrl    = getHSizeOpts (\{UIHSizeOpts|margins} -> margins) ctrl
                           = Nothing

uiDefAttributes	:: UIDef -> UIAttributes
uiDefAttributes {UIDef|content=UIForm {UIForm|attributes}}	                = attributes
uiDefAttributes {UIDef|content=UIBlock {UIBlock|attributes}}	            = attributes
uiDefAttributes _													        = 'DM'.newMap

uiDefControls :: UIDef -> [UIControl]
uiDefControls {UIDef|content=UIForm {UIForm|controls}}	                = map fst controls
uiDefControls {UIDef|content=UIBlock {UIBlock|content}}	                = content.UIItemsOpts.items
uiDefControls {UIDef|content=UIFinal (UIViewport content _)}			= content.UIItemsOpts.items
uiDefControls _														    = []

uiDefAnnotatedControls :: UIDef -> [(UIControl,UIAttributes)]
uiDefAnnotatedControls {UIDef|content=UIForm {UIForm|controls}} = controls
uiDefAnnotatedControls {UIDef|content=UIBlock {UIBlock|content}}	            = [(c,'DM'.newMap)\\c <- content.UIItemsOpts.items]
uiDefAnnotatedControls {UIDef|content=UIFinal (UIViewport content _)}			= [(c,'DM'.newMap)\\c <- content.UIItemsOpts.items]
uiDefAnnotatedControls _										                = []

uiDefActions :: UIDef -> [UIAction]
uiDefActions {UIDef|content=UIEmpty {UIEmpty|actions}}	= actions
uiDefActions {UIDef|content=UIBlock {UIBlock|actions}}	= actions
uiDefActions _								            = []

uiDefDirection :: UIDef -> UIDirection
uiDefDirection {UIDef|content=UIBlock {UIBlock|content}}	    = content.UIItemsOpts.direction
uiDefDirection {UIDef|content=UIFinal (UIViewport content _)}	= content.UIItemsOpts.direction
uiDefDirection _											    = Vertical

uiDefWindows :: UIDef -> [UIWindow]
uiDefWindows {UIDef|windows}		                            = windows
uiDefWindows _													= []

uiDefSetAttribute :: String String UIDef -> UIDef
uiDefSetAttribute key value {UIDef|content=UIForm stack=:{UIForm|attributes},windows}
	= {UIDef|content=UIForm {UIForm|stack & attributes = 'DM'.put key value attributes},windows=windows}
uiDefSetAttribute key value {UIDef|content=UIBlock sub=:{UIBlock|attributes},windows}
	= {UIDef|content=UIBlock {UIBlock|sub & attributes = 'DM'.put key value attributes},windows=windows}
uiDefSetAttribute key value def = def

uiDefSetDirection :: UIDirection UIDef -> UIDef
uiDefSetDirection direction {UIDef|content=(UIBlock sub),windows}
    = {UIDef|content=UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & direction = direction}},windows=windows}
uiDefSetDirection direction def = def

uiDefSetHalign :: UIHAlign UIDef -> UIDef
uiDefSetHalign align {UIDef|content=UIBlock sub,windows}
    = {UIDef|content=UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & halign = align}},windows=windows}
uiDefSetHalign align def = def

uiDefSetValign :: UIVAlign UIDef -> UIDef
uiDefSetValign align {UIDef|content=UIBlock sub,windows}
    = {UIDef|content=UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & valign = align}},windows=windows}
uiDefSetValign align def = def

uiDefSetPadding :: Int Int Int Int UIDef -> UIDef
uiDefSetPadding top right bottom left {UIDef|content=UIBlock sub,windows}
    = {UIDef|content=UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & padding = Just {top=top,right=right,bottom=bottom,left=left}}},windows=windows}
uiDefSetPadding _ _ _ _ def = def

uiDefSetMargins :: Int Int Int Int UIDef -> UIDef
uiDefSetMargins top right bottom left {UIDef|content=UIForm stack=:{UIForm|size},windows}
    = {UIDef|content=UIForm {UIForm|stack & size = {UISizeOpts|size & margins = Just {top = top, right = right, bottom = bottom, left = left}}},windows=windows}
uiDefSetMargins top right bottom left {UIDef|content=UIBlock ui=:{UIBlock|size},windows}
    = {UIDef|content=UIBlock {UIBlock|ui & size = {UISizeOpts|size & margins = Just {top = top, right = right, bottom = bottom, left = left}}},windows=windows}
uiDefSetMargins _ _ _ _ def = def

uiDefSetBaseCls :: String UIDef -> UIDef
uiDefSetBaseCls baseCls {UIDef|content=UIBlock sub,windows}
    = {UIDef|content=UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & baseCls = Just baseCls}},windows=windows}
uiDefSetBaseCls _ def = def

uiDefSetHeight :: UISize UIDef -> UIDef
uiDefSetHeight height {UIDef|content=UIForm stack=:{UIForm|size},windows}
    = {UIDef|content=UIForm {UIForm|stack & size = {UISizeOpts|size & height = Just height}},windows=windows}
uiDefSetHeight height {UIDef|content=UIBlock ui=:{UIBlock|size},windows}
    = {UIDef|content=UIBlock {UIBlock|ui & size = {UISizeOpts|size & height = Just height}},windows=windows}
uiDefSetHeight height def = def

uiDefSetWidth :: UISize UIDef -> UIDef
uiDefSetWidth width {UIDef|content=UIForm stack=:{UIForm|size},windows}
    = {UIDef|content=UIForm {UIForm|stack & size = {UISizeOpts|size & width = Just width}},windows=windows}
uiDefSetWidth width {UIDef|content=UIBlock ui=:{UIBlock|size},windows}
    = {UIDef|content=UIBlock {UIBlock|ui & size = {UISizeOpts|size & width = Just width}},windows=windows}
uiDefSetWidth width def = def

uiDefSetSize :: UISize UISize UIDef -> UIDef
uiDefSetSize width height {UIDef|content=UIForm stack=:{UIForm|size},windows}
    = {UIDef|content=UIForm {UIForm|stack & size = {UISizeOpts|size & width = Just width, height = Just height}},windows=windows}
uiDefSetSize width height {UIDef|content=UIBlock ui=:{UIBlock|size},windows}
    = {UIDef|content=UIBlock {UIBlock|ui & size = {UISizeOpts|size & width = Just width, height = Just height}},windows=windows}
uiDefSetSize width height def = def

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
encodeUIControl (UIEditDateTime sopts eopts)			= enc "itwc_edit_datetime" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditDocument sopts eopts)			= enc "itwc_edit_document" [toJSON sopts, encEditOpts eopts]
encodeUIControl (UIEditButton sopts eopts opts)			= enc "itwc_editbutton" [toJSON sopts, encEditOpts eopts, toJSON opts]
encodeUIControl (UIDropdown sopts copts)				= enc "itwc_choice_dropdown" [toJSON sopts, toJSON copts]
encodeUIControl (UIListChoice sopts copts)				= enc "itwc_choice_list" [toJSON sopts, toJSON copts]
encodeUIControl (UIRadioGroup sopts copts)				= enc "itwc_choice_radiogroup" [toJSON sopts, toJSON copts]
encodeUIControl (UICheckboxGroup sopts copts)			= enc "itwc_choice_checkboxgroup" [toJSON sopts, toJSON copts]
encodeUIControl (UIGrid sopts copts opts)				= enc "itwc_choice_grid" [toJSON sopts, toJSON copts, toJSON opts]
encodeUIControl (UITree sopts copts opts)				= enc "itwc_choice_tree" [toJSON sopts, toJSON copts, toJSON opts]
encodeUIControl (UIActionButton sopts aopts opts)		= enc "itwc_actionbutton" [toJSON sopts, toJSON aopts, toJSON opts]
encodeUIControl (UIMenuButton sopts opts)				= enc "itwc_menubutton" [toJSON sopts, toJSON opts]
encodeUIControl (UILabel sopts opts)					= enc "itwc_label" [toJSON sopts, toJSON opts]
encodeUIControl (UIIcon sopts opts)						= enc "itwc_icon" [toJSON sopts, toJSON opts]
encodeUIControl (UISplitter)						    = enc "itwc_splitter" []
//encodeUIControl (UISVG sopts opts)						= enc "itwc_svg" [toJSON sopts, toJSON opts]
encodeUIControl (UIContainer sopts iopts)			    = enc "itwc_container" [toJSON sopts, toJSON iopts]
encodeUIControl (UIPanel sopts iopts opts)				= enc "itwc_panel" [toJSON sopts, toJSON iopts, toJSON opts]
encodeUIControl (UIFieldSet sopts iopts opts)			= enc "itwc_fieldset" [toJSON sopts, toJSON iopts, toJSON opts]
encodeUIControl (UITabSet sopts opts)					= enc "itwc_tabset" [toJSON sopts, encTabSetOpts opts]
encodeUIControl (UITasklet sopts opts)					= enc "itwc_tasklet" [toJSON sopts, toJSON opts]
encodeUIControl (UIEditlet sopts opts)					= enc "itwc_edit_editlet" [toJSON sopts, removeEditletValue (toJSON opts)]
encodeUIControl (UIEmbedding sopts opts)                = enc "itwc_embedding" [toJSON sopts, toJSON opts]

removeEditletValue (JSONObject fields) = JSONObject [field \\ field=:(name,_) <- fields | name <> "value"]

encodeUIWindow :: !UIWindow -> JSONNode
encodeUIWindow (UIWindow sopts iopts opts)				= enc "itwc_window" [toJSON sopts, toJSON iopts, toJSON opts]

encodeUITab :: !UITab -> JSONNode
encodeUITab (UITab iopts opts) 							= enc "itwc_tabitem" [toJSON iopts,toJSON opts]

derive JSONEncode UIViewOpts, UIChoiceOpts, UIActionOpts, UIItemsOpts
derive JSONEncode UISliderOpts, UIProgressOpts, UIGridOpts, UITreeOpts, UIButtonOpts, UITreeNode, UILabelOpts
derive JSONEncode UIIconOpts
derive JSONEncode UIPanelOpts, UIFieldSetOpts, UIWindowOpts, UITabOpts
derive JSONEncode UITaskletOpts, UIEditletOpts, UIEmbeddingOpts

toJSONField x = case (JSONEncode{|*|} True x) of
	[node]	= node
	_		= JSONError

JSONEncode{|UISizeOpts|} _ {UISizeOpts|width,minWidth,maxWidth,height,minHeight,maxHeight,margins}
    = [JSONObject [field \\ field <- [("itwcWidth",toJSONField width)
                                     ,("itwcMinWidth",toJSONField minWidth)
                                     ,("itwcMaxWidth",toJSONField maxWidth)
                                     ,("itwcHeight",toJSONField height)
                                     ,("itwcMinHeight",toJSONField minHeight)
                                     ,("itwcMaxHeight",toJSONField maxHeight)
                                     ,("margins",toJSONField margins)
                                     ] | snd field =!= JSONNull]
      ]
JSONEncode{|UIHSizeOpts|} _ {UIHSizeOpts|width,minWidth,maxWidth,margins}
    = [JSONObject [field \\ field <- [("itwcWidth",toJSONField width)
                                     ,("itwcMinWidth",toJSONField minWidth)
                                     ,("itwcMaxWidth",toJSONField maxWidth)
                                     ,("margins",toJSONField margins)
                                     ] | snd field =!= JSONNull]
      ]
JSONEncode{|UIFSizeOpts|} _ {UIFSizeOpts|margins}
    = [JSONObject [field \\ field <- [("margins",toJSONField margins)] | snd field =!= JSONNull]]

JSONEncode{|UISideSizes|} _ {top,right,bottom,left}
	= [JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)]

JSONEncode{|UISize|} _ (ExactSize s)	= [JSONInt s]
JSONEncode{|UISize|} _ WrapSize			= [JSONString "wrap"]
JSONEncode{|UISize|} _ FlexSize			= [JSONString "flex"]

JSONEncode{|UIBound|} _ (ExactBound s)	= [JSONInt s]
JSONEncode{|UIBound|} _ WrapBound		= [JSONString "wrap"]

JSONEncode{|UIVAlign|} _ AlignTop		= [JSONString "top"]
JSONEncode{|UIVAlign|} _ AlignMiddle	= [JSONString "middle"]
JSONEncode{|UIVAlign|} _ AlignBottom	= [JSONString "bottom"]

JSONEncode{|UIHAlign|} _ AlignLeft		= [JSONString "left"]
JSONEncode{|UIHAlign|} _ AlignCenter	= [JSONString "center"]
JSONEncode{|UIHAlign|} _ AlignRight		= [JSONString "right"]

JSONEncode{|UIDirection|} _ Vertical	= [JSONString "vertical"]
JSONEncode{|UIDirection|} _ Horizontal	= [JSONString "horizontal"]

JSONEncode{|UIWindowType|} _ FloatingWindow = [JSONString "floating"]
JSONEncode{|UIWindowType|} _ ModalDialog = [JSONString "modal"]
JSONEncode{|UIWindowType|} _ NotificationBubble = [JSONString "bubble"]

JSONEncode{|UIMenuButtonOpts|} _ {UIMenuButtonOpts|text,iconCls,disabled,menu}
	= [JSONObject (text` ++ [("disabled",JSONBool disabled),("menu",menu`)] ++ iconCls`)]
where
	text`		= maybe [] (\s -> [("text",JSONString s)]) text
	iconCls`	= maybe [] (\s -> [("iconCls",JSONString s)]) iconCls
	menu`       = JSONArray (map toJSON menu)

JSONEncode{|UIMenuItem|} _ (UIActionMenuItem aopts opts)	= [enc "itwc_actionmenuitem" [toJSON aopts,toJSON opts]]
JSONEncode{|UIMenuItem|} _ (UISubMenuItem opts) 			= [enc "itwc_submenuitem" [toJSON opts]]

JSONEncode{|UIControl|} _ control = [encodeUIControl control]

JSONEncode{|UIDef|} _ uidef = [encodeUIDefinition uidef]

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
