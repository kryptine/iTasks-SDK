implementation module iTasks.UI.Definition

import Text.JSON, StdList, StdBool, StdTuple, GenEq, StdFunc, Text.HTML, Text
from Data.Map import :: Map, :: Size
from Data.Functor import class Functor(..)
import qualified Data.Map as DM
import qualified Data.List as DL
from iTasks.API.Core.Types import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount(..), :: Action(..), ::ActionName, :: ActionOption, :: Hotkey

from iTasks._Framework.Generic import class iTask(..)
from iTasks._Framework.Generic.Interaction import generic gEditor, generic gEditMeta, generic gVerify
from iTasks._Framework.Generic.Interaction import :: EditMeta, :: VerifyOptions, :: DataPath, :: VerifiedValue, :: Verification
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..)
from iTasks._Framework.Generic.Defaults			import generic gDefault
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

import Text.HTML

derive class iTask UIDef, UIWindow, UIBlock, UIAction, UIEditor, UIControl
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UIWindowType
derive class iTask UIWindowOpts, UIItemsOpts, UIContainerOpts, UISizeOpts, UIEditOpts, UIViewOpts, UIActionOpts
derive class iTask UIChoiceOpts, UIGridOpts, UITreeOpts, UIProgressOpts, UISliderOpts, UIEmbeddingOpts, UITabOpts
derive class iTask UIPanelOpts, UITabSetOpts, UIFieldSetOpts, UIEditletOpts, UITaskletOpts, UIIconOpts, UILabelOpts
derive class iTask UIHSizeOpts, UIFSizeOpts, UIButtonOpts, UIMenuButtonOpts, UITreeNode, UIMenuItem

instance Functor UIViewOpts
where fmap f opts=:{UIViewOpts|value} = {UIViewOpts|opts & value = fmap f value}

defaultSizeOpts :: UISizeOpts
defaultSizeOpts = {UISizeOpts|width = Nothing, minWidth = Nothing, maxWidth = Nothing, height = Nothing, minHeight = Nothing, maxHeight = Nothing, margins = Nothing}

defaultHSizeOpts :: UIHSizeOpts
defaultHSizeOpts = {UIHSizeOpts|width = Nothing, minWidth = Nothing, maxWidth = Nothing, margins = Nothing}

defaultFSizeOpts :: UIFSizeOpts
defaultFSizeOpts = {UIFSizeOpts|margins = Nothing}

defaultItemsOpts :: [UIControl] -> UIItemsOpts
defaultItemsOpts items = {UIItemsOpts|items = items, direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing, baseCls=Nothing,bodyCls=Nothing}
defaultContainerOpts :: UIContainerOpts
defaultContainerOpts = {UIContainerOpts|direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing, baseCls=Nothing,bodyCls=Nothing}

defaultContainer :: ![UIControl] -> UIControl
defaultContainer items = UIContainer defaultSizeOpts (defaultItemsOpts items)

defaultFieldSet :: !(Maybe String) ![UIControl]	-> UIControl
defaultFieldSet title items = UIFieldSet defaultSizeOpts (defaultItemsOpts items) {UIFieldSetOpts|title=title}

defaultPanel :: ![UIDef] -> UIDef
defaultPanel items = UIPanel defaultSizeOpts defaultContainerOpts defaultPanelOpts items

defaultPanelOpts :: UIPanelOpts
defaultPanelOpts = {UIPanelOpts|title=Nothing,iconCls=Nothing,frame=False,hotkeys=Nothing}

defaultTabSet :: ![UIDef] -> UIDef
defaultTabSet items = UITabSet defaultSizeOpts defaultTabSetOpts items

defaultTabSetOpts :: UITabSetOpts
defaultTabSetOpts = {UITabSetOpts|activeTab = 0}

defaultTab :: ![UIDef] -> UIDef
defaultTab items = UITab defaultContainerOpts defaultTabOpts items

defaultTabOpts :: UITabOpts
defaultTabOpts = {UITabOpts|title="Untitled",iconCls=Nothing,focusTaskId=Nothing,closeTaskId=Nothing}

defaultWindow :: ![UIControl] -> UIWindow
defaultWindow items = {UIWindow|sizeOpts=defaultSizeOpts,itemsOpts=(defaultItemsOpts items),windowOpts={UIWindowOpts|windowType=FloatingWindow,title=Nothing,iconCls=Nothing,menu=Nothing,hotkeys=Nothing,vpos=Nothing,hpos=Nothing,closeTaskId=Nothing,focusTaskId=Nothing}}

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
//hasSizeOpts (UIPanel sOpts iOpts opts)			= True
hasSizeOpts (UIFieldSet sOpts iOpts opts)		= True
//hasSizeOpts (UITabSet sOpts opts)				= True
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
//getSizeOpts f (UIPanel sOpts iOpts opts)			= f sOpts
getSizeOpts f (UIFieldSet sOpts iOpts opts)			= f sOpts
//getSizeOpts f (UITabSet sOpts opts)					= f sOpts

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
//setSizeOpts f (UIPanel sOpts iOpts opts)			= (UIPanel (f sOpts) iOpts opts)
setSizeOpts f (UIFieldSet sOpts iOpts opts)			= (UIFieldSet (f sOpts) iOpts opts)
//setSizeOpts f (UITabSet sOpts opts)					= (UITabSet (f sOpts) opts)

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

instance setPadding UIControl
where
	setPadding top right bottom left (UIContainer sOpts iOpts)
		= UIContainer sOpts {UIItemsOpts|iOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}}
	setPadding top right bottom left ctrl = ctrl
instance setPadding UIDef
where
	setPadding top right bottom left (UIPanel sOpts cOpts pOpts items)
		= UIPanel sOpts {UIContainerOpts|cOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} pOpts items
	setPadding top right bottom left def = def

setTitle :: !String !UIControl -> UIControl
//setTitle title (UIPanel sOpts iOpts opts)		= UIPanel sOpts iOpts {UIPanelOpts|opts & title = Just (escapeStr title)}
setTitle title (UIFieldSet sOpts iOpts opts)	= UIFieldSet sOpts iOpts {UIFieldSetOpts|opts & title = Just (escapeStr title)}
setTitle title ctrl								= ctrl

setFramed :: !Bool !UIControl -> UIControl
//setFramed frame (UIPanel sOpts iOpts opts)	= UIPanel sOpts iOpts {UIPanelOpts|opts & frame = frame}
setFramed frame ctrl						= ctrl

setIconCls :: !String !UIControl -> UIControl
setIconCls iconCls (UIActionButton sOpts aOpts opts)	= UIActionButton sOpts aOpts {UIButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIMenuButton sOpts opts)			= UIMenuButton sOpts {UIMenuButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIIcon sOpts opts)					= UIIcon sOpts {UIIconOpts|opts & iconCls = iconCls}
//setIconCls iconCls (UIPanel sOpts iOpts opts) 			= UIPanel sOpts iOpts {UIPanelOpts|opts & iconCls = Just iconCls}
setIconCls iconCls ctrl									= ctrl

instance setBaseCls UIControl
where
	setBaseCls baseCls (UIContainer sOpts iOpts)	    = UIContainer sOpts {UIItemsOpts|iOpts & baseCls = Just baseCls}
	setBaseCls baseCls ctrl								= ctrl

instance setBaseCls UIDef
where
	setBaseCls baseCls (UIPanel sOpts cOpts pOpts items)	= UIPanel sOpts {UIContainerOpts|cOpts & baseCls = Just baseCls} pOpts items
	setBaseCls baseCls def = def

instance setDirection UIControl
where
	setDirection dir (UIContainer sOpts iOpts)	    = UIContainer sOpts {UIItemsOpts|iOpts & direction = dir}
	setDirection dir ctrl							= ctrl
instance setDirection UIDef
where
	setDirection dir (UIPanel sOpts cOpts pOpts items) = UIPanel sOpts {UIContainerOpts|cOpts & direction = dir} pOpts items
	setDirection dir def = def

instance setHalign UIControl
where
	setHalign align (UIContainer sOpts iOpts)	    = UIContainer sOpts {UIItemsOpts|iOpts & halign = align}
	setHalign align ctrl							= ctrl

instance setHalign UIDef
where
	setHalign align (UIPanel sOpts cOpts pOpts items) = UIPanel sOpts {UIContainerOpts|cOpts & halign = align} pOpts items
	setHalign align def = def

setValign :: !UIVAlign !UIControl -> UIControl
setValign align (UIContainer sOpts iOpts)	    = UIContainer sOpts {UIItemsOpts|iOpts & valign = align}
//setValign align (UIPanel sOpts iOpts opts)		= UIPanel sOpts {iOpts & valign = align} opts
setValign align ctrl							= ctrl

getMargins :: !UIControl -> (Maybe UISideSizes)
getMargins ctrl
    | hasSizeOpts ctrl     = getSizeOpts (\{UISizeOpts|margins} -> margins) ctrl
    | hasFSizeOpts ctrl    = getFSizeOpts (\{UIFSizeOpts|margins} -> margins) ctrl
    | hasHSizeOpts ctrl    = getHSizeOpts (\{UIHSizeOpts|margins} -> margins) ctrl
                           = Nothing

uiDefAttributes	:: UIDef -> UIAttributes
//uiDefAttributes (UIBlock {UIBlock|attributes})	= attributes
uiDefAttributes _								= 'DM'.newMap

uiDefControls :: UIDef -> [UIControl]
//uiDefControls (UIBlock {UIBlock|content})	                = content.UIItemsOpts.items
uiDefControls (UIControl control)							= [control]
uiDefControls _												= []

uiDefAnnotatedControls :: UIDef -> [(UIControl,UIAttributes)]
//uiDefAnnotatedControls (UIBlock {UIBlock|content})	            = [(c,'DM'.newMap)\\c <- content.UIItemsOpts.items]
uiDefAnnotatedControls (UIControl control)						= [(control,'DM'.newMap)]
uiDefAnnotatedControls _										= []

uiDefDirection :: UIDef -> UIDirection
//uiDefDirection (UIBlock {UIBlock|content})	    = content.UIItemsOpts.direction
uiDefDirection _								= Vertical

uiDefWindows :: UIDef -> [UIWindow]
uiDefWindows _													= []

uiDefSetAttribute :: String String UIDef -> UIDef
//uiDefSetAttribute key value (UILayers [UIBlock sub=:{UIBlock|attributes}:aux])
//	= UILayers [UIBlock {UIBlock|sub & attributes = 'DM'.put key value attributes}:aux]
//uiDefSetAttribute key value (UIBlock sub=:{UIBlock|attributes})
	//= UIBlock {UIBlock|sub & attributes = 'DM'.put key value attributes}
uiDefSetAttribute key value def = def

uiDefSetDirection :: UIDirection UIDef -> UIDef
//uiDefSetDirection direction (UIBlock sub)
    //= UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & direction = direction}}
uiDefSetDirection direction def = def

uiDefSetHalign :: UIHAlign UIDef -> UIDef
//uiDefSetHalign align (UIBlock sub)
 //   = UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & halign = align}}
uiDefSetHalign align def = def

uiDefSetValign :: UIVAlign UIDef -> UIDef
//uiDefSetValign align (UIBlock sub)
 //   = UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & valign = align}}
uiDefSetValign align def = def

uiDefSetPadding :: Int Int Int Int UIDef -> UIDef
//uiDefSetPadding top right bottom left (UIBlock sub)
 //   = UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & padding = Just {top=top,right=right,bottom=bottom,left=left}}}
uiDefSetPadding _ _ _ _ def = def

uiDefSetMargins :: Int Int Int Int UIDef -> UIDef
//uiDefSetMargins top right bottom left (UIBlock ui=:{UIBlock|size})
//    = UIBlock {UIBlock|ui & size = {UISizeOpts|size & margins = Just {top = top, right = right, bottom = bottom, left = left}}}
uiDefSetMargins _ _ _ _ def = def

uiDefSetBaseCls :: String UIDef -> UIDef
//uiDefSetBaseCls baseCls (UIBlock sub) = UIBlock {UIBlock|sub & content = {UIItemsOpts|sub.UIBlock.content & baseCls = Just baseCls}}
uiDefSetBaseCls _ def = def

uiDefSetHeight :: UISize UIDef -> UIDef
//uiDefSetHeight height (UIBlock ui=:{UIBlock|size}) = UIBlock {UIBlock|ui & size = {UISizeOpts|size & height = Just height}}
uiDefSetHeight height def = def

uiDefSetWidth :: UISize UIDef -> UIDef
//uiDefSetWidth width (UIBlock ui=:{UIBlock|size}) = UIBlock {UIBlock|ui & size = {UISizeOpts|size & width = Just width}}
uiDefSetWidth width def = def

uiDefSetSize :: UISize UISize UIDef -> UIDef
//uiDefSetSize width height (UIBlock ui=:{UIBlock|size}) = UIBlock {UIBlock|ui & size = {UISizeOpts|size & width = Just width, height = Just height}}
uiDefSetSize width height def = def

//Encoding of UI definitions to the JSON format expected by the client
class encodeUI a :: a -> JSONNode

instance encodeUI Int				where encodeUI v = JSONInt v
instance encodeUI Real				where encodeUI v = JSONReal v
instance encodeUI Char 				where encodeUI v = JSONString (toString v)
instance encodeUI String			where encodeUI v = JSONString v
instance encodeUI Bool				where encodeUI v = JSONBool v
instance encodeUI Document			where encodeUI v = toJSON v
instance encodeUI Date				where encodeUI v = toJSON v
instance encodeUI Time				where encodeUI v = toJSON v
instance encodeUI HtmlTag			where encodeUI v = JSONString (toString v)
instance encodeUI ProgressAmount
where
	encodeUI ProgressUndetermined = JSONString "undetermined"
	encodeUI (ProgressRatio ratio)	= JSONReal ratio

instance encodeUI JSONNode
where
	encodeUI v = toJSON v

instance encodeUI (Maybe a) | encodeUI a
where
	encodeUI Nothing = JSONNull
	encodeUI (Just a) = encodeUI a

instance encodeUI [a] | encodeUI a
where
	encodeUI l = JSONArray (map encodeUI l)

instance encodeUI UIDef
where
	encodeUI UIEmpty 						= component "itwc_raw_empty" []
	encodeUI (UIEditor _ control) 			= encodeUI control
	encodeUI (UICompoundEditor opts defs)	= component "itwc_raw_compoundeditor"  [encodeUI opts, JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UICompoundContent defs) 		= component "itwc_raw_compoundcontent" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UIParallel defs) 				= component "itwc_raw_parallel" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UIInteract defs) 				= component "itwc_raw_interact" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UIStep defs) 					= component "itwc_raw_step" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UIAction action) 				= component "itwc_raw_action" [encodeUI action]
	encodeUI (UIPanel sopts iopts opts defs)= component "itwc_panel" [encodeUI sopts, encodeUI iopts, encodeUI opts
																		,JSONObject [("items",JSONArray (map encodeUI defs))]]

	encodeUI (UITabSet sopts opts defs)	    = component "itwc_tabset" [encodeUI sopts, encodeUI opts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UITab copts opts defs) 	 	= component "itwc_tabitem" [encodeUI copts, encodeUI opts,JSONObject [("items",JSONArray (map encodeUI defs))]]

	encodeUI (UIWindow window) 				= component "itwc_raw_window" [encodeUI window]
	encodeUI (UILayers defs) 				= component "itwc_raw_layers" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UIForm defs) 					= component "itwc_raw_form" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UIFormItem label def info) 	= component "itwc_raw_form_item" [JSONObject [("items",JSONArray[encodeUI label,encodeUI def,encodeUI info])]]
	encodeUI (UIControl control) 			= encodeUI control
	encodeUI (UIBlock sopts copts defs)     = component "itwc_raw_block" [encodeUI sopts, encodeUI copts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UIBlocks blocks actions) 		= component "itwc_raw_blocks" [JSONObject [("items",JSONArray (map encodeUI blocks ++ map encodeUI actions))]]

instance encodeUI UIControl
where
	encodeUI (UIViewString sopts vopts)			= component "itwc_view_string" [encodeUI sopts,encodeUI vopts]
	encodeUI (UIViewHtml sopts vopts)			= component "itwc_view_html" [encodeUI sopts, encodeUI vopts]
	encodeUI (UIViewDocument sopts vopts)		= component "itwc_view_document" [encodeUI sopts, encodeUI vopts]
	encodeUI (UIViewCheckbox sopts vopts)		= component "itwc_view_checkbox" [encodeUI sopts, encodeUI vopts]
	encodeUI (UIViewSlider sopts vopts opts)	= component "itwc_view_slider" [encodeUI sopts, encodeUI vopts, encodeUI opts]
	encodeUI (UIViewProgress sopts vopts opts)	= component "itwc_view_progress" [encodeUI sopts, encodeUI vopts, encodeUI opts]
	encodeUI (UIIcon sopts opts)				= component "itwc_view_icon" [encodeUI sopts, encodeUI opts]
	encodeUI (UIEditString sopts eopts)			= component "itwc_edit_string" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditNote sopts eopts)			= component "itwc_edit_note" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditPassword sopts eopts)		= component "itwc_edit_password" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditInt sopts eopts)			= component "itwc_edit_int" [encodeUI sopts, encodeUI  eopts]
	encodeUI (UIEditDecimal sopts eopts)		= component "itwc_edit_decimal" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditCheckbox sopts eopts)		= component "itwc_edit_checkbox" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditSlider sopts eopts opts)	= component "itwc_edit_slider" [encodeUI sopts, encodeUI eopts, encodeUI opts]
	encodeUI (UIEditDate sopts eopts)			= component "itwc_edit_date" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditTime sopts eopts)			= component "itwc_edit_time" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditDateTime sopts eopts)		= component "itwc_edit_datetime" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditDocument sopts eopts)		= component "itwc_edit_document" [encodeUI sopts, encodeUI eopts]
	encodeUI (UIEditButton sopts eopts opts)	= component "itwc_editbutton" [encodeUI sopts, encodeUI eopts, encodeUI opts]
	encodeUI (UIDropdown sopts copts)			= component "itwc_choice_dropdown" [encodeUI sopts, encodeUI copts]
	encodeUI (UIListChoice sopts copts)			= component "itwc_choice_list" [encodeUI sopts, encodeUI copts]
	encodeUI (UIRadioGroup sopts copts)			= component "itwc_choice_radiogroup" [encodeUI sopts, encodeUI copts]
	encodeUI (UICheckboxGroup sopts copts)		= component "itwc_choice_checkboxgroup" [encodeUI sopts, encodeUI copts]
	encodeUI (UIGrid sopts copts opts)			= component "itwc_choice_grid" [encodeUI sopts, encodeUI copts, encodeUI opts]
	encodeUI (UITree sopts copts opts)			= component "itwc_choice_tree" [encodeUI sopts, encodeUI copts, encodeUI opts]
	encodeUI (UIActionButton sopts aopts opts)	= component "itwc_actionbutton" [encodeUI sopts, encodeUI aopts, encodeUI opts]
	encodeUI (UIMenuButton sopts opts)			= component "itwc_menubutton" [encodeUI sopts, encodeUI opts]
	encodeUI (UILabel sopts opts)				= component "itwc_label" [encodeUI sopts, encodeUI opts]
	encodeUI (UISplitter)						= component "itwc_splitter" []
	encodeUI (UIContainer sopts iopts)			= component "itwc_container" [encodeUI sopts, encodeUI iopts]
	encodeUI (UIFieldSet sopts iopts opts)		= component "itwc_fieldset" [encodeUI sopts, encodeUI iopts, encodeUI opts]
	encodeUI (UITasklet sopts opts)				= component "itwc_tasklet" [encodeUI sopts, encodeUI opts]
	encodeUI (UIEditlet sopts opts)				= component "itwc_edit_editlet" [encodeUI sopts, encodeUI opts]
	encodeUI (UIEmbedding sopts opts)			= component "itwc_embedding" [encodeUI sopts, encodeUI opts]

instance encodeUI UIWindow
where
	encodeUI {UIWindow|sizeOpts,itemsOpts,windowOpts}
		= component "itwc_window" [encodeUI sizeOpts, encodeUI itemsOpts, encodeUI windowOpts]


instance encodeUI UIItemsOpts 
where
	encodeUI {UIItemsOpts|items,direction,halign,valign,padding,baseCls,bodyCls}
		= JSONObject [field \\ field <- [("items",JSONArray (map encodeUI items))
                                     	,("direction",encodeUI direction)
                                     	,("halign",encodeUI halign)
                                     	,("valign",encodeUI valign)
                                     	,("padding",encodeUI padding)
                                     	,("baseCls",encodeUI baseCls)
                                     	,("bodyCls",encodeUI bodyCls)
                                     	] | snd field =!= JSONNull]
instance encodeUI UIContainerOpts 
where
	encodeUI {UIContainerOpts|direction,halign,valign,padding,baseCls,bodyCls}
		= JSONObject [field \\ field <- [("direction",encodeUI direction)
                                     	,("halign",encodeUI halign)
                                     	,("valign",encodeUI valign)
                                     	,("padding",encodeUI padding)
                                     	,("baseCls",encodeUI baseCls)
                                     	,("bodyCls",encodeUI bodyCls)
                                     	] | snd field =!= JSONNull]
instance encodeUI UIEditor
where
	encodeUI {UIEditor|optional,attributes}
		= JSONObject [("optional",encodeUI optional)
					 ,("attributes",JSONObject [(k,JSONString v) \\ (k,v) <- 'DM'.toList attributes])
					 ]

instance encodeUI UIAction
where
	encodeUI {UIAction|taskId,action,enabled}
		= JSONObject [("taskId",encodeUI taskId)
					 ,("action",encodeUI action)
					 ,("enabled",encodeUI enabled)
					 ]

instance encodeUI Action
where
	encodeUI (Action name _) = JSONString name

instance encodeUI UIBlock
where
	encodeUI {UIBlock|attributes,content,size}
		= component "itwc_raw_block" [encAttr,encodeUI content,encodeUI size]
	where
		encAttr = JSONObject [(k,JSONString v) \\ (k,v) <- 'DM'.toList attributes] 

instance encodeUI (UIViewOpts a) | encodeUI a
where
	encodeUI {UIViewOpts|value} = JSONObject [("value",encodeUI value)]

instance encodeUI UIEditOpts
where
	encodeUI {UIEditOpts|taskId,editorId,value}
		= JSONObject ([("taskId",JSONString taskId),("editorId",JSONString editorId)] ++ maybe [] (\v -> [("value",v)]) value)

instance encodeUI UITabSetOpts
where
	encodeUI {UITabSetOpts|activeTab}
		= JSONObject [("activeTab",JSONInt activeTab)]

instance encodeUI UISizeOpts
where
	encodeUI {UISizeOpts|width,minWidth,maxWidth,height,minHeight,maxHeight,margins}
    	= JSONObject [field \\ field <- [("itwcWidth",encodeUI width)
                                     	,("itwcMinWidth",encodeUI minWidth)
                                     	,("itwcMaxWidth",encodeUI  maxWidth)
                                     	,("itwcHeight",encodeUI  height)
                                     	,("itwcMinHeight",encodeUI minHeight)
                                     	,("itwcMaxHeight",encodeUI maxHeight)
                                     	,("margins",encodeUI margins)
                                     	] | snd field =!= JSONNull]

instance encodeUI UIHSizeOpts
where
	encodeUI {UIHSizeOpts|width,minWidth,maxWidth,margins}
    	= JSONObject [field \\ field <- [("itwcWidth",encodeUI width)
                                     	,("itwcMinWidth",encodeUI minWidth)
                                     	,("itwcMaxWidth",encodeUI maxWidth)
                                     	,("margins",encodeUI margins)
                                     	] | snd field =!= JSONNull]
instance encodeUI UIFSizeOpts
where
	encodeUI {UIFSizeOpts|margins}
    	= JSONObject [field \\ field <- [("margins",encodeUI margins)] | snd field =!= JSONNull]

instance encodeUI UISideSizes 
where
	encodeUI {top,right,bottom,left}
		= JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)

instance encodeUI UISize
where
	encodeUI (ExactSize s)	= JSONInt s
	encodeUI WrapSize		= JSONString "wrap"
	encodeUI FlexSize		= JSONString "flex"

instance encodeUI UIBound
where
	encodeUI (ExactBound s)	= JSONInt s
	encodeUI WrapBound		= JSONString "wrap"

instance encodeUI UIVAlign
where
	encodeUI AlignTop		= JSONString "top"
	encodeUI AlignMiddle	= JSONString "middle"
	encodeUI AlignBottom	= JSONString "bottom"

instance encodeUI UIHAlign
where
	encodeUI AlignLeft		= JSONString "left"
	encodeUI AlignCenter	= JSONString "center"
	encodeUI AlignRight		= JSONString "right"

instance encodeUI UIDirection
where
	encodeUI Vertical		= JSONString "vertical"
	encodeUI Horizontal		= JSONString "horizontal"

instance encodeUI UIWindowType
where
	encodeUI FloatingWindow 	= JSONString "floating"
	encodeUI ModalDialog 		= JSONString "modal"
	encodeUI NotificationBubble = JSONString "bubble"

instance encodeUI UIMenuButtonOpts 
where
	encodeUI {UIMenuButtonOpts|text,iconCls,disabled,menu}
		= JSONObject (text` ++ [("disabled",JSONBool disabled),("menu",menu`)] ++ iconCls`)
	where
		text`		= maybe [] (\s -> [("text",JSONString s)]) text
		iconCls`	= maybe [] (\s -> [("iconCls",JSONString s)]) iconCls
		menu`       = JSONArray (map encodeUI menu)

instance encodeUI UIMenuItem
where
	encodeUI (UIActionMenuItem aopts opts)	= component "itwc_actionmenuitem" [encodeUI aopts,encodeUI opts]
	encodeUI (UISubMenuItem opts) 			= component "itwc_submenuitem" [encodeUI opts]

instance encodeUI UIButtonOpts where encodeUI opts = toJSON opts
instance encodeUI UIActionOpts where encodeUI opts = toJSON opts
instance encodeUI UIEmbeddingOpts where encodeUI opts = toJSON opts
instance encodeUI UITaskletOpts where encodeUI opts = toJSON opts
instance encodeUI UIFieldSetOpts where encodeUI opts = toJSON opts
instance encodeUI UIPanelOpts where encodeUI opts = toJSON opts
instance encodeUI UILabelOpts where encodeUI opts = toJSON opts
instance encodeUI UITreeOpts where encodeUI opts = toJSON opts
instance encodeUI UIGridOpts where encodeUI opts = toJSON opts
instance encodeUI UISliderOpts where encodeUI opts = toJSON opts
instance encodeUI UIIconOpts where encodeUI opts = toJSON opts
instance encodeUI UIProgressOpts where encodeUI opts = toJSON opts

instance encodeUI (UIChoiceOpts a) | JSONEncode{|*|} a 
where
	 encodeUI opts = toJSON opts

instance encodeUI UIEditletOpts
where
	encodeUI opts = let (JSONObject fields) = toJSON opts in JSONObject [field \\ field <- fields | fst field <> "value"]

instance encodeUI UIWindowOpts
where
	encodeUI {UIWindowOpts|windowType,title,iconCls,menu,hotkeys,vpos,hpos,focusTaskId,closeTaskId}
    	= JSONObject [field \\ field <- [("windowType",encodeUI windowType)
                                     	,("title",encodeUI title)
                                     	,("iconCls",encodeUI iconCls)
                                     	,("menu",encodeUI menu)
                                     	,("hotkeys",toJSON hotkeys)
                                     	,("vpos",encodeUI vpos)
                                     	,("hpos",encodeUI hpos)
                                     	,("focusTaskId",encodeUI focusTaskId)
                                     	,("closeTaskId",encodeUI closeTaskId)
                                     	] | snd field =!= JSONNull]
instance encodeUI UITabOpts
where
	encodeUI {UITabOpts|title,iconCls,focusTaskId,closeTaskId}
    	= JSONObject [field \\ field <- [("title",encodeUI title)
                                     	,("iconCls",encodeUI iconCls)
                                     	,("focusTaskId",encodeUI focusTaskId)
                                     	,("closeTaskId",encodeUI closeTaskId)
                                     	] | snd field =!= JSONNull]

component :: String [JSONNode] -> JSONNode
component xtype opts = JSONObject [("xtype",JSONString xtype):optsfields]
where
	optsfields = flatten [fields \\ JSONObject fields <- opts]
