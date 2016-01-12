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

derive class iTask UI, UINodeType, UIAction, UIEditor, UIControl
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UIWindowType
derive class iTask UIWindowOpts, UIContainerOpts, UISizeOpts, UIEditOpts, UIViewOpts, UIActionOpts
derive class iTask UIChoiceOpts, UIGridOpts, UITreeOpts, UIProgressOpts, UISliderOpts, UIEmbeddingOpts, UITabOpts
derive class iTask UIPanelOpts, UITabSetOpts, UIEditletOpts, UITaskletOpts, UIIconOpts, UILabelOpts
derive class iTask UIHSizeOpts, UIFSizeOpts, UIButtonOpts, UIMenuButtonOpts, UITreeNode, UIMenuItem

instance Functor UIViewOpts
where fmap f opts=:{UIViewOpts|value} = {UIViewOpts|opts & value = fmap f value}

setSize :: !UISize !UISize !UI -> UI
setSize width height def
    | hasSizeOpts def = setSizeOpts (\opts -> {UISizeOpts| opts & width = Just width, height = Just height}) def
    | hasHSizeOpts def = setHSizeOpts (\opts -> {UIHSizeOpts| opts & width = Just width}) def
                        = def

setWidth :: !UISize !UI -> UI
setWidth width def
    | hasSizeOpts def = setSizeOpts (\opts -> {UISizeOpts| opts & width = Just width}) def
    | hasHSizeOpts def = setHSizeOpts (\opts -> {UIHSizeOpts| opts & width = Just width}) def
                           = def

setHeight :: !UISize !UI -> UI
setHeight height def
    | hasSizeOpts def = setSizeOpts (\opts -> {UISizeOpts| opts & height = Just height}) def
                      = def

setMinSize :: !UIBound !UIBound !UI -> UI
setMinSize minWidth minHeight def
    | hasSizeOpts def   = setSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just minWidth, minHeight = Just minHeight}) def
    | hasHSizeOpts def  = setHSizeOpts (\opts -> {UIHSizeOpts| opts & minWidth = Just minWidth}) def
                        = def

setMinWidth :: !UIBound !UI -> UI
setMinWidth minWidth def
    | hasSizeOpts def   = setSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just minWidth}) def
    | hasHSizeOpts def 	= setHSizeOpts (\opts -> {UIHSizeOpts| opts & minWidth = Just minWidth}) def
                        = def

setMinHeight :: !UIBound !UI -> UI
setMinHeight minHeight def
    | hasSizeOpts def = setSizeOpts (\opts -> {UISizeOpts| opts & minHeight = Just minHeight}) def
                       = def

setMaxSize :: !UIBound !UIBound !UI -> UI
setMaxSize maxWidth maxHeight def
    | hasSizeOpts def = setSizeOpts (\opts -> {UISizeOpts| opts & maxWidth = Just maxWidth, maxHeight = Just maxHeight}) def
    | hasHSizeOpts def = setHSizeOpts (\opts -> {UIHSizeOpts| opts & maxWidth = Just maxWidth}) def
                           = def

setMaxWidth :: !UIBound !UI -> UI
setMaxWidth maxWidth def
    | hasSizeOpts def  = setSizeOpts (\opts -> {UISizeOpts| opts & maxWidth = Just maxWidth}) def
    | hasHSizeOpts def = setHSizeOpts (\opts -> {UIHSizeOpts| opts & maxWidth = Just maxWidth}) def
                       = def

setMaxHeight :: !UIBound !UI -> UI
setMaxHeight maxHeight def
    | hasSizeOpts def  = setSizeOpts (\opts -> {UISizeOpts| opts & maxHeight = Just maxHeight}) def
                       = def

fill :: !UI -> UI
fill def = setSize FlexSize FlexSize def

fillHeight :: !UI -> UI
fillHeight def = setHeight FlexSize def

fillWidth :: !UI -> UI
fillWidth def = setWidth FlexSize def

fixedHeight	:: !Int !UI -> UI
fixedHeight size def = setHeight (ExactSize size) def

fixedWidth :: !Int !UI -> UI
fixedWidth size def = setWidth (ExactSize size) def

wrapHeight :: !UI -> UI
wrapHeight def = setHeight WrapSize def

wrapWidth :: !UI -> UI
wrapWidth def = setWidth WrapSize def

setMargins :: !Int !Int !Int !Int !UI -> UI
setMargins top right bottom left def
    | hasSizeOpts def      = setSizeOpts (\opts -> {UISizeOpts| opts & margins = Just margins}) def
    | hasFSizeOpts def     = setFSizeOpts (\opts -> {UIFSizeOpts| opts & margins = Just margins}) def
    | hasHSizeOpts def     = setHSizeOpts (\opts -> {UIHSizeOpts| opts & margins = Just margins}) def
                           = def
where
    margins = {top = top, right = right, bottom = bottom, left = left}

setMargin :: (UISideSizes -> UISideSizes) !UI -> UI
setMargin f def
    | hasSizeOpts def = setSizeOpts (\opts=:{UISizeOpts|margins} -> {UISizeOpts| opts & margins = Just (f (fromMaybe {top = 0, right = 0, bottom = 0, left = 0} margins))}) def
    | hasFSizeOpts def = setFSizeOpts (\opts=:{UIFSizeOpts|margins} -> {UIFSizeOpts| opts & margins = Just (f (fromMaybe {top = 0, right = 0, bottom = 0, left = 0} margins))}) def
    | hasHSizeOpts def = setHSizeOpts (\opts=:{UIHSizeOpts|margins} -> {UIHSizeOpts| opts & margins = Just (f (fromMaybe {top = 0, right = 0, bottom = 0, left = 0} margins))}) def
                           = def

setTopMargin :: !Int !UI -> UI
setTopMargin top def = setMargin (\m -> {m & top = top}) def

setRightMargin	:: !Int !UI -> UI
setRightMargin right def = setMargin (\m -> {m & right = right}) def

setBottomMargin	:: !Int !UI -> UI
setBottomMargin bottom def = setMargin (\m -> {m & bottom = bottom}) def

setLeftMargin :: !Int !UI -> UI
setLeftMargin left def = setMargin (\m -> {m & left = left}) def

setPadding :: !Int !Int !Int !Int !UI -> UI
setPadding top right bottom left (UI (UIContainer sOpts cOpts) items)
	= UI (UIContainer sOpts {UIContainerOpts|cOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}}) items
setPadding top right bottom left (UI (UIPanel sOpts cOpts pOpts) items)
	= UI (UIPanel sOpts {UIContainerOpts|cOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} pOpts) items
setPadding top right bottom left def = def

setTitle :: !String !UI -> UI
setTitle title (UI (UIPanel sOpts cOpts opts) items) = UI (UIPanel sOpts cOpts {UIPanelOpts|opts & title = Just (escapeStr title)}) items
setTitle title (UI (UITab cOpts opts) items) = UI (UITab cOpts {UITabOpts|opts & title = escapeStr title}) items
setTitle title def = def

setFramed :: !Bool !UI -> UI
setFramed frame (UI (UIPanel sOpts cOpts opts) items) = UI (UIPanel sOpts cOpts {UIPanelOpts|opts & frame = frame}) items
setFramed frame def = def

setIconCls :: !String !UI -> UI
setIconCls iconCls (UI (UIControl (UIActionButton sOpts aOpts opts)) items)
	= UI (UIControl (UIActionButton sOpts aOpts {UIButtonOpts|opts & iconCls = Just iconCls})) items
setIconCls iconCls (UI (UIControl (UIMenuButton sOpts opts)) items)
	= UI (UIControl (UIMenuButton sOpts {UIMenuButtonOpts|opts & iconCls = Just iconCls})) items
setIconCls iconCls (UI (UIControl (UIIcon sOpts opts)) items)
	= UI (UIControl (UIIcon sOpts {UIIconOpts|opts & iconCls = iconCls})) items
setIconCls iconCls (UI (UIPanel sOpts cOpts pOpts) items)
	= UI (UIPanel sOpts cOpts {UIPanelOpts|pOpts & iconCls = Just iconCls}) items
setIconCls iconCls def = def

setBaseCls :: !String !UI -> UI
setBaseCls baseCls (UI (UIContainer sOpts cOpts) items)
	= UI (UIContainer sOpts {UIContainerOpts|cOpts & baseCls = Just baseCls}) items
setBaseCls baseCls (UI (UIPanel sOpts cOpts pOpts) items)
	= UI (UIPanel sOpts {UIContainerOpts|cOpts & baseCls = Just baseCls} pOpts) items
setBaseCls baseCls def = def

setDirection :: !UIDirection !UI -> UI
setDirection dir (UI (UIContainer sOpts cOpts) items)   = UI (UIContainer sOpts {UIContainerOpts|cOpts & direction = dir}) items
setDirection dir (UI (UIPanel sOpts cOpts pOpts) items) = UI (UIPanel sOpts {UIContainerOpts|cOpts & direction = dir} pOpts) items
setDirection dir def = def

setHalign :: !UIHAlign !UI -> UI
setHalign align (UI (UIContainer sOpts cOpts) items)   = UI (UIContainer sOpts {UIContainerOpts|cOpts & halign = align}) items
setHalign align (UI (UIPanel sOpts cOpts pOpts) items) = UI (UIPanel sOpts {UIContainerOpts|cOpts & halign = align} pOpts) items
setHalign align def = def

setValign :: !UIVAlign !UI -> UI
setValign align (UI (UIContainer sOpts cOpts) items)   = UI (UIContainer sOpts {UIContainerOpts|cOpts & valign = align}) items
setValign align (UI (UIPanel sOpts cOpts opts) items)  = UI (UIPanel sOpts {cOpts & valign = align} opts) items
setValign align def = def

defaultSizeOpts :: UISizeOpts
defaultSizeOpts = {UISizeOpts|width = Nothing, minWidth = Nothing, maxWidth = Nothing, height = Nothing, minHeight = Nothing, maxHeight = Nothing, margins = Nothing}

defaultHSizeOpts :: UIHSizeOpts
defaultHSizeOpts = {UIHSizeOpts|width = Nothing, minWidth = Nothing, maxWidth = Nothing, margins = Nothing}

defaultFSizeOpts :: UIFSizeOpts
defaultFSizeOpts = {UIFSizeOpts|margins = Nothing}

defaultContainerOpts :: UIContainerOpts
defaultContainerOpts = {UIContainerOpts|direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing, baseCls=Nothing,bodyCls=Nothing}

defaultContainer :: ![UI] -> UI
defaultContainer items = UI (UIContainer defaultSizeOpts defaultContainerOpts) items

defaultPanel :: ![UI] -> UI
defaultPanel items = UI (UIPanel defaultSizeOpts defaultContainerOpts defaultPanelOpts) items

defaultPanelOpts :: UIPanelOpts
defaultPanelOpts = {UIPanelOpts|title=Nothing,iconCls=Nothing,frame=False,hotkeys=Nothing}

defaultTabSet :: ![UI] -> UI
defaultTabSet items = UI (UITabSet defaultSizeOpts defaultTabSetOpts) items

defaultTabSetOpts :: UITabSetOpts
defaultTabSetOpts = {UITabSetOpts|activeTab = 0}

defaultTab :: ![UI] -> UI
defaultTab items = UI (UITab defaultContainerOpts defaultTabOpts) items

defaultTabOpts :: UITabOpts
defaultTabOpts = {UITabOpts|title="Untitled",iconCls=Nothing,focusTaskId=Nothing,closeTaskId=Nothing}

defaultWindow :: ![UI] -> UI
defaultWindow items = UI (UIWindow defaultSizeOpts defaultContainerOpts defaultWindowOpts) items

defaultWindowOpts :: UIWindowOpts
defaultWindowOpts = {UIWindowOpts|windowType=FloatingWindow,title=Nothing,iconCls=Nothing,vpos=Nothing,hpos=Nothing,closeTaskId=Nothing,focusTaskId=Nothing}

stringDisplay :: !String -> UI
stringDisplay value = UI (UIControl (UIViewString defaultSizeOpts {UIViewOpts|value = Just (escapeStr value)})) []

hasFSizeOpts :: !UI -> Bool
hasFSizeOpts (UI (UIControl (UIViewCheckbox sOpts vOpts)) items) = True
hasFSizeOpts (UI (UIControl (UIEditCheckbox sOpts eOpts)) items) = True
hasFSizeOpts (UI (UIControl (UIIcon sOpts opts)) items)          = True
hasFSizeOpts _                                                   = False

getFSizeOpts :: (UIFSizeOpts -> a) UI -> a
getFSizeOpts f (UI (UIControl (UIViewCheckbox sOpts vOpts)) items) = f sOpts
getFSizeOpts f (UI (UIControl (UIEditCheckbox sOpts eOpts)) items) = f sOpts
getFSizeOpts f (UI (UIControl (UIIcon sOpts opts)) items)          = f sOpts

setFSizeOpts :: (UIFSizeOpts -> UIFSizeOpts) UI -> UI
setFSizeOpts f (UI (UIControl (UIViewCheckbox sOpts vOpts)) items) = UI (UIControl (UIViewCheckbox (f sOpts) vOpts)) items
setFSizeOpts f (UI (UIControl (UIEditCheckbox sOpts eOpts)) items) = UI (UIControl (UIEditCheckbox (f sOpts) eOpts)) items
setFSizeOpts f (UI (UIControl (UIIcon sOpts opts)) items)          = UI (UIControl (UIIcon (f sOpts) opts)) items
setFSizeOpts f def = def

hasSizeOpts :: !UI -> Bool
hasSizeOpts (UI (UIControl (UIViewString sOpts vOpts)) items) = True
hasSizeOpts (UI (UIControl (UIViewHtml sOpts vOpts)) items) = True
hasSizeOpts (UI (UIControl (UIEditNote sOpts eOpts)) items) = True
hasSizeOpts (UI (UIControl (UIEditButton sOpts eOpts opts)) items)	= True
hasSizeOpts (UI (UIControl (UIListChoice sOpts cOpts)) items) = True
hasSizeOpts (UI (UIControl (UIRadioGroup sOpts cOpts)) items) = True
hasSizeOpts (UI (UIControl (UICheckboxGroup sOpts cOpts)) items) = True
hasSizeOpts (UI (UIControl (UIGrid sOpts cOpts opts)) items) = True
hasSizeOpts (UI (UIControl (UITree sOpts cOpts opts)) items) = True
hasSizeOpts (UI (UIControl (UIActionButton sOpts aOpts opts)) items) = True
hasSizeOpts (UI (UIControl (UIMenuButton sOpts opts)) items) = True
hasSizeOpts (UI (UIControl (UITasklet sOpts opts)) items) = True
hasSizeOpts (UI (UIControl (UIEditlet sOpts opts)) items) = True
hasSizeOpts (UI (UIContainer sOpts cOpts) items) = True
hasSizeOpts (UI (UIPanel sOpts cOpts pOpts) items) = True
hasSizeOpts (UI (UITabSet sOpts opts) items) = True
hasSizeOpts _ = False

getSizeOpts :: (UISizeOpts -> a) UI -> a
getSizeOpts f (UI (UIControl (UIViewString	sOpts vOpts)) items)      = f sOpts
getSizeOpts f (UI (UIControl (UIViewHtml sOpts vOpts)) items)          = f sOpts
getSizeOpts f (UI (UIControl (UIEditNote sOpts eOpts)) items)          = f sOpts
getSizeOpts f (UI (UIControl (UIEditButton sOpts eOpts opts)) items)   = f sOpts
getSizeOpts f (UI (UIControl (UIListChoice sOpts cOpts)) items)        = f sOpts
getSizeOpts f (UI (UIControl (UIRadioGroup sOpts cOpts)) items)        = f sOpts
getSizeOpts f (UI (UIControl (UICheckboxGroup sOpts cOpts)) items)     = f sOpts
getSizeOpts f (UI (UIControl (UIGrid sOpts cOpts opts)) items)         = f sOpts
getSizeOpts f (UI (UIControl (UITree sOpts cOpts opts)) items)         = f sOpts
getSizeOpts f (UI (UIControl (UIActionButton sOpts aOpts opts)) items) = f sOpts
getSizeOpts f (UI (UIControl (UIMenuButton sOpts opts)) items)	       = f sOpts
getSizeOpts f (UI (UIControl (UITasklet sOpts opts)) items)            = f sOpts
getSizeOpts f (UI (UIControl (UIEditlet sOpts opts)) items)            = f sOpts
getSizeOpts f (UI (UIContainer sOpts iOpts) items)                     = f sOpts
getSizeOpts f (UI (UIPanel sOpts iOpts pOpts) items)	               = f sOpts
getSizeOpts f (UI (UITabSet sOpts opts) items)                         = f sOpts

setSizeOpts :: (UISizeOpts -> UISizeOpts) UI -> UI
setSizeOpts f (UI (UIControl (UIViewString	sOpts vOpts)) items)       = UI (UIControl (UIViewString (f sOpts) vOpts)) items
setSizeOpts f (UI (UIControl (UIViewHtml sOpts vOpts)) items)          = UI (UIControl (UIViewHtml (f sOpts) vOpts)) items
setSizeOpts f (UI (UIControl (UIEditNote sOpts eOpts)) items)          = UI (UIControl (UIEditNote (f sOpts) eOpts)) items
setSizeOpts f (UI (UIControl (UIEditButton sOpts eOpts opts)) items)   = UI (UIControl (UIEditButton (f sOpts) eOpts opts)) items
setSizeOpts f (UI (UIControl (UIListChoice sOpts cOpts)) items)        = UI (UIControl (UIListChoice (f sOpts) cOpts)) items
setSizeOpts f (UI (UIControl (UIRadioGroup sOpts cOpts)) items)        = UI (UIControl (UIRadioGroup (f sOpts) cOpts)) items
setSizeOpts f (UI (UIControl (UICheckboxGroup sOpts cOpts)) items)     = UI (UIControl (UICheckboxGroup (f sOpts) cOpts)) items
setSizeOpts f (UI (UIControl (UIGrid sOpts cOpts opts)) items)         = UI (UIControl (UIGrid (f sOpts) cOpts opts)) items
setSizeOpts f (UI (UIControl (UITree sOpts cOpts opts)) items)         = UI (UIControl (UITree (f sOpts) cOpts opts)) items
setSizeOpts f (UI (UIControl (UIActionButton sOpts aOpts opts)) items) = UI (UIControl (UIActionButton (f sOpts) aOpts opts)) items
setSizeOpts f (UI (UIControl (UIMenuButton sOpts opts)) items)         = UI (UIControl (UIMenuButton (f sOpts) opts)) items
setSizeOpts f (UI (UIControl (UITasklet sOpts opts)) items)            = UI (UIControl (UITasklet (f sOpts) opts)) items
setSizeOpts f (UI (UIControl (UIEditlet sOpts opts)) items)            = UI (UIControl (UIEditlet (f sOpts) opts)) items
setSizeOpts f (UI (UIContainer sOpts cOpts) items)                     = UI (UIContainer (f sOpts) cOpts) items
setSizeOpts f (UI (UIPanel sOpts cOpts pOpts) items)                   = UI (UIPanel (f sOpts) cOpts pOpts) items
setSizeOpts f (UI (UITabSet sOpts opts) items)                         = UI (UITabSet (f sOpts) opts) items
setSizeOpts f def                                                      = def

hasHSizeOpts :: !UI -> Bool
hasHSizeOpts (UI (UIControl (UIViewDocument sOpts vOpts)) items)      = True
hasHSizeOpts (UI (UIControl (UIViewSlider sOpts vOpts opts)) items)   = True
hasHSizeOpts (UI (UIControl (UIViewProgress sOpts vOpts opts)) items) = True
hasHSizeOpts (UI (UIControl (UIEditString sOpts eOpts)) items)        = True
hasHSizeOpts (UI (UIControl (UIEditPassword sOpts eOpts)) items)      = True
hasHSizeOpts (UI (UIControl (UIEditInt sOpts eOpts)) items)           = True
hasHSizeOpts (UI (UIControl (UIEditDecimal sOpts eOpts)) items)       = True
hasHSizeOpts (UI (UIControl (UIEditSlider sOpts eOpts opts)) items)   = True
hasHSizeOpts (UI (UIControl (UIEditDate sOpts eOpts)) items)          = True
hasHSizeOpts (UI (UIControl (UIEditTime sOpts eOpts)) items)          = True
hasHSizeOpts (UI (UIControl (UIEditDateTime sOpts eOpts)) items)      = True
hasHSizeOpts (UI (UIControl (UIEditDocument sOpts eOpts)) items)      = True
hasHSizeOpts (UI (UIControl (UIDropdown sOpts cOpts)) items)          = True
hasHSizeOpts (UI (UIControl (UILabel sOpts opts)) items)              = True
hasHSizeOpts _                                                        = False

getHSizeOpts :: (UIHSizeOpts -> a) UI -> a
getHSizeOpts f (UI (UIControl (UIViewDocument sOpts vOpts)) items)      = f sOpts
getHSizeOpts f (UI (UIControl (UIViewSlider sOpts vOpts opts)) items)   = f sOpts
getHSizeOpts f (UI (UIControl (UIViewProgress sOpts vOpts opts)) items) = f sOpts
getHSizeOpts f (UI (UIControl (UIEditString	sOpts eOpts)) items)        = f sOpts
getHSizeOpts f (UI (UIControl (UIEditPassword sOpts eOpts)) items)      = f sOpts
getHSizeOpts f (UI (UIControl (UIEditInt sOpts eOpts)) items)           = f sOpts
getHSizeOpts f (UI (UIControl (UIEditDecimal sOpts eOpts)) items)       = f sOpts
getHSizeOpts f (UI (UIControl (UIEditSlider sOpts eOpts opts)) items)   = f sOpts
getHSizeOpts f (UI (UIControl (UIEditDate sOpts eOpts)) items)          = f sOpts
getHSizeOpts f (UI (UIControl (UIEditTime sOpts eOpts)) items)          = f sOpts
getHSizeOpts f (UI (UIControl (UIEditDateTime sOpts eOpts)) items)      = f sOpts
getHSizeOpts f (UI (UIControl (UIEditDocument sOpts eOpts)) items)      = f sOpts
getHSizeOpts f (UI (UIControl (UIDropdown sOpts cOpts)) items)          = f sOpts
getHSizeOpts f (UI (UIControl (UILabel sOpts opts)) items)              = f sOpts

setHSizeOpts :: (UIHSizeOpts -> UIHSizeOpts) UI -> UI
setHSizeOpts f (UI (UIControl (UIViewDocument sOpts vOpts)) items)      = UI (UIControl (UIViewDocument (f sOpts) vOpts)) items
setHSizeOpts f (UI (UIControl (UIViewSlider sOpts vOpts opts)) items)   = UI (UIControl (UIViewSlider (f sOpts) vOpts opts)) items
setHSizeOpts f (UI (UIControl (UIViewProgress sOpts vOpts opts)) items) = UI (UIControl (UIViewProgress (f sOpts) vOpts opts)) items
setHSizeOpts f (UI (UIControl (UIEditString sOpts eOpts)) items)        = UI (UIControl (UIEditString (f sOpts) eOpts)) items
setHSizeOpts f (UI (UIControl (UIEditPassword sOpts eOpts)) items)      = UI (UIControl (UIEditPassword (f sOpts) eOpts)) items
setHSizeOpts f (UI (UIControl (UIEditInt sOpts eOpts)) items)           = UI (UIControl (UIEditInt (f sOpts) eOpts)) items
setHSizeOpts f (UI (UIControl (UIEditDecimal sOpts eOpts)) items)       = UI (UIControl (UIEditDecimal (f sOpts) eOpts)) items
setHSizeOpts f (UI (UIControl (UIEditSlider sOpts eOpts opts)) items)   = UI (UIControl (UIEditSlider (f sOpts) eOpts opts)) items
setHSizeOpts f (UI (UIControl (UIEditDate sOpts eOpts)) items)          = UI (UIControl (UIEditDate (f sOpts) eOpts)) items
setHSizeOpts f (UI (UIControl (UIEditTime sOpts eOpts)) items)          = UI (UIControl (UIEditTime (f sOpts) eOpts)) items
setHSizeOpts f (UI (UIControl (UIEditDateTime sOpts eOpts)) items)      = UI (UIControl (UIEditDateTime (f sOpts) eOpts)) items
setHSizeOpts f (UI (UIControl (UIEditDocument sOpts eOpts)) items)      = UI (UIControl (UIEditDocument (f sOpts) eOpts)) items
setHSizeOpts f (UI (UIControl (UIDropdown sOpts cOpts)) items)          = UI (UIControl (UIDropdown (f sOpts) cOpts)) items
setHSizeOpts f (UI (UIControl (UILabel sOpts opts)) items)              = UI (UIControl (UILabel (f sOpts) opts)) items

getMargins :: !UI -> (Maybe UISideSizes)
getMargins def
    | hasSizeOpts def  = getSizeOpts (\{UISizeOpts|margins} -> margins) def
    | hasFSizeOpts def = getFSizeOpts (\{UIFSizeOpts|margins} -> margins) def
    | hasHSizeOpts def = getHSizeOpts (\{UIHSizeOpts|margins} -> margins) def
                       = Nothing

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

instance encodeUI UI
where
	encodeUI (UI UIEmpty _) 				    = component "itwc_raw_empty" []
	encodeUI (UI (UIEditor _) [control]) 		= encodeUI control
	encodeUI (UI (UICompoundEditor opts) defs)	= component "itwc_raw_compoundeditor"  [encodeUI opts, JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UICompoundContent defs)		= component "itwc_raw_compoundcontent" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIParallel defs) 				= component "itwc_raw_parallel" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIInteract defs) 				= component "itwc_raw_interact" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIStep defs) 					= component "itwc_raw_step" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIAction action) _) 				= component "itwc_raw_action" [encodeUI action]
	encodeUI (UI (UIContainer sopts iopts) defs) = component "itwc_container" [encodeUI sopts, encodeUI iopts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIPanel sopts iopts opts) defs) = component "itwc_panel" [encodeUI sopts, encodeUI iopts, encodeUI opts
																		,JSONObject [("items",JSONArray (map encodeUI defs))]]

	encodeUI (UI (UITabSet sopts opts) defs)	    = component "itwc_tabset" [encodeUI sopts, encodeUI opts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UITab copts opts) defs) 	 	= component "itwc_tabitem" [encodeUI copts, encodeUI opts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIWindow sopts copts opts) defs) = component "itwc_window" [encodeUI sopts, encodeUI copts, encodeUI opts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIForm defs)					= component "itwc_raw_form" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIFormItem [label,def,info]) 	= component "itwc_raw_form_item" [JSONObject [("items",JSONArray[encodeUI label,encodeUI def,encodeUI info])]]
	encodeUI (UI (UIControl control) _)			= encodeUI control
	encodeUI (UI (UIBlock sopts copts) defs)    = component "itwc_raw_block" [encodeUI sopts, encodeUI copts,JSONObject [("items",JSONArray (map encodeUI defs))]]

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
	encodeUI (UITasklet sopts opts)				= component "itwc_tasklet" [encodeUI sopts, encodeUI opts]
	encodeUI (UIEditlet sopts opts)				= component "itwc_edit_editlet" [encodeUI sopts, encodeUI opts]
	encodeUI (UIEmbedding sopts opts)			= component "itwc_embedding" [encodeUI sopts, encodeUI opts]

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
	encodeUI {UIWindowOpts|windowType,title,iconCls,vpos,hpos,focusTaskId,closeTaskId}
    	= JSONObject [field \\ field <- [("windowType",encodeUI windowType)
                                     	,("title",encodeUI title)
                                     	,("iconCls",encodeUI iconCls)
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
