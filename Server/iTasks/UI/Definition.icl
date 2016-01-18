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

derive class iTask UI, UINodeType, UIAction, UIEditor
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UIWindowType
derive class iTask UIWindowOpts, UIContainerOpts, UISizeOpts, UIEditOpts, UIViewOpts, UIActionOpts
derive class iTask UIChoiceOpts, UIGridOpts, UITreeOpts, UIProgressOpts, UISliderOpts, UIEmbeddingOpts, UITabOpts
derive class iTask UIPanelOpts, UITabSetOpts, UIEditletOpts, UITaskletOpts, UIIconOpts, UILabelOpts
derive class iTask UIHSizeOpts, UIFSizeOpts, UIButtonOpts, UIMenuButtonOpts, UITreeNode, UIMenuItem

instance Functor UIViewOpts
where fmap f opts=:{UIViewOpts|value} = {UIViewOpts|opts & value = fmap f value}

ui :: UINodeType -> UI
ui type = UI type 'DM'.newMap []

uic :: UINodeType [UI] -> UI
uic type items = UI type 'DM'.newMap items

uia :: UINodeType UIAttributes -> UI
uia type attr = UI type attr []

uiac :: UINodeType UIAttributes [UI] -> UI
uiac type attr items = UI type attr items

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
setPadding top right bottom left (UI (UIContainer sOpts cOpts) attr items)
	= UI (UIContainer sOpts {UIContainerOpts|cOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}}) attr items
setPadding top right bottom left (UI (UIPanel sOpts cOpts pOpts) attr items)
	= UI (UIPanel sOpts {UIContainerOpts|cOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} pOpts) attr items
setPadding top right bottom left def = def

setTitle :: !String !UI -> UI
setTitle title (UI (UIPanel sOpts cOpts opts) attr items) = UI (UIPanel sOpts cOpts {UIPanelOpts|opts & title = Just (escapeStr title)}) attr items
setTitle title (UI (UITab cOpts opts) attr items) = UI (UITab cOpts {UITabOpts|opts & title = escapeStr title}) attr items
setTitle title (UI type attr items) = UI type ('DM'.put "title" title attr) items

setFramed :: !Bool !UI -> UI
setFramed frame (UI (UIPanel sOpts cOpts opts) attr items) = UI (UIPanel sOpts cOpts {UIPanelOpts|opts & frame = frame}) attr items
setFramed frame def = def

setIconCls :: !String !UI -> UI
setIconCls iconCls (UI (UIActionButton sOpts aOpts opts) attr items)
	= UI (UIActionButton sOpts aOpts {UIButtonOpts|opts & iconCls = Just iconCls}) attr items
setIconCls iconCls (UI (UIMenuButton sOpts opts) attr items)
	= UI (UIMenuButton sOpts {UIMenuButtonOpts|opts & iconCls = Just iconCls}) attr items
setIconCls iconCls (UI (UIIcon sOpts opts) attr items)
	= UI (UIIcon sOpts {UIIconOpts|opts & iconCls = iconCls}) attr items
setIconCls iconCls (UI (UIPanel sOpts cOpts pOpts) attr items)
	= UI (UIPanel sOpts cOpts {UIPanelOpts|pOpts & iconCls = Just iconCls}) attr items
setIconCls iconCls def = def

setBaseCls :: !String !UI -> UI
setBaseCls baseCls (UI (UIContainer sOpts cOpts) attr items)
	= UI (UIContainer sOpts {UIContainerOpts|cOpts & baseCls = Just baseCls}) attr items
setBaseCls baseCls (UI (UIPanel sOpts cOpts pOpts) attr items)
	= UI (UIPanel sOpts {UIContainerOpts|cOpts & baseCls = Just baseCls} pOpts) attr items
setBaseCls baseCls def = def

setDirection :: !UIDirection !UI -> UI
setDirection dir (UI (UIContainer sOpts cOpts) attr items)   = UI (UIContainer sOpts {UIContainerOpts|cOpts & direction = dir}) attr items
setDirection dir (UI (UIPanel sOpts cOpts pOpts) attr items) = UI (UIPanel sOpts {UIContainerOpts|cOpts & direction = dir} pOpts) attr items
setDirection dir def = def

setHalign :: !UIHAlign !UI -> UI
setHalign align (UI (UIContainer sOpts cOpts) attr items)   = UI (UIContainer sOpts {UIContainerOpts|cOpts & halign = align}) attr items
setHalign align (UI (UIPanel sOpts cOpts pOpts) attr items) = UI (UIPanel sOpts {UIContainerOpts|cOpts & halign = align} pOpts) attr items
setHalign align def = def

setValign :: !UIVAlign !UI -> UI
setValign align (UI (UIContainer sOpts cOpts) attr items)   = UI (UIContainer sOpts {UIContainerOpts|cOpts & valign = align}) attr items
setValign align (UI (UIPanel sOpts cOpts opts) attr items)  = UI (UIPanel sOpts {cOpts & valign = align} opts) attr items
setValign align def = def

defaultSizeOpts :: UISizeOpts
defaultSizeOpts = {UISizeOpts|width = Nothing, minWidth = Nothing, maxWidth = Nothing, height = Nothing, minHeight = Nothing, maxHeight = Nothing, margins = Nothing}

defaultHSizeOpts :: UIHSizeOpts
defaultHSizeOpts = {UIHSizeOpts|width = Nothing, minWidth = Nothing, maxWidth = Nothing, margins = Nothing}

defaultFSizeOpts :: UIFSizeOpts
defaultFSizeOpts = {UIFSizeOpts|margins = Nothing}

defaultContainerOpts :: UIContainerOpts
defaultContainerOpts = {UIContainerOpts|direction = Vertical, halign = AlignLeft, valign = AlignTop, padding = Nothing, baseCls=Nothing,bodyCls=Nothing}

defaultContainer :: UINodeType
defaultContainer = UIContainer defaultSizeOpts defaultContainerOpts

defaultPanel :: UINodeType
defaultPanel = UIPanel defaultSizeOpts defaultContainerOpts defaultPanelOpts

defaultPanelOpts :: UIPanelOpts
defaultPanelOpts = {UIPanelOpts|title=Nothing,iconCls=Nothing,frame=False,hotkeys=Nothing}

defaultTabSet :: UINodeType 
defaultTabSet = UITabSet defaultSizeOpts defaultTabSetOpts

defaultTabSetOpts :: UITabSetOpts
defaultTabSetOpts = {UITabSetOpts|activeTab = 0}

defaultTab :: UINodeType
defaultTab = UITab defaultContainerOpts defaultTabOpts

defaultTabOpts :: UITabOpts
defaultTabOpts = {UITabOpts|title="Untitled",iconCls=Nothing,focusTaskId=Nothing,closeTaskId=Nothing}

defaultWindow :: UINodeType
defaultWindow = UIWindow defaultSizeOpts defaultContainerOpts defaultWindowOpts

defaultWindowOpts :: UIWindowOpts
defaultWindowOpts = {UIWindowOpts|windowType=FloatingWindow,title=Nothing,iconCls=Nothing,vpos=Nothing,hpos=Nothing,closeTaskId=Nothing,focusTaskId=Nothing}

stringDisplay :: !String -> UI
stringDisplay value = ui (UIViewString defaultSizeOpts {UIViewOpts|value = Just (escapeStr value)})

hasFSizeOpts :: !UI -> Bool
hasFSizeOpts (UI (UIViewCheckbox sOpts vOpts) attr items) = True
hasFSizeOpts (UI (UIEditCheckbox sOpts eOpts) attr items) = True
hasFSizeOpts (UI (UIIcon sOpts opts) attr items)          = True
hasFSizeOpts _                                            = False

getFSizeOpts :: (UIFSizeOpts -> a) UI -> a
getFSizeOpts f (UI (UIViewCheckbox sOpts vOpts) attr items) = f sOpts
getFSizeOpts f (UI (UIEditCheckbox sOpts eOpts) attr items) = f sOpts
getFSizeOpts f (UI (UIIcon sOpts opts) attr items)          = f sOpts

setFSizeOpts :: (UIFSizeOpts -> UIFSizeOpts) UI -> UI
setFSizeOpts f (UI (UIViewCheckbox sOpts vOpts) attr items) = UI (UIViewCheckbox (f sOpts) vOpts) attr items
setFSizeOpts f (UI (UIEditCheckbox sOpts eOpts) attr items) = UI (UIEditCheckbox (f sOpts) eOpts) attr items
setFSizeOpts f (UI (UIIcon sOpts opts) attr items)          = UI (UIIcon (f sOpts) opts) attr items
setFSizeOpts f def = def

hasSizeOpts :: !UI -> Bool
hasSizeOpts (UI (UIViewString sOpts vOpts) attr items) = True
hasSizeOpts (UI (UIViewHtml sOpts vOpts) attr items) = True
hasSizeOpts (UI (UIEditNote sOpts eOpts) attr items) = True
hasSizeOpts (UI (UIEditButton sOpts eOpts opts) attr items)	= True
hasSizeOpts (UI (UIListChoice sOpts cOpts) attr items) = True
hasSizeOpts (UI (UIRadioGroup sOpts cOpts) attr items) = True
hasSizeOpts (UI (UICheckboxGroup sOpts cOpts) attr items) = True
hasSizeOpts (UI (UIGrid sOpts cOpts opts) attr items) = True
hasSizeOpts (UI (UITree sOpts cOpts opts) attr items) = True
hasSizeOpts (UI (UIActionButton sOpts aOpts opts) attr items) = True
hasSizeOpts (UI (UIMenuButton sOpts opts) attr items) = True
hasSizeOpts (UI (UITasklet sOpts opts) attr items) = True
hasSizeOpts (UI (UIEditlet sOpts opts) attr items) = True
hasSizeOpts (UI (UIContainer sOpts cOpts) attr items) = True
hasSizeOpts (UI (UIPanel sOpts cOpts pOpts) attr items) = True
hasSizeOpts (UI (UITabSet sOpts opts) attr items) = True
hasSizeOpts _ = False

getSizeOpts :: (UISizeOpts -> a) UI -> a
getSizeOpts f (UI (UIViewString	sOpts vOpts) attr items)         = f sOpts
getSizeOpts f (UI (UIViewHtml sOpts vOpts) attr items)           = f sOpts
getSizeOpts f (UI (UIEditNote sOpts eOpts) attr items)           = f sOpts
getSizeOpts f (UI (UIEditButton sOpts eOpts opts) attr items)    = f sOpts
getSizeOpts f (UI (UIListChoice sOpts cOpts) attr items)         = f sOpts
getSizeOpts f (UI (UIRadioGroup sOpts cOpts) attr items)         = f sOpts
getSizeOpts f (UI (UICheckboxGroup sOpts cOpts) attr items)      = f sOpts
getSizeOpts f (UI (UIGrid sOpts cOpts opts) attr items)          = f sOpts
getSizeOpts f (UI (UITree sOpts cOpts opts) attr items)          = f sOpts
getSizeOpts f (UI (UIActionButton sOpts aOpts opts) attr items)  = f sOpts
getSizeOpts f (UI (UIMenuButton sOpts opts) attr items)          = f sOpts
getSizeOpts f (UI (UITasklet sOpts opts) attr items)             = f sOpts
getSizeOpts f (UI (UIEditlet sOpts opts) attr items)             = f sOpts
getSizeOpts f (UI (UIContainer sOpts iOpts) attr items)          = f sOpts
getSizeOpts f (UI (UIPanel sOpts iOpts pOpts) attr items)        = f sOpts
getSizeOpts f (UI (UITabSet sOpts opts) attr items)              = f sOpts

setSizeOpts :: (UISizeOpts -> UISizeOpts) UI -> UI
setSizeOpts f (UI (UIViewString	sOpts vOpts) attr items)         = UI (UIViewString (f sOpts) vOpts) attr items
setSizeOpts f (UI (UIViewHtml sOpts vOpts) attr items)           = UI (UIViewHtml (f sOpts) vOpts) attr items
setSizeOpts f (UI (UIEditNote sOpts eOpts) attr items)           = UI (UIEditNote (f sOpts) eOpts) attr items
setSizeOpts f (UI (UIEditButton sOpts eOpts opts) attr items)    = UI (UIEditButton (f sOpts) eOpts opts) attr items
setSizeOpts f (UI (UIListChoice sOpts cOpts) attr items)         = UI (UIListChoice (f sOpts) cOpts) attr items
setSizeOpts f (UI (UIRadioGroup sOpts cOpts) attr items)         = UI (UIRadioGroup (f sOpts) cOpts) attr items
setSizeOpts f (UI (UICheckboxGroup sOpts cOpts) attr items)      = UI (UICheckboxGroup (f sOpts) cOpts) attr items
setSizeOpts f (UI (UIGrid sOpts cOpts opts) attr items)          = UI (UIGrid (f sOpts) cOpts opts) attr items
setSizeOpts f (UI (UITree sOpts cOpts opts) attr items)          = UI (UITree (f sOpts) cOpts opts) attr items
setSizeOpts f (UI (UIActionButton sOpts aOpts opts) attr items)  = UI (UIActionButton (f sOpts) aOpts opts) attr items
setSizeOpts f (UI (UIMenuButton sOpts opts) attr items)          = UI (UIMenuButton (f sOpts) opts) attr items
setSizeOpts f (UI (UITasklet sOpts opts) attr items)             = UI (UITasklet (f sOpts) opts) attr items
setSizeOpts f (UI (UIEditlet sOpts opts) attr items)             = UI (UIEditlet (f sOpts) opts) attr items
setSizeOpts f (UI (UIContainer sOpts cOpts) attr items)          = UI (UIContainer (f sOpts) cOpts) attr items
setSizeOpts f (UI (UIPanel sOpts cOpts pOpts) attr items)        = UI (UIPanel (f sOpts) cOpts pOpts) attr items
setSizeOpts f (UI (UITabSet sOpts opts) attr items)              = UI (UITabSet (f sOpts) opts) attr items
setSizeOpts f def                                                = def

hasHSizeOpts :: !UI -> Bool
hasHSizeOpts (UI (UIViewDocument sOpts vOpts) attr items)                  = True
hasHSizeOpts (UI (UIViewSlider sOpts vOpts opts) attr items)               = True
hasHSizeOpts (UI (UIViewProgress sOpts vOpts opts) attr items)             = True
hasHSizeOpts (UI (UIEditString sOpts eOpts) attr items)                    = True
hasHSizeOpts (UI (UIEditPassword sOpts eOpts) attr items)                  = True
hasHSizeOpts (UI (UIEditInt sOpts eOpts) attr items)                       = True
hasHSizeOpts (UI (UIEditDecimal sOpts eOpts) attr items)                   = True
hasHSizeOpts (UI (UIEditSlider sOpts eOpts opts) attr items)               = True
hasHSizeOpts (UI (UIEditDate sOpts eOpts) attr items)                      = True
hasHSizeOpts (UI (UIEditTime sOpts eOpts) attr items)                      = True
hasHSizeOpts (UI (UIEditDateTime sOpts eOpts) attr items)                  = True
hasHSizeOpts (UI (UIEditDocument sOpts eOpts) attr items)                  = True
hasHSizeOpts (UI (UIDropdown sOpts cOpts) attr items)                      = True
hasHSizeOpts (UI (UILabel sOpts opts) attr items)                          = True
hasHSizeOpts _                                                             = False

getHSizeOpts :: (UIHSizeOpts -> a) UI -> a
getHSizeOpts f (UI (UIViewDocument sOpts vOpts) attr items)                  = f sOpts
getHSizeOpts f (UI (UIViewSlider sOpts vOpts opts) attr items)               = f sOpts
getHSizeOpts f (UI (UIViewProgress sOpts vOpts opts) attr items)             = f sOpts
getHSizeOpts f (UI (UIEditString sOpts eOpts) attr items)                    = f sOpts
getHSizeOpts f (UI (UIEditPassword sOpts eOpts) attr items)                  = f sOpts
getHSizeOpts f (UI (UIEditInt sOpts eOpts) attr items)                       = f sOpts
getHSizeOpts f (UI (UIEditDecimal sOpts eOpts) attr items)                   = f sOpts
getHSizeOpts f (UI (UIEditSlider sOpts eOpts opts) attr items)               = f sOpts
getHSizeOpts f (UI (UIEditDate sOpts eOpts) attr items)                      = f sOpts
getHSizeOpts f (UI (UIEditTime sOpts eOpts) attr items)                      = f sOpts
getHSizeOpts f (UI (UIEditDateTime sOpts eOpts) attr items)                  = f sOpts
getHSizeOpts f (UI (UIEditDocument sOpts eOpts) attr items)                  = f sOpts
getHSizeOpts f (UI (UIDropdown sOpts cOpts) attr items)                      = f sOpts
getHSizeOpts f (UI (UILabel sOpts opts) attr items)                          = f sOpts

setHSizeOpts :: (UIHSizeOpts -> UIHSizeOpts) UI -> UI
setHSizeOpts f (UI (UIViewDocument sOpts vOpts) attr items)                  = UI (UIViewDocument (f sOpts) vOpts) attr items
setHSizeOpts f (UI (UIViewSlider sOpts vOpts opts) attr items)               = UI (UIViewSlider (f sOpts) vOpts opts) attr items
setHSizeOpts f (UI (UIViewProgress sOpts vOpts opts) attr items)             = UI (UIViewProgress (f sOpts) vOpts opts) attr items
setHSizeOpts f (UI (UIEditString sOpts eOpts) attr items)                    = UI (UIEditString (f sOpts) eOpts) attr items
setHSizeOpts f (UI (UIEditPassword sOpts eOpts) attr items)                  = UI (UIEditPassword (f sOpts) eOpts) attr items
setHSizeOpts f (UI (UIEditInt sOpts eOpts) attr items)                       = UI (UIEditInt (f sOpts) eOpts) attr items
setHSizeOpts f (UI (UIEditDecimal sOpts eOpts) attr items)                   = UI (UIEditDecimal (f sOpts) eOpts) attr items
setHSizeOpts f (UI (UIEditSlider sOpts eOpts opts) attr items)               = UI (UIEditSlider (f sOpts) eOpts opts) attr items
setHSizeOpts f (UI (UIEditDate sOpts eOpts) attr items)                      = UI (UIEditDate (f sOpts) eOpts) attr items
setHSizeOpts f (UI (UIEditTime sOpts eOpts) attr items)                      = UI (UIEditTime (f sOpts) eOpts) attr items
setHSizeOpts f (UI (UIEditDateTime sOpts eOpts) attr items)                  = UI (UIEditDateTime (f sOpts) eOpts) attr items
setHSizeOpts f (UI (UIEditDocument sOpts eOpts) attr items)                  = UI (UIEditDocument (f sOpts) eOpts) attr items
setHSizeOpts f (UI (UIDropdown sOpts cOpts) attr items)                      = UI (UIDropdown (f sOpts) cOpts) attr items
setHSizeOpts f (UI (UILabel sOpts opts) attr items)                          = UI (UILabel (f sOpts) opts) attr items

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
	encodeUI (UI UIEmpty _ _)                           = component "itwc_raw_empty" []
	encodeUI (UI (UIEditor _) attr [control])           = encodeUI control
	encodeUI (UI (UIEditor opts) attr defs)     		= component "itwc_raw_compoundeditor" [encodeUI opts, JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UICompoundContent attr defs)           = component "itwc_raw_compoundcontent" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIParallel attr defs)                  = component "itwc_raw_parallel" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIInteract attr defs)                  = component "itwc_raw_interact" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIStep attr defs)                      = component "itwc_raw_step" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIAction action) _ _)                 = component "itwc_raw_action" [encodeUI action]
	encodeUI (UI (UIContainer sopts iopts) attr defs)   = component "itwc_container" [encodeUI sopts, encodeUI iopts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIPanel sopts iopts opts) attr defs)  = component "itwc_panel" [encodeUI sopts, encodeUI iopts, encodeUI opts
																		,JSONObject [("items",JSONArray (map encodeUI defs))]]

	encodeUI (UI (UITabSet sopts opts) attr defs)	    = component "itwc_tabset" [encodeUI sopts, encodeUI opts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UITab copts opts) attr defs)          = component "itwc_tabitem" [encodeUI copts, encodeUI opts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIWindow sopts copts opts) attr defs) = component "itwc_window" [encodeUI sopts, encodeUI copts, encodeUI opts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIForm attr defs)                      = component "itwc_raw_form" [JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIFormItem attr [label,def,info])      = component "itwc_raw_form_item" [JSONObject [("items",JSONArray[encodeUI label,encodeUI def,encodeUI info])]]
	encodeUI (UI (UIBlock sopts copts) attr defs)       = component "itwc_raw_block" [encodeUI sopts, encodeUI copts,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIViewString sopts vopts)	_ _)		= component "itwc_view_string" [encodeUI sopts,encodeUI vopts]
	encodeUI (UI (UIViewHtml sopts vopts) _ _ )			= component "itwc_view_html" [encodeUI sopts, encodeUI vopts]
	encodeUI (UI (UIViewDocument sopts vopts) _ _)		= component "itwc_view_document" [encodeUI sopts, encodeUI vopts]
	encodeUI (UI (UIViewCheckbox sopts vopts) _ _)		= component "itwc_view_checkbox" [encodeUI sopts, encodeUI vopts]
	encodeUI (UI (UIViewSlider sopts vopts opts) _ _)	= component "itwc_view_slider" [encodeUI sopts, encodeUI vopts, encodeUI opts]
	encodeUI (UI (UIViewProgress sopts vopts opts) _ _)	= component "itwc_view_progress" [encodeUI sopts, encodeUI vopts, encodeUI opts]
	encodeUI (UI (UIIcon sopts opts) _ _)				= component "itwc_view_icon" [encodeUI sopts, encodeUI opts]
	encodeUI (UI (UIEditString sopts eopts) _ _)        = component "itwc_edit_string" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditNote sopts eopts) _ _)          = component "itwc_edit_note" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditPassword sopts eopts) _ _)      = component "itwc_edit_password" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditInt sopts eopts) _ _)           = component "itwc_edit_int" [encodeUI sopts, encodeUI  eopts]
	encodeUI (UI (UIEditDecimal sopts eopts) _ _)       = component "itwc_edit_decimal" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditCheckbox sopts eopts) _ _)      = component "itwc_edit_checkbox" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditSlider sopts eopts opts) _ _)   = component "itwc_edit_slider" [encodeUI sopts, encodeUI eopts, encodeUI opts]
	encodeUI (UI (UIEditDate sopts eopts) _ _)          = component "itwc_edit_date" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditTime sopts eopts) _ _)          = component "itwc_edit_time" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditDateTime sopts eopts) _ _)      = component "itwc_edit_datetime" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditDocument sopts eopts) _ _)      = component "itwc_edit_document" [encodeUI sopts, encodeUI eopts]
	encodeUI (UI (UIEditButton sopts eopts opts) _ _)   = component "itwc_editbutton" [encodeUI sopts, encodeUI eopts, encodeUI opts]
	encodeUI (UI (UIDropdown sopts copts) _ _)          = component "itwc_choice_dropdown" [encodeUI sopts, encodeUI copts]
	encodeUI (UI (UIListChoice sopts copts) _ _)        = component "itwc_choice_list" [encodeUI sopts, encodeUI copts]
	encodeUI (UI (UIRadioGroup sopts copts)	_ _)        = component "itwc_choice_radiogroup" [encodeUI sopts, encodeUI copts]
	encodeUI (UI (UICheckboxGroup sopts copts) _ _)     = component "itwc_choice_checkboxgroup" [encodeUI sopts, encodeUI copts]
	encodeUI (UI (UIGrid sopts copts opts) _ _)         = component "itwc_choice_grid" [encodeUI sopts, encodeUI copts, encodeUI opts]
	encodeUI (UI (UITree sopts copts opts) _ _)         = component "itwc_choice_tree" [encodeUI sopts, encodeUI copts, encodeUI opts]
	encodeUI (UI (UIActionButton sopts aopts opts) _ _) = component "itwc_actionbutton" [encodeUI sopts, encodeUI aopts, encodeUI opts]
    encodeUI (UI (UIMenuButton sopts opts) _ _)         = component "itwc_menubutton" [encodeUI sopts, encodeUI opts]
	encodeUI (UI (UILabel sopts opts) _ _)				= component "itwc_label" [encodeUI sopts, encodeUI opts]
	encodeUI (UI (UISplitter) _ _)						= component "itwc_splitter" []
	encodeUI (UI (UITasklet sopts opts) _ _)            = component "itwc_tasklet" [encodeUI sopts, encodeUI opts]
	encodeUI (UI (UIEditlet sopts opts)	_ _)            = component "itwc_edit_editlet" [encodeUI sopts, encodeUI opts]
	encodeUI (UI (UIEmbedding sopts opts) _ _)          = component "itwc_embedding" [encodeUI sopts, encodeUI opts]

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
	encodeUI {UIEditor|optional}
		= JSONObject [("optional",encodeUI optional)
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
