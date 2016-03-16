implementation module iTasks.UI.Definition

import Text.JSON, StdList, StdOrdList, StdBool, StdTuple, GenEq, StdFunc, Text.HTML, Text
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
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UISide, UIWindowType
derive class iTask UIEditOpts, UIViewOpts, UIActionOpts
derive class iTask UIChoiceOpts, UIGridOpts, UITreeOpts, UIEmbeddingOpts 
derive class iTask UIEditletOpts, UITaskletOpts 
derive class iTask UIButtonOpts, UIMenuButtonOpts, UITreeNode, UIMenuItem

instance Functor UIViewOpts
where fmap f opts=:{UIViewOpts|value} = {UIViewOpts|opts & value = fmap f value}

//SHOULD BE IN Text.JSON
jsonObjectPut :: String JSONNode JSONNode -> JSONNode
jsonObjectPut k v (JSONObject fields) = JSONObject (put k v fields)
where
	put k v [] = [(k,v)]
	put k v [(fk,fv):fs] = if (k == fk) [(fk,v):fs] [(fk,fv):put k v fs]
jsonObjectPut k v node = node

jsonObjectGet :: String JSONNode -> Maybe JSONNode
jsonObjectGet k (JSONObject fields) = get k fields
where
	get k [] = Nothing
	get k [(fk,fv):fs] = if (k == fk) (Just fv) (get fk fs)
jsonObjectGet k node = Nothing

ui :: UINodeType -> UI
ui type = UI type 'DM'.newMap []

uic :: UINodeType [UI] -> UI
uic type items = UI type 'DM'.newMap items

uia :: UINodeType UIAttributes -> UI
uia type attr = UI type attr []

uiac :: UINodeType UIAttributes [UI] -> UI
uiac type attr items = UI type attr items

setSize :: !UISize !UISize !UI -> UI
setSize width heigth ui = (setWidth width o setHeight heigth) ui

setWidth :: !UISize !UI -> UI
setWidth width (UI type attr items) = UI type ('DM'.put "itwcWidth" (encodeUI width) attr) items

setHeight :: !UISize !UI -> UI
setHeight height (UI type attr items) = UI type ('DM'.put "itwcHeight" (encodeUI height) attr) items

setMinSize :: !UIBound !UIBound !UI -> UI
setMinSize minWidth minHeight ui = (setMinWidth minWidth o setMinHeight minHeight) ui

setMinWidth :: !UIBound !UI -> UI
setMinWidth minWidth (UI type attr items) = UI type ('DM'.put "itwcMinWidth" (encodeUI minWidth) attr) items

setMinHeight :: !UIBound !UI -> UI
setMinHeight minHeight (UI type attr items) = UI type ('DM'.put "itwcMinHeight" (encodeUI minHeight) attr) items

setMaxSize :: !UIBound !UIBound !UI -> UI
setMaxSize maxWidth maxHeight ui = (setMaxWidth maxWidth o setMaxHeight maxHeight) ui

setMaxWidth :: !UIBound !UI -> UI
setMaxWidth maxWidth (UI type attr items) = UI type ('DM'.put "itwcMaxWidth" (encodeUI maxWidth) attr) items

setMaxHeight :: !UIBound !UI -> UI
setMaxHeight maxHeight (UI type attr items) = UI type ('DM'.put "itwcMaxHeight" (encodeUI maxHeight) attr) items

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
setMargins top right bottom left (UI type attr items) = UI type ('DM'.put "margins" margins attr) items
where
	margins = JSONObject [("top",JSONInt top),("right",JSONInt right),("bottom",JSONInt bottom),("left",JSONInt left)]

setMargin :: !String !Int !UI -> UI
setMargin margin value (UI type attr items) = UI type ('DM'.put "margins" margins attr) items
where
	margins = jsonObjectPut margin (JSONInt value) (fromMaybe (JSONObject []) ('DM'.get "margins" attr))

setTopMargin :: !Int !UI -> UI
setTopMargin top ui = setMargin "top" top ui

setRightMargin	:: !Int !UI -> UI
setRightMargin right ui = setMargin "right" right ui

setBottomMargin	:: !Int !UI -> UI
setBottomMargin bottom ui = setMargin "bottom" bottom ui

setLeftMargin :: !Int !UI -> UI
setLeftMargin left ui = setMargin "top" left ui

setPadding :: !Int !Int !Int !Int !UI -> UI
setPadding top right bottom left (UI type attr items) = UI type ('DM'.put "padding" padding attr) items
where
	padding = JSONObject [("top",JSONInt top),("right",JSONInt right),("bottom",JSONInt bottom),("left",JSONInt left)]

setPaddingSide :: !String !Int !UI -> UI
setPaddingSide side value (UI type attr items) = UI type ('DM'.put "padding" padding attr) items
where
	padding = jsonObjectPut side (JSONInt value) (fromMaybe (JSONObject []) ('DM'.get "padding" attr))

setTopPadding :: !Int !UI -> UI
setTopPadding top ui = setPaddingSide "top" top ui

setRightPadding :: !Int !UI -> UI
setRightPadding right ui = setPaddingSide "right" right ui

setBottomPadding :: !Int !UI -> UI
setBottomPadding bottom ui = setPaddingSide "bottom" bottom ui

setLeftPadding :: !Int !UI -> UI
setLeftPadding left ui = setPaddingSide "left" left ui

setTitle :: !String !UI -> UI
setTitle title (UI type attr items) = UI type ('DM'.put "title" (JSONString title) attr) items

setFramed :: !Bool !UI -> UI
setFramed frame (UI type attr items) = UI type ('DM'.put "frame" (JSONBool frame) attr) items

setIconCls :: !String !UI -> UI
setIconCls iconCls (UI type attr items) = UI type ('DM'.put "iconCls" (encodeUI iconCls) attr) items

setBaseCls :: !String !UI -> UI
setBaseCls baseCls (UI type attr items) = UI type ('DM'.put "baseCls" (encodeUI baseCls) attr) items

setTooltip :: !String !UI -> UI
setTooltip tooltip (UI type attr items) = UI type ('DM'.put "tooltip" (JSONString tooltip) attr) items

setDirection :: !UIDirection !UI -> UI
setDirection direction (UI type attr items) = UI type ('DM'.put "direction" (encodeUI direction) attr) items

setHalign :: !UIHAlign !UI -> UI
setHalign align (UI type attr items) = UI type ('DM'.put "halign" (encodeUI align) attr) items

setValign :: !UIVAlign !UI -> UI
setValign align (UI type attr items) = UI type ('DM'.put "valign" (encodeUI align) attr) items

setHpos :: !UIHAlign !UI -> UI
setHpos pos (UI type attr items) = UI type ('DM'.put "hpos" (encodeUI pos) attr) items

setVpos :: !UIVAlign !UI -> UI
setVpos pos (UI type attr items) = UI type ('DM'.put "vpos" (encodeUI pos) attr) items

setWindowType :: !UIWindowType !UI -> UI
setWindowType windowType (UI type attr items) = UI type ('DM'.put "windowType" (encodeUI windowType) attr) items

setFocusTaskId ::!String !UI -> UI
setFocusTaskId taskId (UI type attr items) = UI type ('DM'.put "focusTaskId" (JSONString taskId) attr) items

setCloseTaskId :: !String !UI -> UI
setCloseTaskId taskId (UI type attr items) = UI type ('DM'.put "closeTaskId" (JSONString taskId) attr) items

setActiveTab :: !Int !UI -> UI
setActiveTab activeTab (UI type attr items) = UI type ('DM'.put "activeTab" (JSONInt activeTab) attr) items

setValue :: !JSONNode !UI -> UI
setValue value (UI type attr items) = UI type ('DM'.put "value" value attr) items

setMinValue :: !Int !UI -> UI
setMinValue minValue (UI type attr items) = UI type ('DM'.put "minValue" (JSONInt minValue) attr) items

setMaxValue :: !Int !UI -> UI
setMaxValue maxValue (UI type attr items) = UI type ('DM'.put "maxValue" (JSONInt maxValue) attr) items

setText :: !String !UI -> UI
setText text (UI type attr items) = UI type ('DM'.put "text" (JSONString text) attr) items

stringDisplay :: !String -> UI
stringDisplay value = ui (UIViewString {UIViewOpts|value = Just (escapeStr value)})

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
	encodeUI (UI UIEmpty attr _)                           = component "itwc_raw_empty" [encodeAttr attr]
	encodeUI (UI (UIEditor _) attr [control])              = encodeUI control
	encodeUI (UI (UIEditor opts) attr defs)     		   = component "itwc_raw_compoundeditor" [encodeAttr attr,encodeUI opts, JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UICompoundContent attr defs)              = component "itwc_raw_compoundcontent" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIParallel attr defs)                     = component "itwc_raw_parallel" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIInteract attr defs)                     = component "itwc_raw_interact" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIStep attr defs)                         = component "itwc_raw_step" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIAction action) attr _)                 = component "itwc_raw_action" [encodeAttr attr,encodeUI action]
	encodeUI (UI UIContainer attr defs)                    = component "itwc_container" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIPanel attr defs)                        = component "itwc_panel" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]

	encodeUI (UI UITabSet attr defs)	                   = component "itwc_tabset" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UITab attr defs)                          = component "itwc_tabitem" [encodeAttr attr, JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIWindow attr defs)                       = component "itwc_window" [encodeAttr attr, JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIForm attr defs)                         = component "itwc_raw_form" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI UIFormItem attr [label,def,info])         = component "itwc_raw_form_item" [encodeAttr attr,JSONObject [("items",JSONArray[encodeUI label,encodeUI def,encodeUI info])]]
	encodeUI (UI UIFormItem attr defs)                     = component "itwc_raw_form_item" [encodeAttr attr,JSONObject [("items",JSONArray (map encodeUI defs))]]
	encodeUI (UI (UIViewString vopts)	attr _)      = component "itwc_view_string" [encodeAttr attr, encodeUI vopts]
	encodeUI (UI (UIViewHtml vopts) attr _ )         = component "itwc_view_html" [encodeAttr attr, encodeUI vopts]
	encodeUI (UI (UIViewDocument vopts) attr _)      = component "itwc_view_document" [encodeAttr attr, encodeUI vopts]
	encodeUI (UI (UIViewCheckbox vopts) attr _)      = component "itwc_view_checkbox" [encodeAttr attr, encodeUI vopts]
	encodeUI (UI UIViewSlider attr _)                = component "itwc_view_slider" [encodeAttr attr]
	encodeUI (UI (UIViewProgress vopts) attr _)      = component "itwc_view_progress" [encodeAttr attr, encodeUI vopts]
	encodeUI (UI UIIcon attr _)                      = component "itwc_view_icon" [encodeAttr attr]
	encodeUI (UI (UIEditString eopts) attr _)        = component "itwc_edit_string" [encodeAttr attr,  encodeUI eopts]
	encodeUI (UI (UIEditNote eopts) attr _)          = component "itwc_edit_note" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditPassword eopts) attr _)      = component "itwc_edit_password" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditInt eopts) attr _)           = component "itwc_edit_int" [encodeAttr attr,  encodeUI  eopts]
	encodeUI (UI (UIEditDecimal eopts) attr _)       = component "itwc_edit_decimal" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditCheckbox eopts) attr _)      = component "itwc_edit_checkbox" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditSlider eopts) attr _)        = component "itwc_edit_slider" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditDate eopts) attr _)          = component "itwc_edit_date" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditTime eopts) attr _)          = component "itwc_edit_time" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditDateTime eopts) attr _)      = component "itwc_edit_datetime" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditDocument eopts) attr _)      = component "itwc_edit_document" [encodeAttr attr, encodeUI eopts]
	encodeUI (UI (UIEditButton eopts opts) attr _)   = component "itwc_editbutton" [encodeAttr attr, encodeUI eopts, encodeUI opts]
	encodeUI (UI (UIDropdown copts) attr _)          = component "itwc_choice_dropdown" [encodeAttr attr, encodeUI copts]
	encodeUI (UI (UIListChoice copts) attr _)        = component "itwc_choice_list" [encodeAttr attr, encodeUI copts]
	encodeUI (UI (UIRadioGroup copts)	attr _)      = component "itwc_choice_radiogroup" [encodeAttr attr, encodeUI copts]
	encodeUI (UI (UICheckboxGroup copts) attr _)     = component "itwc_choice_checkboxgroup" [encodeAttr attr, encodeUI copts]
	encodeUI (UI (UIGrid copts opts) attr _)         = component "itwc_choice_grid" [encodeAttr attr, encodeUI copts, encodeUI opts]
	encodeUI (UI (UITree copts opts) attr _)         = component "itwc_choice_tree" [encodeAttr attr, encodeUI copts, encodeUI opts]
	encodeUI (UI (UIActionButton aopts opts) attr _) = component "itwc_actionbutton" [encodeAttr attr, encodeUI aopts, encodeUI opts]
    encodeUI (UI (UIMenuButton opts) attr _)         = component "itwc_menubutton" [encodeAttr attr, encodeUI opts]
	encodeUI (UI UILabel attr _)                     = component "itwc_label" [encodeAttr attr]
	encodeUI (UI (UISplitter) attr _)                = component "itwc_splitter" [encodeAttr attr]
	encodeUI (UI (UITasklet opts) attr _)            = component "itwc_tasklet" [encodeAttr attr, encodeUI opts]
	encodeUI (UI (UIEditlet opts)	attr _)            = component "itwc_edit_editlet" [encodeAttr attr, encodeUI opts]
	encodeUI (UI (UIEmbedding opts) attr _)          = component "itwc_embedding" [encodeAttr attr, encodeUI opts]

encodeAttr attr	= JSONObject [(k,encode k v) \\ (k,v) <- 'DM'.toList attr]
where
	//Special cases...
	encode "margins" margins = encodeSides margins
	encode "padding" padding = encodeSides padding
	encode k v = v 

	encodeSides sides = JSONString (toString top +++ " " +++ toString right +++ " " +++ toString bottom +++ " " +++ toString left)
	where
		top    = maybe 0 (\(JSONInt x) -> x) (jsonObjectGet "top" sides)
		right  = maybe 0 (\(JSONInt x) -> x) (jsonObjectGet "right" sides)
		bottom = maybe 0 (\(JSONInt x) -> x) (jsonObjectGet "bottom" sides)
		left   = maybe 0 (\(JSONInt x) -> x) (jsonObjectGet "left" sides)


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
instance encodeUI UITreeOpts where encodeUI opts = toJSON opts
instance encodeUI UIGridOpts where encodeUI opts = toJSON opts

instance encodeUI (UIChoiceOpts a) | JSONEncode{|*|} a 
where
	 encodeUI opts = toJSON opts

instance encodeUI UIEditletOpts
where
	encodeUI opts = let (JSONObject fields) = toJSON opts in JSONObject [field \\ field <- fields | fst field <> "value"]

component :: String [JSONNode] -> JSONNode
component xtype opts = JSONObject [("xtype",JSONString xtype):optsfields]
where
	optsfields = flatten [fields \\ JSONObject fields <- opts]


derive class iTask UIChange, UIChildChange

//Remove unnessecary directives
compactChangeDef :: UIChange -> UIChange
compactChangeDef (ChangeUI localChanges children)
	= case ChangeUI localChanges [child \\ child=:(_,ChangeChild change) <- map compactChildDef children | not (change =: NoChange)] of
		ChangeUI [] [] 	= NoChange
		def 			= def
where
	compactChildDef (idx,ChangeChild change) = (idx,ChangeChild change)
	compactChildDef def = def

compactChangeDef def = def

completeChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]
completeChildChanges children = complete 0 (sortBy indexCmp children)
where
	complete i [] = []
	complete i [c:cs]
		| i < fst c = [(i,ChangeChild NoChange):complete (i + 1) cs]
					= [c:complete (fst c + 1) cs]
	indexCmp x y = fst x < fst y

reindexChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]
reindexChildChanges children = [(i,c) \\ (_,c) <- children & i <- [0..]]

compactChildChanges :: [(Int,UIChildChange)] -> [(Int,UIChildChange)]
compactChildChanges children = [c \\ c <- children | not (noChangeChild c)]
where
	noChangeChild (_,ChangeChild NoChange) = True
	noChangeChild _ = False

encodeUIChanges:: ![UIChange] -> JSONNode
encodeUIChanges defs = JSONArray (map encodeUIChange defs)

encodeUIChange :: !UIChange -> JSONNode
encodeUIChange NoChange = JSONNull
encodeUIChange (ReplaceUI def)
	= JSONObject
		[("type",JSONString "replace")
		,("definition",encodeUI def)
		]
encodeUIChange (ChangeUI operations children)
	= JSONObject
		[("type",JSONString "change")
		,("operations", JSONArray [JSONObject [("method",JSONString method),("arguments",JSONArray arguments)] 
											\\ (method,arguments) <- operations])
		,("children",JSONArray (map encodeChildChange children))
		]
where
	encodeChildChange (i,ChangeChild child) = JSONArray [JSONInt i,JSONString "change",encodeUIChange child]
	encodeChildChange (i,RemoveChild) 		= JSONArray [JSONInt i,JSONString "remove"]
	encodeChildChange (i,InsertChild child) = JSONArray [JSONInt i,JSONString "insert",encodeUI child]

