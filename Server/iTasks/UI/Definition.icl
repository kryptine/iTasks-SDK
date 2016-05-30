implementation module iTasks.UI.Definition

import Text.JSON, StdList, StdOrdList, StdBool, StdTuple, GenEq, StdFunc, Text.HTML, Text
from Data.Map import :: Map (..)
from Data.Functor import class Functor(..)
import qualified Data.Map as DM
import qualified Data.List as DL
from iTasks.API.Core.Types import :: Document, :: DocumentId, :: Date, :: Time, :: ProgressAmount(..), :: Action(..), ::ActionName, :: ActionOption, :: Hotkey

from iTasks._Framework.Generic import class iTask(..)
from iTasks._Framework.Generic.Visualization	import generic gText, :: TextFormat(..)
from iTasks._Framework.Generic.Defaults			import generic gDefault
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
from iTasks.UI.Editor.Generic import generic gEditor
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from GenEq import generic gEq

import Text.HTML

derive class iTask UI, UINodeType
derive class iTask UISize, UIBound, UISideSizes, UIDirection, UIVAlign, UIHAlign, UISide, UIWindowType
derive class iTask UITreeNode 

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
	get k [(fk,fv):fs] = if (k == fk) (Just fv) (get k fs)
jsonObjectGet k node = Nothing

ui :: UINodeType -> UI
ui type = UI type 'DM'.newMap []

uic :: UINodeType [UI] -> UI
uic type items = UI type 'DM'.newMap items

uia :: UINodeType UIAttributes -> UI
uia type attr = UI type attr []

uiac :: UINodeType UIAttributes [UI] -> UI
uiac type attr items = UI type attr items

optionalAttr :: !Bool -> UIAttributes
optionalAttr optional = 'DM'.fromList [("optional",JSONBool optional)]

sizeAttr :: !UISize !UISize -> UIAttributes
sizeAttr width height = 'DM'.fromList [("width",encodeUI width),("height",encodeUI height)]

widthAttr :: !UISize -> UIAttributes
widthAttr width = 'DM'.fromList [("width",encodeUI width)]

heightAttr :: !UISize -> UIAttributes
heightAttr height = 'DM'.fromList [("height",encodeUI height)]

minSizeAttr :: !UIBound !UIBound -> UIAttributes
minSizeAttr minWidth minHeight = 'DM'.fromList [("minWidth",encodeUI minWidth),("minHeight",encodeUI minHeight)]

minWidthAttr :: !UIBound -> UIAttributes
minWidthAttr minWidth = 'DM'.fromList [("minWidth",encodeUI minWidth)]

minHeightAttr :: !UIBound -> UIAttributes
minHeightAttr minHeight = 'DM'.fromList [("minHeight",encodeUI minHeight)]

maxSizeAttr :: !UIBound !UIBound -> UIAttributes
maxSizeAttr maxWidth maxHeight = 'DM'.fromList [("maxWidth",encodeUI maxWidth),("maxHeight",encodeUI maxHeight)]

maxWidthAttr :: !UIBound -> UIAttributes
maxWidthAttr maxWidth = 'DM'.fromList [("maxWidth",encodeUI maxWidth)]

maxHeightAttr :: !UIBound -> UIAttributes
maxHeightAttr maxHeight = 'DM'.fromList [("maxHeight",encodeUI maxHeight)]

marginsAttr :: !Int !Int !Int !Int -> UIAttributes
marginsAttr top right bottom left = 'DM'.fromList [("marginTop",JSONInt top),("marginRight",JSONInt right),("marginBottom",JSONInt bottom),("marginLeft",JSONInt left)]

topMarginAttr :: !Int -> UIAttributes
topMarginAttr top = 'DM'.fromList [("topMargin",JSONInt top)]

rightMarginAttr :: !Int -> UIAttributes
rightMarginAttr right = 'DM'.fromList [("rightMargin",JSONInt right)]

bottomMarginAttr :: !Int -> UIAttributes
bottomMarginAttr bottom = 'DM'.fromList [("bottomMargin",JSONInt bottom)]

leftMarginAttr :: !Int -> UIAttributes
leftMarginAttr left = 'DM'.fromList [("leftMargin",JSONInt left)]

paddingAttr :: !Int !Int !Int !Int -> UIAttributes
paddingAttr top right bottom left = 'DM'.fromList [("paddingTop",JSONInt top),("paddingRight",JSONInt right),("paddingBottom",JSONInt bottom),("paddingLeft",JSONInt left)]

topPaddingAttr :: !Int -> UIAttributes
topPaddingAttr top = 'DM'.fromList [("topPadding",JSONInt top)]

rightPaddingAttr :: !Int -> UIAttributes
rightPaddingAttr right = 'DM'.fromList [("rightPadding",JSONInt right)]

bottomPaddingAttr :: !Int -> UIAttributes
bottomPaddingAttr bottom = 'DM'.fromList [("bottomPadding",JSONInt bottom)]

leftPaddingAttr :: !Int -> UIAttributes
leftPaddingAttr left = 'DM'.fromList [("leftPadding",JSONInt left)]

titleAttr :: !String -> UIAttributes
titleAttr title = 'DM'.fromList [("title",JSONString title)]

frameAttr :: !Bool -> UIAttributes
frameAttr frame = 'DM'.fromList [("frame",JSONBool frame)]

iconClsAttr :: !String -> UIAttributes
iconClsAttr iconCls = 'DM'.fromList [("iconCls",JSONString iconCls)]

baseClsAttr :: !String -> UIAttributes
baseClsAttr baseCls = 'DM'.fromList [("baseCls",JSONString baseCls)]

tooltipAttr :: !String -> UIAttributes
tooltipAttr tooltip = 'DM'.fromList [("tooltip",JSONString tooltip)]

directionAttr :: !UIDirection -> UIAttributes
directionAttr direction = 'DM'.fromList [("direction",encodeUI direction)]

halignAttr :: !UIHAlign -> UIAttributes
halignAttr align = 'DM'.fromList [("halign",encodeUI align)]

valignAttr :: !UIVAlign -> UIAttributes
valignAttr align = 'DM'.fromList [("valign",encodeUI align)]

hposAttr :: !UIHAlign -> UIAttributes
hposAttr pos = 'DM'.fromList [("hpos",encodeUI pos)]

vposAttr :: !UIVAlign -> UIAttributes
vposAttr pos = 'DM'.fromList [("vpos",encodeUI pos)]

windowTypeAttr :: !UIWindowType -> UIAttributes
windowTypeAttr windowType = 'DM'.fromList [("windowType",encodeUI windowType)]

focusTaskIdAttr :: !String -> UIAttributes
focusTaskIdAttr taskId = 'DM'.fromList [("focusTaskId",JSONString taskId)]

closeTaskIdAttr :: !String -> UIAttributes
closeTaskIdAttr taskId = 'DM'.fromList [("closeTaskId",JSONString taskId)]

activeTabAttr :: !Int -> UIAttributes
activeTabAttr activeTab = 'DM'.fromList [("activeTab",JSONInt activeTab)]

valueAttr :: !JSONNode -> UIAttributes
valueAttr value = 'DM'.fromList [("value",value)]

minValueAttr :: !Int -> UIAttributes
minValueAttr minValue = 'DM'.fromList [("minValue",JSONInt minValue)]

maxValueAttr :: !Int -> UIAttributes
maxValueAttr maxValue = 'DM'.fromList [("maxValue",JSONInt maxValue)]

textAttr :: !String -> UIAttributes
textAttr text = 'DM'.fromList [("text",JSONString text)]

enabledAttr :: !Bool -> UIAttributes
enabledAttr enabled = 'DM'.fromList [("enabled",JSONBool enabled)]

instanceNoAttr :: !Int -> UIAttributes
instanceNoAttr instanceNo = 'DM'.fromList [("instanceNo",JSONInt instanceNo)]

instanceKeyAttr :: !String -> UIAttributes
instanceKeyAttr instanceKey = 'DM'.fromList [("instanceKey",JSONString instanceKey)]

columnsAttr :: ![String] -> UIAttributes
columnsAttr columns = 'DM'.fromList [("columns",JSONArray (map JSONString columns))]

doubleClickAttr :: !String !String -> UIAttributes
doubleClickAttr taskId actionId = 'DM'.fromList [("doubleClickAction",JSONArray [JSONString taskId,JSONString actionId])]

actionIdAttr :: !String -> UIAttributes
actionIdAttr actionId = 'DM'.fromList [("actionId",JSONString actionId)]

taskIdAttr :: !String -> UIAttributes
taskIdAttr taskId = 'DM'.fromList [("taskId",JSONString taskId)]

editorIdAttr :: !String -> UIAttributes
editorIdAttr taskId = 'DM'.fromList [("editorId",JSONString taskId)]

labelAttr :: !String -> UIAttributes
labelAttr taskId = 'DM'.fromList [(LABEL_ATTRIBUTE,JSONString taskId)]

editAttrs :: !String !String !(Maybe JSONNode) -> UIAttributes
editAttrs taskId editorId mbValue 
	= 'DM'.fromList [("taskId",JSONString taskId),("editorId",JSONString editorId):maybe [] (\value -> [("value",value)]) mbValue]

choiceAttrs :: !String !String ![Int] ![JSONNode] -> UIAttributes
choiceAttrs taskId editorId value options
	= 'DM'.fromList [("taskId",JSONString taskId),("editorId",JSONString editorId),("value",JSONArray (map JSONInt value)),("options",JSONArray options)]

isOptional :: !UI -> Bool
isOptional (UI _ attr _) = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attr)

stringDisplay :: !String -> UI
stringDisplay value = uia UIViewString (valueAttr (JSONString (escapeStr value)))

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
	encodeUI (UI type attr items) = JSONObject (typeField ++ attrFields ++ childrenField)
	where
		typeField     = [("xtype",JSONString (toString type))]
		attrFields    = 'DM'.toList attr
		childrenField = case items of
			[]    = []
			_     = [("children",JSONArray (map encodeUI items))]

instance toString UINodeType
where
	toString UIEmpty           = "itwc_raw_empty"
	toString UIAction          = "itwc_raw_action"
    toString UIForm            = "itwc_raw_form" 
	toString UIFormItem        = "itwc_raw_form_item"
	toString UIPair            = "itwc_raw_pair"
	toString UIRecord          = "itwc_raw_record"
	toString UICons            = "itwc_raw_cons"
	toString UIVarCons         = "itwc_raw_var_cons"
	toString UIInteract        = "itwc_raw_interact"
	toString UIStep            = "itwc_raw_step"
	toString UIParallel        = "itwc_raw_parallel"
	toString UIComponent       = "Component" 
    toString UIContainer       = "itwc_container"
	toString UIPanel           = "itwc_panel"
	toString UITabSet          = "itwc_tabset"
	toString UIWindow          = "itwc_window"
	toString UIMenu            = "itwc_menu"
	toString UIButtonBar       = "itwc_buttonbar"
	toString UIDebug           = "itwc_debug"
	toString UIViewString      = "itwc_view_string"
	toString UIViewHtml        = "itwc_view_html"
	toString UIViewDocument    = "itwc_view_document"
	toString UIViewSlider      = "itwc_view_slider"
	toString UIViewProgress    = "itwc_view_progress"
	toString UIIcon            = "itwc_view_icon"
	toString UITextField       = "TextField"
	toString UITextArea        = "TextArea"
	toString UIPasswordField   = "PasswordField"
	toString UIIntegerField    = "IntegerField"
	toString UIDecimalField    = "DecimalField"
	toString UICheckbox        = "Checkbox"
	toString UIEditSlider      = "itwc_edit_slider"
	toString UIEditDocument    = "itwc_edit_document"
	toString UIEditButton      = "itwc_editbutton"
	toString UIDropdown        = "itwc_choice_dropdown"
	toString UIGrid            = "itwc_choice_grid"
	toString UITree            = "itwc_choice_tree"
	toString UIListChoice      = "itwc_choice_list"
	toString UIRadioGroup      = "itwc_choice_radiogroup"
	toString UICheckboxGroup   = "itwc_choice_checkboxgroup"
	toString UIActionButton    = "itwc_actionbutton"
	toString UILabel           = "itwc_label"
    toString UISplitter        = "itwc_splitter"
    toString UIViewport        = "Viewport"

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

component :: String [JSONNode] -> JSONNode
component xtype opts = JSONObject [("xtype",JSONString xtype):optsfields]
where
	optsfields = flatten [fields \\ JSONObject fields <- opts]


derive class iTask UIChange, UIAttributeChange, UIChildChange

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
encodeUIChange (ChangeUI attributes children)
	= JSONObject
		[("type",JSONString "change")
		,("attributes", JSONArray [JSONObject [("name",JSONString name),("value",value)] \\ SetAttribute name value <- attributes])
		,("children",JSONArray (map encodeChildChange children))
		]
where
	encodeChildChange (i,ChangeChild child) = JSONArray [JSONInt i,JSONString "change",encodeUIChange child]
	encodeChildChange (i,RemoveChild) 		= JSONArray [JSONInt i,JSONString "remove"]
	encodeChildChange (i,InsertChild child) = JSONArray [JSONInt i,JSONString "insert",encodeUI child]

