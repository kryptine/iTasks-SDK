implementation module iTasks.UI.Editor.Builtin

import iTasks.UI.Definition, iTasks.UI.Editor
import StdFunc, StdBool, GenEq
import Data.Error, Text.JSON, Text.HTML
import qualified Data.Map as DM

textField :: Editor String
textField = fieldComponent toJSON UITextField

integerField :: Editor Int
integerField = fieldComponent toJSON UIIntegerField

decimalField :: Editor Real
decimalField = fieldComponent toJSON UIDecimalField

documentField :: Editor Document
documentField = fieldComponent toJSON UIDocumentField

passwordField :: Editor String
passwordField = fieldComponent toJSON UIPasswordField

textArea :: Editor String
textArea = fieldComponent toJSON UITextArea

checkBox :: Editor Bool
checkBox = fieldComponent toJSON UICheckbox

slider :: Editor Int
slider = fieldComponent toJSON UISlider

label :: Editor String
label = fieldComponent toJSON UILabel

icon :: Editor String
icon = fieldComponent toJSON UIIcon

dropdown :: Bool -> Editor ([String], [Int])
dropdown multi = choiceComponent (const 'DM'.newMap) id JSONString (\o i -> i >= 0 && i < length o) UIDropdown multi

checkGroup :: Bool -> Editor ([String],[Int])
checkGroup multi = choiceComponent (const 'DM'.newMap) id JSONString (\o i -> i >= 0 && i < length o) UIRadioGroup multi

choiceList :: Bool -> Editor ([String],[Int])
choiceList multi = choiceComponent (const 'DM'.newMap) id JSONString (\o i -> i >= 0 && i < length o) UIChoiceList multi

grid :: Bool -> Editor (ChoiceGrid, [Int])
grid multi = choiceComponent (\{ChoiceGrid|header} -> columnsAttr header) (\{ChoiceGrid|rows} -> rows) toOption (\o i -> i >= 0 && i < length o) UIGrid multi
where
	toOption opt = JSONArray (map (JSONString o toString) opt)

tree :: Bool -> Editor ([ChoiceNode], [Int])
tree multi = choiceComponent (const 'DM'.newMap) id toOption checkBounds UITree multi
where
	toOption {ChoiceNode|id,label,icon,expanded,children}
		= JSONObject [("text",JSONString label)
					 ,("iconCls",maybe JSONNull (\i -> JSONString ("icon-"+++i)) icon)
					 ,("value",JSONInt id)
					 ,("expanded",JSONBool expanded)
					 ,("leaf",JSONBool (isEmpty children))
					 ,("children",JSONArray (map toOption children))
					]

	checkBounds options idx 
		= or (map (checkNode idx) options)
	checkNode idx {ChoiceNode|id,children}
		| idx == id = True
		| otherwise = or (map (checkNode idx) children)

progressBar  :: Editor Int
progressBar = integerField

textView :: Editor String
textView = fieldComponent toJSON UITextView

htmlView :: Editor HtmlTag
htmlView = fieldComponent (JSONString o toString) UIHtmlView

//Field like components for which simply knowing the UI type is sufficient
fieldComponent toValue type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where 
	genUI dp val vst=:{VSt|taskId,mode,optional}
		# val = if (mode =: Enter) JSONNull (toValue val) 
		# valid = if (mode =: Enter) optional True //When entering data a value is initially only valid if it is optional
		# mask = FieldMask {touched = False, valid = valid, state = val}
		# attr = 'DM'.unions [optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp), valueAttr val]
		= (Ok (uia type attr,mask),vst)

	onEdit dp (tp,e) val mask vst=:{VSt|optional}
		= case e of
			JSONNull = (Ok (ChangeUI [SetAttribute "value" JSONNull] [],FieldMask {touched=True,valid=optional,state=JSONNull}),val,vst)
			json = case fromJSON e of
				Nothing  = (Ok (NoChange,FieldMask {touched=True,valid=False,state=e}),val,vst)
				Just val = (Ok (ChangeUI [SetAttribute "value" (toValue val)] [],FieldMask {touched=True,valid=True,state=toValue val}),val,vst)

	onRefresh dp new old mask vst=:{VSt|mode,optional}
		| old === new = (Ok (NoChange,mask),new,vst)
		| otherwise   = (Ok (ChangeUI [SetAttribute "value" (toValue new)] [],mask),new,vst)

//Choice components that have a set of options
choiceComponent attr getOptions toOption checkBounds type multi = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (val,sel) vst=:{VSt|taskId,mode,optional}
		# valid = if (mode =: Enter) optional True //When entering data a value is initially only valid if it is optional
		# mask = FieldMask {touched = False, valid = valid, state = JSONNull}
		# attr = 'DM'.unions [attr val,choiceAttrs taskId (editorId dp) sel (map toOption (getOptions val)),multipleAttr multi]
		= (Ok (uia type attr,mask), vst)

	onEdit dp (tp,e) (val,sel) mask vst=:{VSt|optional}
		# options = getOptions val
		= case e of
			JSONNull
				= (Ok (NoChange,FieldMask {touched=True,valid=optional,state=JSONNull}),(val,[]),vst)
			(JSONArray indices)
				# selection = [i \\ JSONInt i <- indices]
				| all (checkBounds options) selection
					= (Ok (NoChange,FieldMask {touched=True,valid=True,state=JSONArray indices}),(val,selection),vst)
				| otherwise
					= (Error ("Choice event out of bounds: " +++ toString (JSONArray indices)),(val,sel),vst)
			(JSONInt idx)
				| checkBounds options idx
					= (Ok (NoChange,FieldMask {touched=True,valid=True,state=JSONInt idx}),(val,[idx]),vst)
				| otherwise
					= (Error ("Choice event out of bounds: " +++ toString idx),(val,sel),vst)
			_ 
				= (Error ("Invalid choice event: " +++ toString e), (val,sel),vst)

	onRefresh dp new old mask vst
		= (Ok (NoChange,mask),new,vst)
