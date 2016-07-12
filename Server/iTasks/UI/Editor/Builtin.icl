implementation module iTasks.UI.Editor.Builtin

import iTasks.UI.Definition, iTasks.UI.Editor
import StdFunc, StdBool, GenEq
import Data.Error, Text.JSON, Text.HTML
import qualified Data.Map as DM

textField :: UIAttributes -> Editor String
textField attr = fieldComponent attr toJSON UITextField

textArea :: UIAttributes -> Editor String
textArea attr = fieldComponent attr toJSON UITextArea

passwordField :: UIAttributes -> Editor String
passwordField attr = fieldComponent attr toJSON UIPasswordField

integerField :: UIAttributes -> Editor Int
integerField attr = fieldComponent attr toJSON UIIntegerField

decimalField :: UIAttributes -> Editor Real
decimalField attr = fieldComponent attr toJSON UIDecimalField

documentField :: UIAttributes -> Editor (!String,!String,!String,!String,!Int)
documentField attr = fieldComponent attr toJSON UIDocumentField

checkBox :: UIAttributes -> Editor Bool
checkBox attr = fieldComponent attr toJSON UICheckbox

slider :: UIAttributes -> Editor Int
slider attr = fieldComponent attr toJSON UISlider

button :: UIAttributes -> Editor Bool
button attr = fieldComponent attr toJSON UIButton

label :: UIAttributes -> Editor String
label attr = viewComponent (\text ->  'DM'.union attr (textAttr text)) UILabel

icon :: UIAttributes -> Editor (!String,!Maybe String)
icon attr = viewComponent (\(iconCls,tooltip) -> 'DM'.unions [iconClsAttr iconCls,maybe 'DM'.newMap tooltipAttr tooltip,attr]) UIIcon

textView :: UIAttributes -> Editor String
textView attr = viewComponent (\text -> 'DM'.fromList [("value",JSONString text)]) UITextView

htmlView :: UIAttributes -> Editor HtmlTag
htmlView attr = viewComponent (\html -> 'DM'.union (valueAttr (JSONString (toString html))) attr) UIHtmlView

progressBar :: UIAttributes -> Editor (Maybe Int, Maybe String)
progressBar attr = viewComponent combine UIProgressBar
where
	combine (amount,text) = 'DM'.unions ((maybe [] (\t -> [textAttr t]) text) ++ (maybe [] (\v -> [valueAttr (JSONInt v)]) amount) ++ [attr])
						
dropdown :: UIAttributes -> Editor ([String], [Int])
dropdown attr = choiceComponent (const attr) id JSONString (\o i -> i >= 0 && i < length o) UIDropdown

checkGroup :: UIAttributes -> Editor ([String],[Int])
checkGroup attr = choiceComponent (const attr) id JSONString (\o i -> i >= 0 && i < length o) UICheckGroup

choiceList :: UIAttributes -> Editor ([String],[Int])
choiceList attr = choiceComponent (const attr) id JSONString (\o i -> i >= 0 && i < length o) UIChoiceList

grid :: UIAttributes -> Editor (ChoiceGrid, [Int])
grid attr = choiceComponent (\{ChoiceGrid|header} -> 'DM'.union attr (columnsAttr header)) (\{ChoiceGrid|rows} -> rows) toOption (\o i -> i >= 0 && i < length o) UIGrid
where
	toOption opt = JSONArray (map (JSONString o toString) opt)

tree :: UIAttributes -> Editor ([ChoiceNode], [Int])
tree attr = choiceComponent (const attr) id toOption checkBounds UITree
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

//Field like components for which simply knowing the UI type is sufficient
fieldComponent attr toValue type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where 
	genUI dp val vst=:{VSt|taskId,mode,optional}
		# val = if (mode =: Enter) JSONNull (toValue val) 
		# valid = if (mode =: Enter) optional True //When entering data a value is initially only valid if it is optional
		# mask = FieldMask {touched = False, valid = valid, state = val}
		# attr = 'DM'.unions [attr,optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp), valueAttr val]
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

//Components which cannot be edited 
viewComponent toAttributes type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst
		= (Ok (uia type (toAttributes val), FieldMask {touched = False, valid = True, state = JSONNull}),vst)

	onEdit dp (tp,e) val mask vst
		= (Error "Edit event for view component",val,vst)

	onRefresh dp new old mask vst
		= case [SetAttribute nk nv \\ ((ok,ov),(nk,nv)) <- zip ('DM'.toList (toAttributes old),'DM'.toList (toAttributes new)) | ok == nk && ov =!= nv] of
			[] 		= (Ok (NoChange,mask),new,vst)
			changes = (Ok (ChangeUI changes [],mask),new,vst)

//Choice components that have a set of options
choiceComponent attr getOptions toOption checkBounds type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (val,sel) vst=:{VSt|taskId,mode,optional}
		# valid = if (mode =: Enter) optional True //When entering data a value is initially only valid if it is optional
		# mask = FieldMask {touched = False, valid = valid, state = JSONNull}
		# attr = 'DM'.unions [attr val,choiceAttrs taskId (editorId dp) sel (map toOption (getOptions val))]
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
