implementation module iTasks.UI.Editor.Builtin

import iTasks.UI.Definition, iTasks.UI.Editor
import StdFunc, GenEq
import Data.Error, Text.JSON, Text.HTML
import qualified Data.Map as DM

textField :: Editor String
textField = simpleComponent toJSON UITextField

integerField :: Editor Int
integerField = simpleComponent toJSON UIIntegerField

decimalField :: Editor Real
decimalField = simpleComponent toJSON UIDecimalField

passwordField :: Editor String
passwordField = simpleComponent toJSON UIPasswordField

textArea :: Editor String
textArea = simpleComponent toJSON UITextArea

checkBox :: Editor Bool
checkBox = simpleComponent toJSON UICheckbox

slider :: Editor Int
slider = integerField

dropdownBox :: Editor String
dropdownBox = textField

progressBar  :: Editor Int
progressBar = integerField

textView :: Editor String
textView = simpleComponent toJSON UIViewString

htmlView :: Editor HtmlTag
htmlView = simpleComponent (JSONString o toString) UIViewHtml

icon :: Editor String
icon = simpleComponent toJSON UIIcon

//Simple components for which simply knowing the UI type is sufficient
simpleComponent toValue type = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where 
	genUI dp val vst=:{VSt|taskId,mode,optional}
		# mask = newFieldMask
		# val = if (mode =: Enter) JSONNull (toValue val) 
		# attr = 'DM'.unions [optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp), valueAttr val]
		= (Ok (uia type attr,mask),vst)

	onEdit dp e val mask vst=:{VSt|optional}
		= case e of
			JSONNull = (Ok (ChangeUI [SetAttribute "value" JSONNull] [],FieldMask {touched=True,valid=optional,state=JSONNull}),val,vst)
			json = case fromJSON e of
				Nothing  = (Ok (NoChange,FieldMask {touched=True,valid=False,state=e}),val,vst)
				Just val = (Ok (ChangeUI [SetAttribute "value" (toValue val)] [],FieldMask {touched=True,valid=True,state=e}),val,vst)

	onRefresh dp new old mask vst=:{VSt|mode,optional}
		| old === new = (Ok (NoChange,mask),new,vst)
		| otherwise   = (Ok (ChangeUI [SetAttribute "value" (toValue new)] [],mask),new,vst)

