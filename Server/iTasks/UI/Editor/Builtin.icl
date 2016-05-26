implementation module iTasks.UI.Editor.Builtin

import iTasks.UI.Definition, iTasks.UI.Editor
import qualified Data.Map as DM

textField :: Editor String
textField = simpleComponent UIEditString

integerField :: Editor Int
integerField = simpleComponent UIEditInt

decimalField :: Editor Real
decimalField = simpleComponent UIEditDecimal

checkBox :: Editor Bool
checkBox = simpleComponent UIEditCheckbox

textView :: Editor String
textView = simpleComponent UIViewString

//Simple components for which simply knowing the UI type is sufficient
simpleComponent type = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where 
	genUI dp val upd vst=:{VSt|taskId,optional}
		# mask = newFieldMask
		= (Ok (uia type ( 'DM'.unions [optionalAttr optional,editAttrs taskId (editorId dp) (checkMaskValue mask val)]),mask),vst)

	updUI dp ov om nv nm vst=:{VSt|optional,disabled}
		| checkMaskValue om ov === checkMaskValue nm nv = (Ok NoChange,vst)
		| otherwise =  (Ok (ChangeUI [SetAttribute "value" (toJSON nv)] []),vst)

	onEdit dp e val mask ust=:{USt|optional}
		= case e of
			JSONNull = (val,FieldMask {touched=True,valid=optional,state=JSONNull},ust)
			json = case fromJSON e of
				Nothing  = (val,FieldMask {touched=True,valid=False,state=e},ust)
				Just val = (val,FieldMask {touched=True,valid=True,state=e},ust)

