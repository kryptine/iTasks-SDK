implementation module iTasks.UI.Editor.Builtin

import iTasks.UI.Definition, iTasks.UI.Editor
import qualified Data.Map as DM

import StdMisc

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
	genUI dp val mask vst=:{VSt|taskId,optional}
		= (Ok (uia type ( 'DM'.unions [optionalAttr optional,editAttrs taskId (editorId dp) (checkMaskValue mask val)])),vst)

	updUI dp ov om nv nm vst=:{VSt|optional,disabled}
		| checkMaskValue om ov === checkMaskValue nm nv = (Ok NoChange,vst)
		| otherwise =  (Ok (ChangeUI [SetAttribute "value" (toJSON nv)] []),vst)

	onEdit dp e val mask ust
		= case e of
			JSONNull = (val,Blanked,ust)
			json = case fromJSON e of
				Nothing  = (val,TouchedUnparsed e,ust)
				Just val = (val,Touched,ust)
