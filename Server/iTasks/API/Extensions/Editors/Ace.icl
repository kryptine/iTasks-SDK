implementation module iTasks.API.Extensions.Editors.Ace

import iTasks
import iTasks.UI.Editor, iTasks.UI.Definition 
import iTasks.UI.JS.Interface
import qualified Data.Map as DM

aceTextArea :: Editor String
aceTextArea = aceEditor

aceEditor :: Editor String
aceEditor = fromEditlet {Editlet|genUI = genUI,initUI = initUI, onEdit = onEdit, onRefresh = onRefresh}
where
	genUI dp v vst=:{VSt|taskId,optional}
    	# attr = 'DM'.unions [optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp), valueAttr (JSONString v)]
		= (Ok (uia UIComponent attr, newFieldMask),vst)

	initUI me world
		//Setup UI component
		# world      = ((me .# "domTag") .= toJSVal "pre") world
		# (cb,world) = jsWrapFun (\a w -> (jsNull,onAttributeChange me a w)) world
		# world      = ((me .# "onAttributeChange") .= cb) world
		//Load Ace javascript
		# (cb,world) = jsWrapFun (\_ w -> (jsNull,initUI` me w)) world
		# world      = addJSFromUrl "/ace.js" (Just cb) world
		= world

	initUI` me world
		//Create Ace editor linked to domEl
		# (domEl,world)  = .? (me .# "domEl") world
		# (editor,world) = jsNewObject "ace.edit" [toJSArg domEl] world
		# world          = ((me .# "editor") .= editor) world
		//Set initial value
		# (value,world)  = .? (me .# "value") world
        # (_,world)      = ((editor .# "setValue") .$ value) world
		//Add event listener
		# (cb,world)     = jsWrapFun (\a w -> (jsNull,onChange editor me w)) world
		# (_,world)      = ((editor .# "on") .$ ("change",cb)) world
		= world

	onAttributeChange me args world
		= jsTrace "Ace: change event from server" world

	onChange editor me world
        # (value,world)  = ((editor .# "getValue") .$ ()) world
		# (taskId,world)  = .? (me .# "taskId") world
		# (editorId,world)  = .? (me .# "editorId") world
		# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,value)) world
		= world

	onEdit dp ([],JSONString nv) v m vst = (Ok (NoChange,m),nv,vst)
	onEdit dp (tp,e) v m vst = (Ok (NoChange,m),v,vst)

	onRefresh dp r v m vst = (Ok (NoChange,m),v,vst)
	onRefresh dp r v m vst 
		| r == v = (Ok (NoChange,m),v,vst)
				 = (Ok (ChangeUI [SetAttribute "value" (JSONString r)] [] ,m),r,vst)
