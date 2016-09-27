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
	genUI dp v vst = (Ok (ui UIComponent,newFieldMask),vst)
	initUI me world
		//Setup UI component
		# world      = ((me .# "domTag") .= toJSVal "pre") world
		//Load Ace javascript
		# (cb,world) = jsWrapFun (\_ w -> (jsNull,initUI` me w)) world
		# world      = addJSFromUrl "/ace.js" (Just cb) world
		= world

	initUI` me world
		//Create Ace editor linked to domEl
		# (domEl,world) = .? (me .# "domEl") world
		# (editor,world) = jsNewObject "ace.edit" [toJSArg domEl] world
		= world

	onEdit dp (tp,e) v m vst = (Ok (NoChange,m),v,vst)
	onRefresh dp r v m vst = (Ok (NoChange,m),v,vst)
