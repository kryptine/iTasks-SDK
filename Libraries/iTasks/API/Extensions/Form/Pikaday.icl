implementation module iTasks.API.Extensions.Form.Pikaday
import iTasks
import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.JS.Interface
import iTasks.UI.Editor.Combinators, iTasks.UI.Editor.Builtin
import iTasks.API.Extensions.DateTime
import qualified Data.Map as DM

PIKADAY_JS_URL :== "/pikaday/pikaday.js"
PIKADAY_CSS_URL :== "/pikaday/css/pikaday.css"
MOMENT_JS_URL :== "/momentjs/moment.min.js"

pikadayField :: Editor String
pikadayField = {Editor|genUI = withClientSideInit initUI genUI, onEdit = onEdit, onRefresh = onRefresh}
where
	genUI dp value vst=:{VSt|taskId,optional}
		//Set both state and options as attributes
		# optionsAttr = 'DM'.fromList
			[("value",JSONString value)
			]
    	# attr = 'DM'.unions [optionsAttr, optionalAttr optional, taskIdAttr taskId, editorIdAttr (editorId dp)]
		= (Ok (uia UITextField attr, FieldMask {touched = False, valid = True, state = JSONString value}),vst)

	initUI me world
		//Load css
		# world      = addCSSFromUrl PIKADAY_CSS_URL world
		//Defer further action till after the field is created...
		# (cb,world) = jsWrapFun (\a w -> (jsNull,initDOMEl me w)) world
		# world      = ((me .# "initDOMEl") .= cb) world
		= world
	
	initDOMEl me world
		//Load javascript library first, then start
		# (cb,world) = jsWrapFun (\_ w -> (jsNull,initDOMEl` me w)) world
		# world      = addJSFromUrl MOMENT_JS_URL Nothing world
		# world      = addJSFromUrl PIKADAY_JS_URL (Just cb) world
		= world

	initDOMEl` me world
		//Create pikaday object
		# (value,world) 	= .? (me .# "attributes.value") world
		# (domEl,world)     = .? (me .# "domEl") world
		# world 		 	= ((domEl .# "value") .= value) world
		//Create onselect
		# (onSelectCb,world) = jsWrapFun (\a w -> (jsNull,onSelect me w)) world
		# (cfg,world)		= jsEmptyObject world
		# world				= ((cfg .# "field") .= domEl) world
		# world				= ((cfg .# "format") .= "YYYY-MM-DD") world
		# world				= ((cfg .# "firstDay") .= 1) world
		# world				= ((cfg .# "onSelect") .= onSelectCb) world
		# (picker,world)	= jsNewObject "Pikaday" [toJSArg cfg] world
		# world				= ((me .# "picker") .= picker) world
		//Handle attribute changes
		# (cb,world) 		= jsWrapFun (\a w -> (jsNull,onAttributeChange picker me a w)) world
		# world      		= ((me .# "onAttributeChange") .= cb) world
		//React to selects
		= world

	onAttributeChange picker me [name,value] world
		# world      = ((me.# "noEvents") .= True ) world
		# (_,world)  = ((picker .# "setDate") .$ value) world
		# world      = ((me.# "noEvents") .= False) world
		= world

	onSelect me world
		# (noEvents,world)  = .? (me .# "noEvents") world
		| (not (jsIsUndefined noEvents)) && jsValToBool noEvents
			= world
		# (picker,world)    = .? (me .# "picker") world
		# (value,world)     = ((picker .# "toString") .$ "YYYY-MM-DD" ) world
		# (taskId,world)  = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
		# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,value)) world
		= world


	onEdit dp (tp,e) val mask vst=:{VSt|optional}
		= case e of
			JSONNull = (Ok (ChangeUI [SetAttribute "value" JSONNull] [],FieldMask {touched=True,valid=optional,state=JSONNull}),val,vst)
			json = case fromJSON e of
				Nothing  = (Ok (NoChange,FieldMask {touched=True,valid=False,state=e}),val,vst)
				Just val = (Ok (ChangeUI [SetAttribute "value" (JSONString val)] [],FieldMask {touched=True,valid=True,state=JSONString val}),val,vst)

	onRefresh dp new old mask vst=:{VSt|mode,optional}
		| old === new = (Ok (NoChange,mask),new,vst)
		| otherwise   = (Ok (ChangeUI [SetAttribute "value" (JSONString new)] [],mask),new,vst)

pikadayDateField :: Editor Date
pikadayDateField = whenDisabled
	(liftEditor toString fromString (textView 'DM'.newMap))
   	(liftEditorAsymmetric toString parseDate (withHintAttributes "date (yyyy-mm-dd)" pikadayField))
