implementation module iTasks.Framework.GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdGeneric, StdEnum, StdFunc, List, Generic, JSON
import Maybe, Functor, Text, HTML, Map
import iTasks.Framework.GenUpdate, iTasks.Framework.GenVerify, iTasks.Framework.UIDefinition, iTasks.Framework.Util, iTasks.Framework.HtmlUtil
import iTasks.API.Core.SystemTypes, iTasks.API.Core.LayoutCombinators

visualizeAsText :: !StaticVisualizationMode !a -> String | gVisualizeText{|*|} a
visualizeAsText mode v = concat (gVisualizeText{|*|} mode v)

visualizeAsEditor :: !a !VerifyMask !TaskId !Layout !*IWorld -> (![(!UIControl,!UIAttributes)],!*IWorld) | gVisualizeEditor{|*|} a
visualizeAsEditor v vmask taskId layout iworld
	# vst		= {VSt|mkVSt taskId iworld & verifyMask = [vmask], currentPath = [], layout = layout}
	# (res,vst)	= gVisualizeEditor{|*|} (Just v) vst
	= (controlsOf res,kmVSt vst)
	
//Generic text visualizer
generic gVisualizeText a :: !StaticVisualizationMode !a -> [String]

gVisualizeText{|UNIT|} _ _ = []

gVisualizeText{|RECORD|} fx mode (RECORD x)
	# viz = fx mode x
	= case mode of
		AsLabel			= take 1 viz
		AsDisplay		= viz

gVisualizeText{|FIELD of {gfd_name}|} fx mode (FIELD x)
	# viz = fx mode x
	= case mode of
		AsDisplay	= [camelCaseToWords gfd_name, ": ": viz] ++ [" "]
		AsLabel	= viz

gVisualizeText{|OBJECT|} fx mode (OBJECT x) = fx mode x

gVisualizeText{|CONS of {gcd_name,gcd_type_def}|} fx mode (CONS x)
	= normalADTStaticViz (fx mode x)
where
	normalADTStaticViz viz
		//If viz is empty, only show constructor name
		| isEmpty viz
			= [gcd_name]
		//If there are multiple constructors, also show the name of the constructor
		| gcd_type_def.gtd_num_conses > 1
			= intersperse " " [gcd_name:viz]
		//Otherwise show visualisation of fields separated by spaces
		| otherwise
			= intersperse " " viz

gVisualizeText{|PAIR|} fx fy mode (PAIR x y) = fx mode x ++ fy mode y

gVisualizeText{|EITHER|} fx fy mode either = case either of
	LEFT x	= fx mode x
	RIGHT y	= fy mode y

gVisualizeText{|Int|}			_ val				= [toString val]
gVisualizeText{|Real|}			_ val				= [toString val]
gVisualizeText{|Char|}			_ val				= [toString val]
gVisualizeText{|String|}		_ val				= [toString val]
gVisualizeText{|Bool|}			_ val				= [toString val]

gVisualizeText {|[]|} fx  mode val					= ["[":  flatten (intersperse [", "] [fx mode x \\ x <- val])] ++ ["]"]
gVisualizeText{|Maybe|} fx mode val					= fromMaybe ["-"] (fmap (\v -> fx mode v) val)

gVisualizeText{|Void|} _ _					= []
gVisualizeText{|Dynamic|} _ _				= []
gVisualizeText{|(->)|} _ _ _ _				= []
gVisualizeText{|JSONNode|} _ val			= [toString val]
gVisualizeText{|HtmlTag|} _ html			= [toString html]

derive gVisualizeText Either, (,), (,,), (,,,), Timestamp, Map

mkVSt :: !TaskId *IWorld -> *VSt
mkVSt taskId iworld
	= {VSt| currentPath = [], selectedConsIndex = -1, optional = False, disabled = False, verifyMask = []
	  , taskId = taskId, layout = autoLayout, iworld = iworld}

kmVSt :: !*VSt -> *IWorld //inverse of mkVSt
kmVSt {VSt|iworld} = iworld

//Generic visualizer
generic gVisualizeEditor a | gVisualizeText a, gHeaders a, gGridRows a :: !(Maybe a)!*VSt -> (!VisualizationResult,!*VSt)

gVisualizeEditor{|UNIT|} _ vst
	= (NormalEditor [],vst)

gVisualizeEditor{|RECORD|} fx _ _ _ val vst=:{VSt|currentPath,verifyMask,optional,disabled,taskId}
	# (cmv,vm)	= popMask verifyMask
	//When optional and no value yet, just show the checkbox
	| optional && isNothing val && not disabled
		= (OptionalEditor [checkbox False], {VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
	# (fieldViz,vst) = fx (fmap fromRECORD val) {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv, optional = False}
	//For optional records we add the checkbox to clear the entire record
	# viz = if (optional && not disabled) (OptionalEditor [checkbox True:controlsOf fieldViz]) fieldViz	
	= (viz,{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
where
	checkbox checked = (UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId = toString taskId, editorId = dp2s currentPath, value = Just checked},newMap)
	
gVisualizeEditor{|FIELD of {gfd_name}|} fx _ _ _ val vst=:{VSt|disabled,layout}
	# (vizBody,vst)		= fx (fmap fromFIELD val) vst
	= case vizBody of
		HiddenEditor			= (HiddenEditor,vst)
		NormalEditor controls
			# controls = layout.Layout.editor {UIControlSequence|attributes = addLabel disabled gfd_name newMap, controls = controls, direction = Vertical}
			= (NormalEditor controls,vst)
		OptionalEditor controls	
			# controls = layout.Layout.editor {UIControlSequence|attributes = addLabel True gfd_name newMap, controls = controls, direction = Vertical}
			= (OptionalEditor controls, vst)


gVisualizeEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} fx _ _ _ val vst=:{currentPath,selectedConsIndex = oldSelectedConsIndex,disabled,verifyMask,taskId,layout}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	# (cmv,vm)	= popMask verifyMask
	# x			= fmap fromOBJECT val
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		# (items, vst=:{selectedConsIndex}) = fx x vst
		# content	= layout.editor {UIControlSequence|attributes = newMap, controls = (if (isTouched cmv) (controlsOf items) []), direction = Horizontal}
		= (NormalEditor [(UIDropdown defaultSizeOpts
								{UIChoiceOpts
								| taskId = toString taskId
								, editorId = dp2s currentPath
								, value = if (isTouched cmv) [selectedConsIndex] []
								, options = [gdc.gcd_name \\ gdc <- gtd_conses]}
							,addVerAttributes (verifyElementStr cmv) newMap)
						: content
						]
		  ,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})
	//ADT with one constructor or static render: put content into container, if empty show cons name
	| otherwise
		# (vis,vst) = fx x vst
		# vis = case vis of
			HiddenEditor 	= HiddenEditor
			NormalEditor []
				= if (isTouched cmv || disabled) (NormalEditor [((stringDisplay ((gtd_conses !! vst.selectedConsIndex).gcd_name)),newMap)]) (NormalEditor [])			
			NormalEditor items
				= NormalEditor (layout.editor {UIControlSequence|attributes = newMap, controls = items, direction = Horizontal})
			OptionalEditor items
				= OptionalEditor (layout.editor {UIControlSequence|attributes = newMap, controls = items, direction = Horizontal})
		= (vis,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})
where
	addSpacing [] = []
	addSpacing [d:ds] = [d:map (setMargins 0 0 0 5) ds]

gVisualizeEditor{|CONS of {gcd_index}|} fx _ _ _ val vst = visualizeCustom mkControl vst
where
	mkControl name _ _ vst=:{VSt|taskId,currentPath,optional,disabled}
		# x = fmap fromCONS val
		# (viz,vst)	= fx x vst
		= (controlsOf viz, {VSt | vst & selectedConsIndex = gcd_index})

gVisualizeEditor{|PAIR|} fx _ _ _ fy _ _ _ val vst
	# (x,y)			= (fmap fromPAIRX val, fmap fromPAIRY val)
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	= case (vizx,vizy) of	//Define combination for all nine combinations of normal/optional/hidden editors
		(NormalEditor ex,	NormalEditor ey)		= (NormalEditor (ex ++ ey), vst)
		(NormalEditor ex,	OptionalEditor ey)		= (NormalEditor (ex ++ ey), vst)
		(NormalEditor ex,	HiddenEditor)			= (NormalEditor ex, vst)
		(OptionalEditor ex,	NormalEditor ey)		= (NormalEditor (ex ++ ey), vst)
		(OptionalEditor ex,	OptionalEditor ey)		= (OptionalEditor (ex ++ ey), vst)
		(OptionalEditor ex, HiddenEditor)			= (OptionalEditor ex, vst)
		(HiddenEditor,		NormalEditor ey)		= (NormalEditor ey, vst)
		(HiddenEditor,		OptionalEditor ey)		= (OptionalEditor ey, vst)
		(HiddenEditor,		HiddenEditor)			= (HiddenEditor, vst)
		
		
gVisualizeEditor{|EITHER|} fx _ _ _ fy _ _ _ val vst = case val of
		Nothing			= fx Nothing vst
		Just (LEFT x)	= fx (Just x) vst
		Just (RIGHT y)	= fy (Just y) vst



gVisualizeEditor{|Int|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditInt defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)
	
gVisualizeEditor{|Real|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|Char|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap toString val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|String|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gVisualizeEditor{|Bool|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled		= ([(UIViewCheckbox defaultSizeOpts {UIViewOpts|value = val},addVerAttributes verRes newMap)],vst)
		| otherwise		= ([(UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)


gVisualizeEditor{|[]|} fx _ _ _ val vst=:{VSt|taskId,currentPath,disabled,verifyMask,layout}
	# (cmv,vm)		= popMask verifyMask
	# vst			= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# ver			= verifyElementStr cmv
	# val			= fromMaybe [] val
	# (items,vst)	= listControl val vst
	= (NormalEditor [(listContainer items,addVerAttributes ver newMap)],{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
where
	name = dp2s currentPath
	listControl items vst=:{VSt|optional,disabled}
		# (itemsVis,vst)	= childVisualizations fx items vst
		# numItems = length items
		| disabled
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
		| otherwise
			//# (newItem,vst)		= newChildVisualization fx True vst
			//= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [newItemControl newItem],vst)
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
						
	listItemControl disabled numItems idx item 
		# controls	= map fst (layout.editor {UIControlSequence| attributes = newMap, controls = controlsOf item, direction = Vertical})
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Just (JSONString ("mup_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=idx == 0}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Just (JSONString ("mdn_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= idx == numItems - 1}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Just (JSONString ("rem_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=False}
					  ]
		= setHeight WrapSize (setDirection Horizontal (defaultContainer (if disabled controls (controls ++ buttons))))
/*
	newItemControl item
		# controls	= map fst (layout.editor (newMap,controlsOf item))
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=True}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= True}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=True}
					  ]
		= setDirection Horizontal (defaultContainer (controls ++ buttons))
*/	
	addItemControl numItems
		# controls	= [UIViewString /*{*/defaultSizeOpts /* & width=Just FlexSize }*/ {UIViewOpts|value= Just (numItemsText numItems)}]
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=Just (JSONString "add")} {UIButtonOpts|text=Nothing,iconCls=Just "icon-add",disabled=False}]
		= setHeight WrapSize (setDirection Horizontal (defaultContainer (controls ++ buttons)))
	
	listContainer items
		= setHeight WrapSize (defaultContainer items)
	
	numItemsText 1 = "1 item"
	numItemsText n = toString n +++ " items"
	
gVisualizeEditor{|(,)|} fx _ _ _ fy _ _ _ val vst=:{VSt|currentPath,verifyMask}
	# (x,y)			= (fmap fst val, fmap snd val)
	# (cmv,vm)		= popMask verifyMask
	# vst			= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	# viz = case (vizx,vizy) of
		(HiddenEditor,HiddenEditor) = HiddenEditor
		_	= NormalEditor (controlsOf vizx ++ controlsOf vizy)
				 
	= (viz, {VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gVisualizeEditor{|(->)|} _ _ _ _ _ _ _ _	_ vst	= noVisualization vst
		
gVisualizeEditor{|Dynamic|}					_ vst	= noVisualization vst

gVisualizeEditor{|Maybe|} fx _ _ _ val vst=:{VSt|currentPath,optional,disabled}
	| disabled && noValue val
		= (OptionalEditor [], {VSt|vst & currentPath = stepDataPath currentPath})
	# (viz,vst) = case val of
		Just (Just x)	= fx (Just x) {VSt|vst & optional = True}
		_				= fx Nothing {VSt|vst & optional = True}
	= (toOptional viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
where
	toOptional	(NormalEditor ex)	= OptionalEditor ex
	toOptional	viz					= viz
	
	noValue (Just Nothing)			= True	
	noValue Nothing					= True
	noValue _						= False
		
gVisualizeEditor{|Void|} _ vst = noVisualization vst
gVisualizeEditor{|HtmlTag|}	val vst = visualizeCustom toControl vst
where
	toControl name touched _ vst
		= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = val},newMap)], vst)

derive gVisualizeEditor JSONNode, Either, (,,), (,,,), Timestamp, Map //TODO Make specializations for (,,) and (,,,)

generic gHeaders a :: a -> [String]

gHeaders{|UNIT|} _			= []
gHeaders{|PAIR|} _ _ _		= []
gHeaders{|EITHER|} _ _ _	= []
gHeaders{|OBJECT|} _ _		= [""]
gHeaders{|CONS|} _ _		= []
gHeaders{|RECORD of {grd_fields}|} _ _	= [camelCaseToWords fieldname \\ fieldname <- grd_fields]
gHeaders{|FIELD|} _ _		= []
gHeaders{|Int|}	_			= [""]
gHeaders{|Char|} _			= [""]
gHeaders{|String|} _		= [""]
gHeaders{|Real|} _			= [""]
gHeaders{|Bool|} _ 			= [""]
gHeaders{|Dynamic|}	_		= [""]
gHeaders{|HtmlTag|}	_		= [""]
gHeaders{|(->)|} _ _ _		= [""]

derive gHeaders [], (,), (,,), (,,,), Maybe, Either, Void, Map, JSONNode, Timestamp

generic gGridRows a | gVisualizeText a :: !a ![String] -> Maybe [String]

gGridRows{|OBJECT|} _ _ _ _					= Nothing
gGridRows{|CONS|} _ _ _ acc					= Nothing
gGridRows{|PAIR|} fx _ fy _ (PAIR x y) acc	= fy y (fromMaybe [] (fx x acc))
gGridRows{|RECORD|} fx _ (RECORD r) acc		= fmap reverse (fx r acc) 
gGridRows{|FIELD|} _ gx (FIELD f) acc		= Just [concat (gx AsLabel f):acc]
gGridRows{|EITHER|} _ _ _ _	_ _				= abort "gGridRows: EITHER should not occur"
gGridRows{|UNIT|} _ _						= abort "gGridRows: UNIT should not occur"

gGridRows{|Int|} i _						= Nothing
gGridRows{|Char|} c _						= Nothing
gGridRows{|String|} s _						= Nothing
gGridRows{|Real|} r _						= Nothing
gGridRows{|Bool|} b _						= Nothing
gGridRows{|Dynamic|} d _					= Nothing
gGridRows{|HtmlTag|} h _					= Nothing
gGridRows{|(->)|} _ gx _ gy f _				= Nothing

derive gGridRows [], (,), (,,), (,,,), Maybe, Either, Void, Map, JSONNode, Timestamp

//***** UTILITY FUNCTIONS *************************************************************************************************	
		
visualizeCustom :: !UIVizFunction !*VSt -> *(!VisualizationResult,!*VSt)
visualizeCustom tuiF vst=:{VSt|currentPath,disabled,verifyMask}
	// only check mask if generating editor definition & not for labels
	# (cmv,vm)	= popMask verifyMask
	# touched	= isTouched cmv
	# vst		= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# ver		= verifyElementStr cmv
	# (vis,vst) = tuiF (dp2s currentPath) touched ver vst
	= (NormalEditor vis,{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
noVisualization :: !*VSt -> *(!VisualizationResult,!*VSt)
noVisualization vst=:{VSt|currentPath,verifyMask}
	# (_,vm)	= popMask verifyMask
	= (NormalEditor [], {VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
childVisualizations :: !((Maybe a) -> .(*VSt -> *(!VisualizationResult,*VSt))) ![a] !*VSt -> *(![VisualizationResult],!*VSt)
childVisualizations fx children vst = childVisualizations` children [] vst
where
	childVisualizations` [] acc vst
		= (reverse acc,vst)
	childVisualizations` [child:children] acc vst
		# (childV,vst) = fx (Just child) vst
		= childVisualizations` children [childV:acc] vst

newChildVisualization :: !((Maybe a) -> .(*VSt -> *(VisualizationResult,*VSt))) !Bool !*VSt -> *(!VisualizationResult,!*VSt)
newChildVisualization fx newOptional vst=:{VSt|optional}
	# (childV,vst) = fx Nothing {VSt|vst & optional = newOptional}
	= (childV,{VSt|vst & optional = optional})

eventValue :: !DataPath !(Maybe (!String,!JSONNode)) -> Maybe JSONNode
eventValue currentPath mbEvent = case mbEvent of
	Just (dp,val) | dp == dp2s currentPath	= Just val
	_										= Nothing

verifyElementStr :: !VerifyMask -> VerifyResult
verifyElementStr cmv = case cmv of
	VMValid mbHnt _							= maybe NoMsg ValidMsg mbHnt
	VMValidWithState mbHnt _ _				= maybe NoMsg ValidMsg mbHnt
	VMUntouched mbHnt _ _					= maybe NoMsg HintMsg mbHnt
	VMInvalid (FormatError e) _				= ErrorMsg e
	VMInvalid BlankError _					= ErrorMsg "This value is required"
	VMInvalidWithState (FormatError e) _ _	= ErrorMsg e
	VMInvalidWithState BlankError _ _		= ErrorMsg "This value is required"

addVerAttributes :: !VerifyResult !UIAttributes -> UIAttributes
addVerAttributes (HintMsg msg)	attr = put HINT_ATTRIBUTE msg attr
addVerAttributes (ValidMsg msg)	attr = put VALID_ATTRIBUTE msg attr
addVerAttributes (ErrorMsg msg) attr = put ERROR_ATTRIBUTE msg attr
addVerAttributes _				attr = attr

addLabel :: !Bool !String !UIAttributes -> UIAttributes
addLabel optional label attr = put LABEL_ATTRIBUTE (format optional label) attr
where
	format optional label = camelCaseToWords label +++ if optional "" "*" +++ ":" //TODO: Move to layout

checkMask :: !Bool !(Maybe a) -> (Maybe a)
checkMask False _	= Nothing
checkMask _ val 	= val

controlsOf :: !VisualizationResult -> [(UIControl,UIAttributes)]
controlsOf (NormalEditor controls)		= controls
controlsOf (OptionalEditor controls)	= controls
controlsOf HiddenEditor					= []

//*********************************************************************************************************************
	
(+++>) infixr 5	:: !a !String -> String | gVisualizeText{|*|} a
(+++>) a s = visualizeAsText AsLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualizeText{|*|} a
(<+++) s a = s +++ visualizeAsText AsLabel a
