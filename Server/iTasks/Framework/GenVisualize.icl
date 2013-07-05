implementation module iTasks.Framework.GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdGeneric, StdEnum, StdFunc, Data.List, Data.Generic, Text.JSON
import Data.Maybe, Data.Functor, Text, Text.HTML, Data.Map
import iTasks.Framework.GenUpdate, iTasks.Framework.GenVerify, iTasks.Framework.UIDefinition, iTasks.Framework.UIDiff, iTasks.Framework.Util, iTasks.Framework.HtmlUtil
import iTasks.API.Core.SystemTypes, iTasks.API.Core.LayoutCombinators

visualizeAsLabel :: !a -> String | gVisualizeText{|*|} a
visualizeAsLabel v = concat (gVisualizeText{|*|} AsLabel v)

visualizeAsText :: !a -> String | gVisualizeText{|*|} a
visualizeAsText v = join "\n" (gVisualizeText{|*|} AsText v)

visualizeAsRow :: !a -> [String] | gVisualizeText{|*|} a
visualizeAsRow v = gVisualizeText{|*|} AsRow v

visualizeAsEditor :: !(VerifiedValue a) !TaskId !Layout !*IWorld -> (![(!UIControl,!UIAttributes)],!*IWorld) | gEditor{|*|} a
visualizeAsEditor (v,mask,ver) taskId layout iworld
	# vst		= {VSt|mkVSt taskId iworld & layout = layout}
	# (res,vst)	= gEditor{|*|} [] (v,mask,ver) vst
	= (controlsOf res,kmVSt vst)
	
//Generic text visualizer
generic gVisualizeText a :: !VisualizationFormat !a -> [String]

gVisualizeText{|UNIT|} _ _ = []

gVisualizeText{|RECORD|} fx mode (RECORD x)
	# viz = fx mode x
	= case mode of
		AsLabel		= take 1 viz
		AsText		= viz
		AsRow		= viz
		
gVisualizeText{|FIELD of {gfd_name}|} fx mode (FIELD x)
	# viz = fx mode x
	= case mode of
		AsText		= [camelCaseToWords gfd_name, ": ": viz] ++ [" "]
		AsLabel		= viz
		AsRow		= viz
		
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

gVisualizeText {|[]|} fx  mode val					= [concat (["[":  flatten (intersperse [", "] [fx mode x \\ x <- val])] ++ ["]"])]
gVisualizeText{|Maybe|} fx mode val					= fromMaybe ["-"] (fmap (\v -> fx mode v) val)

gVisualizeText{|Void|} _ _					= []
gVisualizeText{|Dynamic|} _ _				= []
gVisualizeText{|(->)|} _ _ _ _				= []
gVisualizeText{|JSONNode|} _ val			= [toString val]
gVisualizeText{|HtmlTag|} _ html			= [toString html]

derive gVisualizeText Either, (,), (,,), (,,,), Timestamp, Map

mkVSt :: !TaskId *IWorld -> *VSt
mkVSt taskId iworld
	= {VSt| selectedConsIndex = -1, optional = False, disabled = False
	  , taskId = toString taskId, layout = autoLayout, iworld = iworld}

kmVSt :: !*VSt -> *IWorld //inverse of mkVSt
kmVSt {VSt|iworld} = iworld

//Generic visualizer
generic gEditor a | gVisualizeText a, gDefault a, gEditMeta a, JSONEncode a, JSONDecode a
				   :: !DataPath !(VerifiedValue a) !*VSt -> (!VisualizationResult,!*VSt)

gEditor{|UNIT|} dp _ vst = (NormalEditor [],vst)

gEditor{|RECORD of {grd_arity}|} fx _ _ _ _ _ dp (RECORD x,mask,ver) vst=:{VSt|optional,disabled,taskId}
	//When optional and no value yet, just show the checkbox
	| optional &&  not (isTouched mask) && not disabled
		= (OptionalEditor [checkbox False], vst)
	# (fieldsViz,vst) = fx (pairPath grd_arity dp) (x,toPairMask grd_arity mask,toPairVerification grd_arity ver) {VSt|vst & optional = False}
	//For optional records we add the checkbox to clear the entire record
	# viz = if (optional && not disabled) (OptionalEditor [checkbox True:controlsOf fieldsViz]) fieldsViz	
	= (viz,vst)
where
	checkbox checked = (UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId = taskId, editorId = editorId dp, value = Just (JSONBool checked)},newMap)
	
gEditor{|FIELD of {gfd_name}|} fx _ _ _ _ _ dp (val,mask,ver) vst=:{VSt|disabled,layout}
	# (vizBody,vst)		= fx dp (fromFIELD val,mask,ver) vst
	= case vizBody of
		HiddenEditor			= (HiddenEditor,vst)
		NormalEditor controls
			# controls = layout.Layout.editor {UIControlSequence|attributes = addLabel disabled gfd_name newMap, controls = controls, direction = Vertical}
			= (NormalEditor controls,vst)
		OptionalEditor controls	
			# controls = layout.Layout.editor {UIControlSequence|attributes = addLabel True gfd_name newMap, controls = controls, direction = Vertical}
			= (OptionalEditor controls, vst)

gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} fx _ _ hx _ _ dp vv=:(OBJECT x,mask,ver) vst=:{selectedConsIndex = oldSelectedConsIndex,disabled,taskId,layout}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		# (items, vst=:{selectedConsIndex}) = fx dp (x,mask,ver) vst
		# content	= layout.editor {UIControlSequence|attributes = newMap, controls = (if (isTouched mask) (controlsOf items) []), direction = Horizontal}
		= (NormalEditor [(UIDropdown defaultSizeOpts
								{UIChoiceOpts
								| taskId = taskId
								, editorId = editorId dp
								, value = if (isTouched mask) [selectedConsIndex] []
								, options = [gdc.gcd_name \\ gdc <- gtd_conses]}
							,verifyAttributes (x,mask,ver) (hx x))
						: content
						]
		  			,{vst & selectedConsIndex = oldSelectedConsIndex})
	//ADT with one constructor or static render: put content into container, if empty show cons name
	| otherwise
		# (vis,vst) = fx dp (x,mask,ver) vst
		# vis = case vis of
			HiddenEditor 	= HiddenEditor
			NormalEditor []
				= if (isTouched mask || disabled) (NormalEditor [((stringDisplay ((gtd_conses !! vst.selectedConsIndex).gcd_name)),newMap)]) (NormalEditor [])			
			NormalEditor items
				= NormalEditor (layout.editor {UIControlSequence|attributes = newMap, controls = items, direction = Horizontal})
			OptionalEditor items
				= OptionalEditor (layout.editor {UIControlSequence|attributes = newMap, controls = items, direction = Horizontal})
		= (vis,{vst & selectedConsIndex = oldSelectedConsIndex})
where
	addSpacing [] = []
	addSpacing [d:ds] = [d:map (setMargins 0 0 0 5) ds]

gEditor{|CONS of {gcd_index,gcd_arity}|} fx _ _ _ _ _ dp (val,mask,ver) vst=:{VSt|taskId,optional,disabled}
	# (viz,vst)	= fx (pairPath gcd_arity dp) (fromCONS val,toPairMask gcd_arity mask,toPairVerification gcd_arity ver) vst
	= (NormalEditor (controlsOf viz), {VSt | vst & selectedConsIndex = gcd_index})

gEditor{|PAIR|} fx _ _ _ _ _ fy _ _ _ _ _ dp (PAIR x y, CompoundMask [xmask,ymask],CompoundVerification [xver,yver]) vst
	# (dpx,dpy)		= pairPathSplit dp
	# (vizx, vst)	= fx dpx (x,xmask,xver) vst
	# (vizy, vst)	= fy dpy (y,ymask,yver) vst
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

//Encode the full range of fields in the datapath, such that it can be decomposed in PAIRs by the pairSplit
pairPath 0 dp = dp
pairPath 1 dp = dp
pairPath n dp = [0, n - 1: dp]

pairPathSplit [begin,end:dp]
	| range == 2	= (dp ++ [begin],dp ++ [end])
	| range == 3	= (dp ++ [begin],[begin + 1,end:dp])
					= ([begin,middle - 1:dp],[middle,end:dp])
where
	range = end - begin + 1
	middle = range / 2
		
gEditor{|EITHER|} fx _ _ _ _ _ fy _ _ _ _ _ dp (LEFT x,mask,ver) vst = fx dp (x,mask,ver) vst
gEditor{|EITHER|} fx _ _ _ _ _ fy _ _ _ _ _ dp (RIGHT y,mask,ver) vst =  fy dp (y,mask,ver) vst	
		
gEditor{|Int|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled	
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditInt defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},verifyAttributes vv (gEditMeta{|*|} val))],vst)
	
gEditor{|Real|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled} 
	| disabled	
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gEditor{|Char|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled	
    	= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditString defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gEditor{|String|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
    	= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value= checkMask mask val},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditString defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gEditor{|Bool|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled		
		= (NormalEditor [(UIViewCheckbox defaultSizeOpts {UIViewOpts|value =checkMask mask val},verifyAttributes vv (gEditMeta{|*|} val))],vst)
	| otherwise	
		= (NormalEditor [(UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gEditor{|[]|} fx _ _ _ _ _ dp (val,mask,ver) vst=:{VSt|taskId,disabled,layout}
	# (items,vst)	= listControl dp val (subMasks (length val) mask) (subVerifications (length val) ver) vst
	= (NormalEditor [(listContainer items,newMap)],vst)
where
	listControl dp items masks vers vst=:{VSt|optional,disabled}
		# (itemsVis,vst)	= childVisualizations fx dp items masks vers vst
		# numItems = length items
		| disabled
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
		| otherwise
			//# (newItem,vst)		= newChildVisualization fx True vst
			//= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [newItemControl newItem],vst)
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
						
	listItemControl disabled numItems idx item 
		# controls	= map fst (layout.editor {UIControlSequence| attributes = newMap, controls = controlsOf item, direction = Vertical})
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mup_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=idx == 0}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mdn_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= idx == numItems - 1}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("rem_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=False}
					  ]
		= setHeight WrapSize (setDirection Horizontal (defaultContainer (if disabled controls (controls ++ buttons))))
/*
	newItemControl item
		# controls	= map fst (layout.editor (newMap,controlsOf item))
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=True}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= True}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=name,value=Nothing} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=True}
					  ]
		= setDirection Horizontal (defaultContainer (controls ++ buttons))
*/	
	addItemControl numItems
		# controls	= [UIViewString /*{*/defaultSizeOpts /* & width=Just FlexSize }*/ {UIViewOpts|value= Just (numItemsText numItems)}]
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString "add")} {UIButtonOpts|text=Nothing,iconCls=Just "icon-add",disabled=False}]
		= setHeight WrapSize (setDirection Horizontal (defaultContainer (controls ++ buttons)))
	
	listContainer items
		= setHeight WrapSize (defaultContainer items)
	
	numItemsText 1 = "1 item"
	numItemsText n = toString n +++ " items"
	
gEditor{|(,)|} fx _ _ _ _ _ fy _ _ _ _ _ dp ((x,y),mask,ver) vst
	# (vizx, vst)	= fx [0:dp] (x,subMasks 2 mask !! 0,subVerifications 2 ver !! 0) vst
	# (vizy, vst)	= fy [1:dp] (y,subMasks 2 mask !! 1,subVerifications 2 ver !! 1) vst
	# viz = case (vizx,vizy) of
		(HiddenEditor,HiddenEditor) = HiddenEditor
		_	= NormalEditor (controlsOf vizx ++ controlsOf vizy)
				 
	= (viz, vst)

gEditor{|(->)|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ vst	= (HiddenEditor,vst)
		
gEditor{|Dynamic|} _ _ vst	= (HiddenEditor,vst)

gEditor{|Maybe|} fx _ dx _ _ _ dp (val,mask,ver) vst=:{VSt|optional,disabled}
	| disabled && isNothing val = (OptionalEditor [], vst)
	# (viz,vst) = case val of
		(Just x)	= fx dp (x,mask,ver) {VSt|vst & optional = True}
		_			= fx dp (dx [],Untouched,ver) {VSt|vst & optional = True}
	= (toOptional viz, {VSt|vst & optional = optional})
where
	toOptional	(NormalEditor ex)	= OptionalEditor ex
	toOptional	viz					= viz
	
gEditor{|Void|} _ _ vst = (HiddenEditor,vst)
gEditor{|HtmlTag|}	dp (val,mask,ver) vst
	= (NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just val},newMap)], vst)

derive gEditor JSONNode, Either, (,,), (,,,), Timestamp, Map //TODO Make specializations for (,,) and (,,,)

generic gEditMeta a :: a -> [EditMeta]

gEditMeta{|UNIT|} _			= [{label=Nothing,hint=Nothing}]
gEditMeta{|PAIR|} fx fy _	= fx undef ++ fy undef
gEditMeta{|EITHER|} fx fy _	= fx undef //Only consider first constructor
gEditMeta{|OBJECT|} fx _	= fx undef
gEditMeta{|CONS|} fx _		= fx undef
gEditMeta{|RECORD|} fx _ 	= fx undef
gEditMeta{|FIELD of {gfd_name}|} fx _
							= [{EditMeta|m & label = Just (fromMaybe (camelCaseToWords gfd_name) label)} \\ m=:{EditMeta|label} <- fx undef]
gEditMeta{|Int|}	_		= [{label=Nothing,hint=Just "You may enter an integer number"}]
gEditMeta{|Char|} _			= [{label=Nothing,hint=Just "You may enter a single character"}]
gEditMeta{|String|} _		= [{label=Nothing,hint=Just "You may enter a single line of text"}]
gEditMeta{|Real|} _			= [{label=Nothing,hint=Just "You may enter a decimal number"}]
gEditMeta{|Bool|} _ 		= [{label=Nothing,hint=Nothing}]
gEditMeta{|Dynamic|}	_	= [{label=Nothing,hint=Just ""}]
gEditMeta{|HtmlTag|}	_	= [{label=Nothing,hint=Nothing}]
gEditMeta{|(->)|} _ _ _		= [{label=Nothing,hint=Nothing}]
gEditMeta{|Maybe|} fx _		= fx undef
gEditMeta{|[]|} fx _		= fx undef

derive gEditMeta (,), (,,), (,,,), Either, Void, Map, JSONNode, Timestamp

//***** UTILITY FUNCTIONS *************************************************************************************************	
	
childVisualizations :: !(DataPath (VerifiedValue a) -> .(*VSt -> *(!VisualizationResult,*VSt))) !DataPath ![a] ![InteractionMask] ![Verification] !*VSt -> *(![VisualizationResult],!*VSt)
childVisualizations fx dp children masks vers vst = childVisualizations` 0 children masks vers [] vst
where
	childVisualizations` i [] [] [] acc vst
		= (reverse acc,vst)
	childVisualizations` i [child:children] [mask:masks] [ver:vers] acc vst
		# (childV,vst) = fx [i:dp] (child,mask,ver) vst
		= childVisualizations` (i + 1) children masks vers [childV:acc] vst

newChildVisualization :: !((Maybe a) -> .(*VSt -> *(VisualizationResult,*VSt))) !Bool !*VSt -> *(!VisualizationResult,!*VSt)
newChildVisualization fx newOptional vst=:{VSt|optional}
	# (childV,vst) = fx Nothing {VSt|vst & optional = newOptional}
	= (childV,{VSt|vst & optional = optional})

verifyAttributes :: !(VerifiedValue a) [EditMeta] -> UIAttributes
verifyAttributes (val,mask,ver) meta
	| isTouched mask	= case ver of
		(CorrectValue msg)		= put VALID_ATTRIBUTE (fromMaybe "This value is ok" msg) newMap
		(IncorrectValue msg)	= put ERROR_ATTRIBUTE msg newMap
		(UnparsableValue)		= put ERROR_ATTRIBUTE "This value not in the required format" newMap
		(MissingValue)			= put ERROR_ATTRIBUTE "This value is required" newMap
		_						= newMap
	| otherwise
		= case meta of	
			[{EditMeta|hint=Just hint}:_]	= put HINT_ATTRIBUTE hint newMap
			_								= newMap

addLabel :: !Bool !String !UIAttributes -> UIAttributes
addLabel optional label attr = put LABEL_ATTRIBUTE (format optional label) attr
where
	format optional label = camelCaseToWords label +++ if optional "" "*" +++ ":" //TODO: Move to layout

checkMask :: !InteractionMask a -> Maybe a
checkMask mask val
    | isTouched mask    = Just val
                        = Nothing

checkMaskValue :: !InteractionMask a -> Maybe JSONNode | JSONEncode{|*|} a
checkMaskValue Touched val               = Just (toJSON val)
checkMaskValue (TouchedWithState s) val  = Just (toJSON val)
checkMaskValue (TouchedUnparsed r) _  	 = Just r
checkMaskValue _ _                       = Nothing

controlsOf :: !VisualizationResult -> [(UIControl,UIAttributes)]
controlsOf (NormalEditor controls)		= controls
controlsOf (OptionalEditor controls)	= controls
controlsOf HiddenEditor					= []

//*********************************************************************************************************************
	
(+++>) infixr 5	:: !a !String -> String | gVisualizeText{|*|} a
(+++>) a s = visualizeAsLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualizeText{|*|} a
(<+++) s a = s +++ visualizeAsLabel a
