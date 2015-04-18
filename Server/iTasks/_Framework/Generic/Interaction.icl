implementation module iTasks._Framework.Generic.Interaction

from StdFunc import const
import StdList, StdBool, StdTuple, StdMisc
import Data.Maybe, Data.Either, Data.Error, Data.Map, Data.Generic, Data.Functor, Data.Tuple
import Text, Text.JSON
import iTasks._Framework.IWorld
import iTasks._Framework.UIDefinition
import iTasks._Framework.Util
import iTasks.API.Core.Types
import iTasks.API.Core.LayoutCombinators

visualizeAsEditor :: !(VerifiedValue a) !TaskId !LayoutRules !*IWorld -> (![(!UIControl,!UIAttributes)],!*IWorld) | gEditor{|*|} a & gEditMeta{|*|} a
visualizeAsEditor (v,mask,ver) taskId layout iworld
	# vst = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = toString taskId, layout = layout, iworld = iworld}
	# (res,vst=:{VSt|iworld})	= gEditor{|*|} [] (v,mask,ver) (gEditMeta{|*|} v) vst
	= (controlsOf res,iworld)

updateValueAndMask :: !TaskId !DataPath !JSONNode !(MaskedValue a) !*IWorld -> (!MaskedValue a,!*IWorld) | gUpdate{|*|} a
updateValueAndMask taskId path update (a,mask) iworld
    # (res,ust=:{USt|iworld}) = gUpdate{|*|} path update (a,mask) {USt|taskId=toString taskId,editorId=editorId path,iworld=iworld}
    = (res,iworld)

//Generic visualizer
generic gEditor a | gText a, gDefault a, gEditMeta a, JSONEncode a, JSONDecode a
				   :: !DataPath !(VerifiedValue a) ![EditMeta] !*VSt -> (!VisualizationResult,!*VSt)

gEditor{|UNIT|} dp _ _ vst = (NormalEditor [],vst)

gEditor{|EITHER|} fx _ _ mx _ _ fy _ _ my _ _ dp (LEFT x,mask,ver) meta vst = fx dp (x,mask,ver) (mx x) vst
gEditor{|EITHER|} fx _ _ mx _ _ fy _ _ my _ _ dp (RIGHT y,mask,ver) meta vst =  fy dp (y,mask,ver) (my y) vst	

gEditor{|RECORD of {grd_arity}|} fx _ _ mx _ _ dp (RECORD x,mask,ver) meta vst=:{VSt|optional,disabled,taskId}
	//When optional and no value yet, just show the checkbox
	| optional && not (isTouched mask)
		= if disabled (OptionalEditor [],vst) (OptionalEditor [checkbox False], vst)
	# (fieldsViz,vst) = fx (pairPath grd_arity dp) (x,toPairMask grd_arity mask,toPairVerification grd_arity ver) (mx x) {VSt|vst & optional = False}
	//For optional records we add the checkbox to clear the entire record
	# viz = if (optional && not disabled) (OptionalEditor [checkbox True:controlsOf fieldsViz]) fieldsViz	
	= (viz,vst)
where
	checkbox checked = (UIEditCheckbox defaultFSizeOpts {UIEditOpts|taskId = taskId, editorId = editorId dp, value = Just (JSONBool checked)},newMap)

gEditor{|FIELD of {gfd_name}|} fx _ _ mx _ _ dp (FIELD x,mask,ver) _ vst=:{VSt|disabled,layout}
	# (vizBody,vst)		= fx dp (x,mask,ver) (mx x) vst
	= case vizBody of
		HiddenEditor			= (HiddenEditor,vst)
		NormalEditor controls
			= (NormalEditor [(c,addLabel disabled gfd_name a) \\ (c,a) <- controls],vst)
		OptionalEditor controls	
			= (OptionalEditor [(c,addLabel True gfd_name a) \\ (c,a) <- controls], vst)

gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} fx _ _ mx _ _ dp vv=:(OBJECT x,mask,ver) meta vst=:{selectedConsIndex = oldSelectedConsIndex,disabled,taskId,layout}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		# (items, vst=:{selectedConsIndex}) = fx dp (x,mask,ver) meta {VSt|vst & optional=False}
        # (controls,choice) = case mask of
            Untouched   = ([],[])
            Blanked     = ([],[])
            _           = (controlsOf items,[selectedConsIndex])
		# content	= layout.layoutSubEditor {UIForm|attributes = newMap, controls = controls,size = defaultSizeOpts}
		= (NormalEditor [(UIDropdown defaultHSizeOpts
								{UIChoiceOpts
								| taskId = taskId
								, editorId = editorId dp
								, value = choice
								, options = [gdc.gcd_name \\ gdc <- gtd_conses]}
							, editorAttributes (x,case mask of (CompoundMask _) = Touched; _ = mask,ver) [{EditMeta|hint=Just "Select an option",label=Nothing,unit=Nothing}])
						: content
						]
		  			,{vst & selectedConsIndex = oldSelectedConsIndex})
	//ADT with one constructor or static render: put content into container, if empty show cons name
	| otherwise
		# (vis,vst) = fx dp (x,mask,ver) meta vst
		# vis = case vis of
			HiddenEditor 	= HiddenEditor
			NormalEditor []
                = NormalEditor [(stringDisplay (if (isTouched mask) (gtd_conses !! vst.selectedConsIndex).gcd_name ""),newMap)]
				//= if (isTouched mask) (NormalEditor [((stringDisplay ((gtd_conses !! vst.selectedConsIndex).gcd_name)),newMap)]) (NormalEditor [])			
			NormalEditor items
				= NormalEditor (layout.layoutSubEditor {UIForm|attributes = newMap, controls = items, size = defaultSizeOpts})
			OptionalEditor items
				= OptionalEditor (layout.layoutSubEditor {UIForm|attributes = newMap, controls = items, size = defaultSizeOpts})
		= (vis,{vst & selectedConsIndex = oldSelectedConsIndex})

gEditor{|CONS of {gcd_index,gcd_arity}|} fx _ _ mx _ _ dp (CONS x,mask,ver) meta vst=:{VSt|taskId,optional,disabled}
	# (viz,vst)	= fx (pairPath gcd_arity dp) (x,toPairMask gcd_arity mask,toPairVerification gcd_arity ver) meta vst
    = (viz,{VSt | vst & selectedConsIndex = gcd_index})

gEditor{|PAIR|} fx _ _ mx _ _ fy _ _ my _ _ dp (PAIR x y, CompoundMask [xmask,ymask],CompoundVerification [xver,yver]) meta vst
	# (dpx,dpy)		= pairPathSplit dp
	# (vizx, vst)	= fx dpx (x,xmask,xver) (mx x) vst
	# (vizy, vst)	= fy dpy (y,ymask,yver) (my y) vst
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
pairPath 1 dp = dp ++ [0]
pairPath n dp = [0, n - 1: dp]

pairPathSplit [begin,end:dp]
	| range == 2	= (dp ++ [begin],dp ++ [end])
	| range == 3	= (dp ++ [begin],[begin + 1,end:dp])
					= ([begin,middle - 1:dp],[middle,end:dp])
where
	range = end - begin + 1
	middle = begin + range / 2
		
		
gEditor{|Int|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled	
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditInt defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|Real|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled	
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditDecimal defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|Char|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled	
    	= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditString defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|String|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
    	= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value= checkMask mask val},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditString defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|Bool|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled} //Bools are shown as optional by default, because a mandatory bool makes little sense
	| disabled		
		= (OptionalEditor [(UIViewCheckbox defaultFSizeOpts {UIViewOpts|value =checkMask mask val},newMap)],vst)
	| otherwise	
		= (OptionalEditor [(UIEditCheckbox defaultFSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|[]|} fx _ _ mx _ _ dp (val,mask,ver) meta vst=:{VSt|taskId,disabled,layout}
	# (items,vst)	= listControl dp val (subMasks (length val) mask) (subVerifications (length val) ver) vst
	= (NormalEditor [(listContainer items,newMap)],vst)
where
	listControl dp items masks vers vst=:{VSt|optional,disabled}
		# (itemsVis,vst)	= childVisualizations fx mx dp items masks vers vst
		# numItems = length items
		| disabled
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
		| otherwise
			//# (newItem,vst)		= newChildVisualization fx True vst
			//= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [newItemControl newItem],vst)
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
						
	listItemControl disabled numItems idx item
		//# controls	= map fst (layout.layoutSubEditor {UIForm| attributes = newMap, controls = controlsOf item, size = defaultSizeOpts})
		# controls	= decorateControls (layout.layoutSubEditor {UIForm| attributes = newMap, controls = controlsOf item, size = defaultSizeOpts})
		# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mup_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=idx == 0}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mdn_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= idx == numItems - 1}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("rem_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=False}
					  ]
		= setHeight WrapSize (setDirection Horizontal (defaultContainer (if disabled controls (controls ++ buttons))))
/*
	newItemControl item
		# controls	= map fst (layout.layoutSubEditor (newMap,controlsOf item))
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

gEditor{|EditableList|} fx _ _ mx _ _ dp ({EditableList|items,add,remove,reorder,count},mask,ver) meta vst=:{VSt|taskId,disabled,layout}
	# (controls,vst) = listControls dp items (subMasks (length items) mask) (subVerifications (length items) ver) vst
	= (NormalEditor [(listContainer controls,newMap)],vst)
where
    enableAdd = case add of ELNoAdd = False ; _ = True;

	listControls dp items masks vers vst=:{VSt|optional,disabled}
		# (itemsVis,vst)	= childVisualizations fx mx dp items masks vers vst
		# numItems = length items
		| not disabled && (enableAdd || count)
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
		| otherwise
			= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
						
	listItemControl disabled numItems idx item
		# controls	= map (setWidth FlexSize) (decorateControls (layout.layoutSubEditor {UIForm| attributes = newMap, controls = controlsOf item, size = defaultSizeOpts}))
		# buttons	= (if reorder
                      [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mup_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=idx == 0}
					  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mdn_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= idx == numItems - 1}
                      ] []) ++
					  (if remove
                      [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("rem_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=False}
					  ] [])
		= setHalign AlignRight (setHeight WrapSize (setDirection Horizontal (defaultContainer (if disabled controls (controls ++ buttons)))))
	addItemControl numItems
		# counter   = if count [UIViewString {UISizeOpts|defaultSizeOpts & width=Just FlexSize} {UIViewOpts|value= Just (numItemsText numItems)}] []
		# button	= if enableAdd [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString "add")} {UIButtonOpts|text=Nothing,iconCls=Just "icon-add",disabled=False}] []
		= setHalign AlignRight (setHeight WrapSize (setDirection Horizontal (defaultContainer (counter ++ button))))
	
	listContainer controls
		= setHeight WrapSize (defaultContainer controls)
	
	numItemsText 1 = "1 item"
	numItemsText n = toString n +++ " items"
	
gEditor{|()|} dp vv meta vst = (HiddenEditor,vst)

gEditor{|(,)|} fx _ _ mx _ _ fy _ _ my _ _ dp ((x,y),mask,ver) meta vst
	# (vizx, vst)	= fx (dp ++ [0]) (x,subMasks 2 mask !! 0,subVerifications 2 ver !! 0) (mx x) vst
	# (vizy, vst)	= fy (dp ++ [1]) (y,subMasks 2 mask !! 1,subVerifications 2 ver !! 1) (my y) vst
	# viz = case (vizx,vizy) of
		(HiddenEditor,HiddenEditor) = HiddenEditor
		_	= NormalEditor (controlsOf vizx ++ controlsOf vizy)
	= (viz, vst)

gEditor{|(->)|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ vst = (HiddenEditor,vst)
		
gEditor{|Dynamic|} _ _ _ vst = (HiddenEditor,vst)

gEditor{|Maybe|} fx _ dx mx _ _ dp (val,mask,ver) meta vst=:{VSt|optional,disabled}
	# (viz,vst) = case val of
		(Just x)	= fx dp (x,mask,ver) (mx x) {VSt|vst & optional = True}
		_			= fx dp (dx,Untouched,ver) (mx dx) {VSt|vst & optional = True}
	= (toOptional viz, {VSt|vst & optional = optional})
where
	toOptional	(NormalEditor ex)	= OptionalEditor ex
	toOptional	viz					= viz
	
gEditor{|Void|} _ _ _ vst = (HiddenEditor,vst)
gEditor{|HtmlTag|}	dp (val,mask,ver) meta vst
	= (NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just val},newMap)], vst)
gEditor{|RWShared|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ vst = (HiddenEditor,vst)

derive gEditor JSONNode, Either, MaybeError, (,,), (,,,), Timestamp, Map //TODO Make specializations for (,,) and (,,,)

generic gEditMeta a :: a -> [EditMeta]

gEditMeta{|UNIT|} _			= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|PAIR|} fx fy _	= fx undef ++ fy undef
gEditMeta{|EITHER|} fx fy _	= fx undef //Only consider first constructor
gEditMeta{|OBJECT|} fx _	= fx undef
gEditMeta{|CONS|} fx _		= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|RECORD|} fx _ 	= fx undef
gEditMeta{|FIELD of {gfd_name}|} fx _
							= [{EditMeta|m & label = Just (fromMaybe (camelCaseToWords gfd_name) label)} \\ m=:{EditMeta|label} <- fx undef]
gEditMeta{|Int|}	_		= [{label=Nothing,hint=Just "You may enter an integer number",unit=Nothing}]
gEditMeta{|Char|} _			= [{label=Nothing,hint=Just "You may enter a single character",unit=Nothing}]
gEditMeta{|String|} _		= [{label=Nothing,hint=Just "You may enter a single line of text",unit=Nothing}]
gEditMeta{|Real|} _			= [{label=Nothing,hint=Just "You may enter a decimal number",unit=Nothing}]
gEditMeta{|Bool|} _ 		= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|Dynamic|}	_	= [{label=Nothing,hint=Just "",unit=Nothing}]
gEditMeta{|HtmlTag|}	_	= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|(->)|} _ _ _		= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|Maybe|} fx _		= fx undef
gEditMeta{|[]|} fx _		        = fx undef
gEditMeta{|EditableList|} fx _      = fx undef

gEditMeta{|()|} _                   = []
gEditMeta{|(,)|} fa fb _            = fa undef ++ fb undef
gEditMeta{|(,,)|} fa fb fc _        = fa undef ++ fb undef ++ fc undef
gEditMeta{|(,,,)|} fa fb fc fd _    = fa undef ++ fb undef ++ fc undef ++ fd undef
gEditMeta{|RWShared|} _ _ _ _ = [{label=Nothing,hint=Nothing,unit=Nothing}]

derive gEditMeta Either, MaybeError, Void, Map, JSONNode, Timestamp, EditableListAdd

//Generic Verify
generic gVerify a :: !VerifyOptions (MaskedValue a) -> Verification

gVerify{|UNIT|} _ (value,mask) = CorrectValue Nothing

gVerify{|PAIR|} fx fy options (PAIR x y, CompoundMask [xmask,ymask]) 
	= CompoundVerification [fx options (x,xmask), fy options (y,ymask)]
	
gVerify{|EITHER|} fx _ options (LEFT x,mask) = fx options (x,mask)
	
gVerify{|EITHER|} _ fy options (RIGHT y,mask) = fy options (y,mask)
	
gVerify{|RECORD of {grd_arity}|} fx options (RECORD x, mask)
	= fromPairVerification grd_arity (fx options (x, toPairMask grd_arity mask))
	
gVerify{|FIELD|} fx options (FIELD x,mask) = fx options (x,mask)
	
gVerify{|OBJECT|} fx options=:{VerifyOptions|optional} (OBJECT x,mask) = case mask of
    Blanked     = if optional MissingValue (CorrectValue Nothing)
    _           = fx options (x,mask)
	
gVerify{|CONS of {gcd_arity}|} fx options (CONS x,mask)
	= fromPairVerification gcd_arity (fx options (x, toPairMask gcd_arity mask))
	
gVerify{|Int|} options mv = simpleVerify options mv
gVerify{|Real|} options mv = simpleVerify options mv
gVerify{|Char|} options mv = simpleVerify options mv
gVerify{|String|} options mv = simpleVerify options mv
gVerify{|Bool|} options mv = alwaysValid mv

gVerify{|Maybe|} fx options (Nothing, mask) = CorrectValue Nothing
gVerify{|Maybe|} fx options (Just x, mask) = fx {VerifyOptions|options & optional = True} (x,mask)
	
gVerify{|[]|} fx  options=:{VerifyOptions|optional,disabled} (list,mask)
	= CompoundVerification (verifyListItems list (subMasks (length list) mask))
where
	verifyListItems [] [] = []
	verifyListItems [x:xs] [m:ms] = [fx options (x,m):verifyListItems xs ms]

gVerify{|EditableList|} fx options=:{VerifyOptions|optional,disabled} ({EditableList|items},mask)
	= CompoundVerification (verifyListItems items (subMasks (length items) mask))
where
	verifyListItems [] [] = []
	verifyListItems [x:xs] [m:ms] = [fx options (x,m):verifyListItems xs ms]

gVerify{|(->)|} _ _ _ mv	= alwaysValid mv
gVerify{|Dynamic|}	_ mv	= alwaysValid mv

gVerify{|HtmlTag|} _ mv = alwaysValid mv
gVerify{|JSONNode|} _ mv = alwaysValid mv
gVerify{|()|} _ mv      = alwaysValid mv
gVerify{|RWShared|} _ _ _ _ mv = alwaysValid mv

derive gVerify (,), (,,), (,,,), Void, Either, MaybeError, Timestamp, Map

//Generic updater
generic gUpdate a | gDefault a, JSONEncode a, JSONDecode a :: !DataPath !JSONNode !(MaskedValue a) !*USt -> (!MaskedValue a,!*USt)

gUpdate{|UNIT|} _ _ val ust = (val,ust)

gUpdate{|PAIR|} gUpdx gDefx jEncx jDecx gUpdy gDefy jEncy jDecy [0:target] upd (PAIR x y, xmask) ust
	# ((x,xmask),ust) = gUpdx target upd (x,xmask) ust
	= ((PAIR x y,xmask),ust)
gUpdate{|PAIR|} gUpdx gDefx jEncx jDecx gUpdy gDefy jEncy jDecy [1:target] upd (PAIR x y, ymask) ust
	# ((y,ymask),ust) = gUpdy target upd (y,ymask) ust
	= ((PAIR x y,ymask),ust)
gUpdate{|PAIR|} gUpdx gDefx jEncx jDecx gUpdy gDefy jEncy jDecy target upd val ust = (val,ust)

gUpdate{|EITHER|} gUpdx gDefx jEncx jDecx gUpdy gDefy jEncy jDecy [t:ts] upd (either,mask) ust
    | t == -1 = case ts of
        [] = ((LEFT (gDefx), Untouched),ust)
        _  = appFst (appFst LEFT) (gUpdx ts upd (gDefx,Untouched) ust)
    | t == -2 = case ts of
        [] = ((RIGHT (gDefy), Untouched),ust)
        _  = appFst (appFst RIGHT) (gUpdy ts upd (gDefy,Untouched) ust)
    | otherwise
        = case either of
            (LEFT x)  = appFst (appFst LEFT) (gUpdx [t:ts] upd (x,mask) ust)
            (RIGHT y) = appFst (appFst RIGHT) (gUpdy [t:ts] upd (y,mask) ust)

gUpdate{|OBJECT of {gtd_num_conses,gtd_conses}|} gUpdx gDefx jEncx jDecx [] upd (OBJECT x,mask) ust//Update is a constructor switch
	# consIdx = case upd of
		JSONInt i	= i
		_			= 0
	# mask	        = case upd of
		JSONNull	= Blanked	//Reset
		_			= CompoundMask (repeatn (gtd_conses !! consIdx).gcd_arity Untouched)
    # ((x,_),ust)= gUpdx (updConsPath (if (consIdx < gtd_num_conses) consIdx 0) gtd_num_conses) upd (x,mask) ust
	= ((OBJECT x, mask),ust)

gUpdate{|OBJECT|} gUpdx gDefx jEncx jDecx target upd (OBJECT object, mask) ust //Update is targeted somewhere in a substructure of this value
	= appFst (appFst OBJECT) (gUpdx target upd (object,mask) ust)

gUpdate{|CONS of {gcd_arity,gcd_index}|} gUpdx gDefx jEncx jDecx [index:target] upd (CONS cons,mask) ust
	| index >= gcd_arity
		= ((CONS cons,mask),ust)	
	# childMasks = subMasks gcd_arity mask
	# ((cons,targetMask),ust) = gUpdx (updPairPath index gcd_arity ++ target) upd (cons,childMasks !! index) ust
	= ((CONS cons,CompoundMask (updateAt index targetMask childMasks)),ust)
gUpdate{|CONS|} gUpdx gDefx jEncx jDecx target upd val ust = (val,ust)

gUpdate{|RECORD of {grd_arity}|} gUpdx gDefx jEncx jDecx [] upd (RECORD record,mask) ust
    # mask = case upd of
        JSONBool False  = Blanked
        _               = Touched
    = ((RECORD record,mask),ust)
gUpdate{|RECORD of {grd_arity}|} gUpdx gDefx jEncx jDecx [index:target] upd (RECORD record,mask) ust
	| index >= grd_arity
		= ((RECORD record,mask),ust)
	# childMasks = subMasks grd_arity mask
	# ((record,targetMask),ust) = gUpdx (updPairPath index grd_arity ++ target) upd (record,childMasks !! index) ust
	= ((RECORD record,CompoundMask (updateAt index targetMask childMasks)),ust)

gUpdate{|RECORD|} gUpdx gDefx jEncx jDecx _ _ val ust = (val,ust)
	
gUpdate{|FIELD|} gUpdx gDefx jEncx jDecx target upd (FIELD field,mask) ust
    = appFst (appFst FIELD) (gUpdx target upd (field,mask) ust)

updConsPath i n
 	| i >= n	
		= []
	| n == 1
		= []
	| i < (n/2)
		= [ -1: updConsPath i (n/2) ]
	| otherwise
		= [ -2: updConsPath (i - (n/2)) (n - (n/2)) ]

updPairPath i n
	| i >= n
		= []
	| n == 1
		= []
	| i < (n /2)
		= [0: updPairPath i (n /2)]
	| otherwise
		= [1: updPairPath (i - (n/2)) (n - (n/2))]

gUpdate{|Int|}		target upd val ust = basicUpdateSimple target upd val ust
gUpdate{|Real|}		target upd val ust = basicUpdateSimple target upd val ust
gUpdate{|Char|}		target upd val ust = basicUpdateSimple target upd val ust
gUpdate{|Bool|}		target upd val ust = basicUpdateSimple target upd val ust
gUpdate{|String|}	target upd val ust = basicUpdateSimple target upd val ust
			
gUpdate{|Maybe|} gUpdx gDefx jEncx jDecx target upd (m,mmask) ust
	| isEmpty target && (upd === JSONNull || upd === JSONBool False)
		= ((Nothing, Blanked),ust) //Reset
	| otherwise
		= appFst (appFst Just) (gUpdx target upd (maybe (gDefx,Untouched) (\x -> (x,mmask)) m) ust)

gUpdate{|[]|} gUpdx gDefx jEncx jDecx target upd (l,listMask) ust
	# (l,childMasks)
		= case ((not (isEmpty target)) && (hd target >= (length l))) of
			True
				= (l++[gDefx], subMasks (length l) listMask ++ [Untouched])
			False
				= (l, subMasks (length l) listMask)
	# ((l,childMasks),ust) = updateElements gUpdx target upd l childMasks ust
	| isEmpty target
		//Process the reordering commands
		# split = split "_" (fromMaybe "" (fromJSON upd))
		# index = toInt (last split)
		# (l,childMasks) = case hd split of	
			"mup" = (swap l index,swap childMasks index) 
			"mdn" = (swap l (index+1),swap childMasks (index+1))
			"rem" = (removeAt index l,removeAt index childMasks)	
			"add"
				= (insertAt (length l) gDefx l, insertAt (length l) Untouched childMasks)
			_	
				= (l,childMasks)
		= ((l,CompoundMask childMasks),ust)
	| otherwise
		= ((l,CompoundMask childMasks),ust)
where
	updateElements fx [i:target] upd elems masks ust
		| i >= (length elems)
			= ((elems,masks),ust)
		# ((nx,nm),ust)	= fx target upd (elems !! i,masks !! i) ust
		= ((updateAt i nx elems, updateAt i nm masks),ust)
	updateElements fx target upd elems masks ust
		= ((elems,masks),ust)
	
	swap []	  _		= []
	swap list index
		| index == 0 			= list //prevent move first element up
		| index >= length list 	= list //prevent move last element down
		| otherwise				
			# f = list !! (index-1)
			# l = list !! (index)
			= updateAt (index-1) l (updateAt index f list)

gUpdate{|EditableList|} gUpdx gDefx jEncx jDecx target upd (l=:{EditableList|items,add,remove,reorder},listMask) ust
	# (items,childMasks)
		= case ((not (isEmpty target)) && (hd target >= (length items))) of
			True
				= (items++[gDefx], subMasks (length items) listMask ++ [Untouched])
			False
				= (items, subMasks (length items) listMask)
	# ((items,childMasks),ust) = updateElements gUpdx target upd items childMasks ust
	| isEmpty target
		//Process the reordering commands
		# split = split "_" (fromMaybe "" (fromJSON upd))
		# index = toInt (last split)
		# (items,childMasks) = case hd split of	
			"mup" = if reorder (swap items index,swap childMasks index) (items,childMasks)
			"mdn" = if reorder (swap items (index+1),swap childMasks (index+1)) (items,childMasks)
			"rem" = if remove  (removeAt index items,removeAt index childMasks)	(items,childMasks)
			"add"
                = case add of
                    ELAddBlank      = (insertAt (length items) gDefx items, insertAt (length items) Untouched childMasks)
                    ELAddValue f    = (insertAt (length items) (f items) items, insertAt (length items) Touched childMasks)
                    _               = (items,childMasks)
			_	
				= (items,childMasks)
		= (({EditableList|l & items = items},CompoundMask childMasks),ust)
	| otherwise
		= (({EditableList|l & items = items},CompoundMask childMasks),ust)
where
	updateElements fx [i:target] upd elems masks ust
		| i >= (length elems)
			= ((elems,masks),ust)
		# ((nx,nm),ust)	= fx target upd (elems !! i,masks !! i) ust
		= ((updateAt i nx elems, updateAt i nm masks),ust)
	updateElements fx target upd elems masks ust
		= ((elems,masks),ust)
	
	swap []	  _		= []
	swap list index
		| index == 0 			= list //prevent move first element up
		| index >= length list 	= list //prevent move last element down
		| otherwise				
			# f = list !! (index-1)
			# l = list !! (index)
			= updateAt (index-1) l (updateAt index f list)

gUpdate{|Dynamic|} target upd val ust = basicUpdate (\Void v -> Just v) target upd val ust
gUpdate{|(->)|} _ _ _ gUpdy _ _ _ _ target upd val ust = basicUpdate (\Void v -> Just v) target upd val ust

gUpdate{|HtmlTag|} target upd val ust = (val,ust)
gUpdate{|()|} target upd val ust = (val,ust)
gUpdate{|RWShared|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ val ust = (val,ust)

derive gUpdate Either, MaybeError, (,), (,,), (,,,), JSONNode, Void, Timestamp, Map

checkMask :: !InteractionMask a -> Maybe a
checkMask mask val
    | isTouched mask    = Just val
                        = Nothing

checkMaskValue :: !InteractionMask a -> Maybe JSONNode | JSONEncode{|*|} a
checkMaskValue Touched val               = Just (toJSON val)
checkMaskValue (TouchedWithState s) val  = Just (toJSON val)
checkMaskValue (TouchedUnparsed r) _  	 = Just r
checkMaskValue _ _                       = Nothing

editorAttributes :: !(VerifiedValue a) [EditMeta] -> UIAttributes
editorAttributes (val,mask,ver) meta
    # hint = case meta of
        [{EditMeta|hint}]   = hint
        _                   = Nothing
    # attr = case meta of
        [{EditMeta|unit=Just (Left unit)}:_]    = put PREFIX_ATTRIBUTE unit newMap
        [{EditMeta|unit=Just (Right unit)}:_]   = put POSTFIX_ATTRIBUTE unit newMap
        _                                       = newMap
    | isTouched mask = case ver of
        (CorrectValue msg)		= put VALID_ATTRIBUTE (fromMaybe "This value is ok" msg) attr
        (WarningValue msg)		= put WARNING_ATTRIBUTE msg attr
        (IncorrectValue msg)	= put ERROR_ATTRIBUTE msg attr
        (UnparsableValue)		= put ERROR_ATTRIBUTE "This value not in the required format" attr
        (MissingValue)			= put ERROR_ATTRIBUTE "This value is required" attr
        _						= attr
    | otherwise
        = maybe attr (\h -> put HINT_ATTRIBUTE h attr) hint

controlsOf :: !VisualizationResult -> [(UIControl,UIAttributes)]
controlsOf (NormalEditor controls)		= controls
controlsOf (OptionalEditor controls)	= controls
controlsOf HiddenEditor					= []

addLabel :: !Bool !String !UIAttributes -> UIAttributes
addLabel optional label attr = putCond LABEL_ATTRIBUTE (format optional label) attr
where
	format optional label = camelCaseToWords label +++ if optional "" "*" +++ ":" //TODO: Move to layout
    putCond k v m = maybe (put k v m) (const m) (get k m)

childVisualizations :: !(DataPath (VerifiedValue a) [EditMeta] -> .(*VSt -> *(!VisualizationResult,*VSt))) !(a -> [EditMeta]) !DataPath ![a] ![InteractionMask] ![Verification] !*VSt -> *(![VisualizationResult],!*VSt)
childVisualizations fx mx dp children masks vers vst = childVisualizations` 0 children masks vers [] vst
where
	childVisualizations` i [] [] [] acc vst
		= (reverse acc,vst)
	childVisualizations` i [child:children] [mask:masks] [ver:vers] acc vst
		# (childV,vst) = fx (dp ++ [i]) (child,mask,ver) (mx child) vst
		= childVisualizations` (i + 1) children masks vers [childV:acc] vst

verifyValue :: !a -> Verification | gVerify{|*|} a
verifyValue val = verifyMaskedValue (val,Touched)
	
verifyMaskedValue :: !(MaskedValue a) -> Verification | gVerify{|*|} a
verifyMaskedValue mv = gVerify{|*|} {VerifyOptions|optional = False, disabled = False} mv 

isValid :: !Verification -> Bool
isValid (CorrectValue _) = True
isValid (WarningValue _) = True
isValid (CompoundVerification vs) = foldr (\v t -> t && isValid v) True vs 
isValid _ = False

alwaysValid :: !(MaskedValue a) -> Verification
alwaysValid _ = CorrectValue Nothing

simpleVerify :: !VerifyOptions !(MaskedValue a) -> Verification
simpleVerify options mv = customVerify (const True) (const undef) options mv

customVerify :: !(a -> Bool) !(a -> String) !VerifyOptions (MaskedValue a) -> Verification
customVerify pred mkErrMsg options=:{VerifyOptions|optional,disabled} (val,mask)
	= case mask of
		Untouched				= if optional (CorrectValue Nothing) MissingValue
		Touched					= validateValue val
		TouchedWithState s		= validateValue val		
		TouchedUnparsed r		= UnparsableValue
		Blanked					= if optional (CorrectValue Nothing) MissingValue
		CompoundMask _		    = validateValue val
where
	validateValue val
		| pred val	= CorrectValue Nothing
		| otherwise	= IncorrectValue(mkErrMsg val)

basicUpdate :: !(upd a -> Maybe a) !DataPath !JSONNode !(MaskedValue a) !*USt -> (!MaskedValue a,!*USt) | JSONDecode{|*|} upd
basicUpdate toV target upd (v,vmask) ust
	| isEmpty target
        # mbV   = maybe Nothing (\u -> toV u v) (fromJSON upd)
        # v     = fromMaybe v mbV
        # vmask = if (upd === JSONNull) Blanked (if (isNothing mbV) (TouchedUnparsed upd) Touched)
        = ((v,vmask),ust)
	| otherwise
		= ((v,vmask),ust)

basicUpdateSimple :: !DataPath !JSONNode !(MaskedValue a) !*USt -> (!MaskedValue a,!*USt) | JSONDecode{|*|} a
basicUpdateSimple target upd val iworld = basicUpdate (\json old -> fromJSON json) target upd val iworld
