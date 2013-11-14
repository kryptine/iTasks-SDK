implementation module iTasks.Framework.Generic.Interaction

import StdList, StdBool, StdTuple, StdFunc, StdMisc
import Data.Maybe, Data.Either, Data.Map, Data.Generic, Data.Functor, Data.Tuple
import Text, Text.JSON
import iTasks.Framework.IWorld
import iTasks.Framework.UIDefinition
import iTasks.Framework.Util
import iTasks.API.Core.LayoutCombinators

visualizeAsEditor :: !(VerifiedValue a) !TaskId !LayoutRules !*IWorld -> (![(!UIControl,!UIAttributes)],!*IWorld) | gEditor{|*|} a & gEditMeta{|*|} a
visualizeAsEditor (v,mask,ver) taskId layout iworld
	# vst = {VSt| selectedConsIndex = -1, optional = False, disabled = False, taskId = toString taskId, layout = layout, iworld = iworld}
	# (res,vst=:{VSt|iworld})	= gEditor{|*|} [] (v,mask,ver) (gEditMeta{|*|} v) vst
	= (controlsOf res,iworld)

updateValueAndMask :: !DataPath !JSONNode !(MaskedValue a) -> MaskedValue a | gUpdate{|*|} a
updateValueAndMask path update (a,mask) = gUpdate{|*|} path update (a,mask)

//Generic visualizer
generic gEditor a | gVisualizeText a, gDefault a, gEditMeta a, JSONEncode a, JSONDecode a
				   :: !DataPath !(VerifiedValue a) ![EditMeta] !*VSt -> (!VisualizationResult,!*VSt)

gEditor{|UNIT|} dp _ _ vst = (NormalEditor [],vst)

gEditor{|EITHER|} fx _ _ mx _ _ fy _ _ my _ _ dp (LEFT x,mask,ver) meta vst = fx dp (x,mask,ver) (mx x) vst
gEditor{|EITHER|} fx _ _ mx _ _ fy _ _ my _ _ dp (RIGHT y,mask,ver) meta vst =  fy dp (y,mask,ver) (my y) vst	

gEditor{|RECORD of {grd_arity}|} fx _ _ mx _ _ dp (RECORD x,mask,ver) meta vst=:{VSt|optional,disabled,taskId}
	//When optional and no value yet, just show the checkbox
	| optional &&  not (isTouched mask)
		= if disabled (OptionalEditor [],vst) (OptionalEditor [checkbox False], vst)
	# (fieldsViz,vst) = fx (pairPath grd_arity dp) (x,toPairMask grd_arity mask,toPairVerification grd_arity ver) (mx x) {VSt|vst & optional = False}
	//For optional records we add the checkbox to clear the entire record
	# viz = if (optional && not disabled) (OptionalEditor [checkbox True:controlsOf fieldsViz]) fieldsViz	
	= (viz,vst)
where
	checkbox checked = (UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId = taskId, editorId = editorId dp, value = Just (JSONBool checked)},newMap)

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
		# (items, vst=:{selectedConsIndex}) = fx dp (x,mask,ver) meta vst
        # (controls,choice) = case mask of
            Untouched   = ([],[])
            Blanked     = ([],[])
            _           = (controlsOf items,[selectedConsIndex])
		# content	= layout.layoutSubEditor {UIControlStack|attributes = newMap, controls = controls}
		= (NormalEditor [(UIDropdown defaultSizeOpts
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
				= NormalEditor (layout.layoutSubEditor {UIControlStack|attributes = newMap, controls = items})
			OptionalEditor items
				= OptionalEditor (layout.layoutSubEditor {UIControlStack|attributes = newMap, controls = items})
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
        = (NormalEditor [(UIEditInt defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|Real|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled	
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|Char|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled	
    	= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditString defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|String|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
    	= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value= checkMask mask val},newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditString defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

gEditor{|Bool|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled} //Bools are shown as optional by default, because a mandatory bool makes little sense
	| disabled		
		= (OptionalEditor [(UIViewCheckbox defaultSizeOpts {UIViewOpts|value =checkMask mask val},newMap)],vst)
	| otherwise	
		= (OptionalEditor [(UIEditCheckbox defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes vv meta)],vst)

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
		# controls	= map fst (layout.layoutSubEditor {UIControlStack| attributes = newMap, controls = controlsOf item})
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

derive gEditor JSONNode, Either, (,,), (,,,), Timestamp, Map //TODO Make specializations for (,,) and (,,,)

generic gEditMeta a :: a -> [EditMeta]

gEditMeta{|UNIT|} _			= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|PAIR|} fx fy _	= fx undef ++ fy undef
gEditMeta{|EITHER|} fx fy _	= fx undef //Only consider first constructor
gEditMeta{|OBJECT|} fx _	= fx undef
gEditMeta{|CONS|} fx _		= fx undef
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
gEditMeta{|[]|} fx _		= fx undef

derive gEditMeta (,), (,,), (,,,), Either, Void, Map, JSONNode, Timestamp

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
		
gVerify{|(->)|} _ _ _ mv	= alwaysValid mv
gVerify{|Dynamic|}	_ mv	= alwaysValid mv

gVerify{|HtmlTag|} _ mv = alwaysValid mv
gVerify{|JSONNode|} _ mv = alwaysValid mv

derive gVerify (,), (,,), (,,,), Void, Either, Timestamp, Map

//Generic updater
generic gUpdate a | gDefault a, JSONDecode a :: !DataPath !JSONNode !(MaskedValue a) -> (MaskedValue a)

gUpdate{|UNIT|} _ _ val = val

gUpdate{|PAIR|} gUpdx gDefx jDecx gUpdy gDefy jDecy [0:target] upd (PAIR x y, xmask)
	# (x,xmask) = gUpdx target upd (x,xmask)
	= (PAIR x y,xmask)
gUpdate{|PAIR|} gUpdx gDefx jDecx gUpdy gDefy jDecy [1:target] upd (PAIR x y, ymask)
	# (y,ymask) = gUpdy target upd (y,ymask)
	= (PAIR x y,ymask)
gUpdate{|PAIR|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd val = val

gUpdate{|EITHER|} gUpdx gDefx jDecx gUpdy gDefy jDecy [t:ts] upd (either,mask)
    | t == -1 = case ts of
        [] = (LEFT (gDefx), Untouched)
        _  = appFst LEFT (gUpdx ts upd (gDefx,Untouched))
    | t == -2 = case ts of
        [] = (RIGHT (gDefy), Untouched)
        _  = appFst RIGHT (gUpdy ts upd (gDefy,Untouched))
    | otherwise
        = case either of
            (LEFT x)  = appFst LEFT (gUpdx [t:ts] upd (x,mask))
            (RIGHT y) = appFst RIGHT (gUpdy [t:ts] upd (y,mask))

gUpdate{|OBJECT of {gtd_num_conses,gtd_conses}|} gUpdx gDefx jDecx [] upd (OBJECT x,mask) //Update is a constructor switch
	# consIdx = case upd of
		JSONInt i	= i
		_			= 0
	# mask	        = case upd of
		JSONNull	= Blanked	//Reset
		_			= CompoundMask (repeatn (gtd_conses !! consIdx).gcd_arity Untouched)
    # (x,_)         = gUpdx (updConsPath (if (consIdx < gtd_num_conses) consIdx 0) gtd_num_conses) upd (x,mask)
	= (OBJECT x, mask)

gUpdate{|OBJECT|} gUpdx gDefx jDecx target upd (OBJECT object, mask) //Update is targeted somewhere in a substructure of this value
	= appFst OBJECT (gUpdx target upd (object,mask))

gUpdate{|CONS of {gcd_arity,gcd_index}|} gUpdx gDefx jDecx [index:target] upd (CONS cons,mask)
	| index >= gcd_arity
		= (CONS cons,mask)	
	# childMasks = subMasks gcd_arity mask
	# (cons,targetMask) = gUpdx (updPairPath index gcd_arity ++ target) upd (cons,childMasks !! index)
	= (CONS cons,CompoundMask (updateAt index targetMask childMasks))
gUpdate{|CONS|} gUpdx gDefx jDecx target upd val = val

gUpdate{|RECORD of {grd_arity}|} gUpdx gDefx jDecx [index:target] upd (RECORD record,mask)
	| index >= grd_arity
		= (RECORD record,mask)
	# childMasks = subMasks grd_arity mask
	# (record,targetMask) = gUpdx (updPairPath index grd_arity ++ target) upd (record,childMasks !! index)
	= (RECORD record,CompoundMask (updateAt index targetMask childMasks))

gUpdate{|RECORD|} gUpdx gDefx jDecx _ _ val = val
	
gUpdate{|FIELD|} gUpdx gDefx jDecx target upd (FIELD field,mask)= appFst FIELD (gUpdx target upd (field,mask))

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

gUpdate{|Int|}		target upd val = basicUpdateSimple target upd val
gUpdate{|Real|}		target upd val = basicUpdateSimple target upd val
gUpdate{|Char|}		target upd val = basicUpdateSimple target upd val
gUpdate{|Bool|}		target upd val = basicUpdateSimple target upd val
gUpdate{|String|}	target upd val = basicUpdateSimple target upd val
			
gUpdate{|Maybe|} gUpdx gDefx jDecx target upd (m,mmask)
	| isEmpty target && (upd === JSONNull || upd === JSONBool False)
		= (Nothing, Blanked) //Reset
	| otherwise
		= case m of
			Nothing
				// Create a default value
				# x  	= gDefx
				// Search in the default value
				# (x,mmask)	= gUpdx target upd (x,Untouched)
				= (Just x, mmask)
			Just x
				= appFst Just (gUpdx target upd (x,mmask))

gUpdate{|[]|} gUpdx gDefx jDecx target upd (l,listMask)
	# (l,childMasks)
		= case ((not (isEmpty target)) && (hd target >= (length l))) of
			True
				= (l++[gDefx], subMasks (length l) listMask ++ [Untouched])
			False
				= (l, subMasks (length l) listMask)
	# (l,childMasks)	= updateElements gUpdx target upd l childMasks
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
		= (l,CompoundMask childMasks)
	| otherwise
		= (l,CompoundMask childMasks)
where
	updateElements fx [i:target] upd elems masks
		| i >= (length elems)
			= (elems,masks)
		# (nx,nm)	= fx target upd (elems !! i,masks !! i)
		= (updateAt i nx elems, updateAt i nm masks) 
	updateElements fx target upd elems masks
		= (elems,masks)
	
	swap []	  _		= []
	swap list index
		| index == 0 			= list //prevent move first element up
		| index >= length list 	= list //prevent move last element down
		| otherwise				
			# f = list !! (index-1)
			# l = list !! (index)
			= updateAt (index-1) l (updateAt index f list)

/*
gUpdate{|(,)|} gUpdx gDefx jDecx gUpdy gDefy jDecy [0:target] upd ((x,y), mask)
    # [xmask,ymask:_] = subMasks 2 mask
	# (x,xmask) = gUpdx target upd (x,xmask)
    = ((x,y),CompoundMask [xmask,ymask])
gUpdate{|(,)|} gUpdx gDefx jDecx gUpdy gDefy jDecy [1:target] upd ((x,y), mask)
    # [xmask,ymask:_] = subMasks 2 mask
	# (y,ymask) = gUpdy target upd (y,ymask)
    = ((x,y),CompoundMask [xmask,ymask])
gUpdate{|(,)|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd ((x,y), mask)
    = ((x,y), mask)
*/
	
gUpdate{|Dynamic|}		target upd val = basicUpdate (\Void v -> Just v) target upd val
gUpdate{|(->)|} _ _ gUpdy _ _ _ target upd val = basicUpdate (\Void v -> Just v) target upd val

gUpdate{|HtmlTag|} target upd val = val

derive gUpdate Either, (,), (,,), (,,,), JSONNode, Void, Timestamp, Map

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
addLabel optional label attr = put LABEL_ATTRIBUTE (format optional label) attr
where
	format optional label = camelCaseToWords label +++ if optional "" "*" +++ ":" //TODO: Move to layout

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

basicUpdate :: !(upd a -> Maybe a) !DataPath !JSONNode !(MaskedValue a) -> MaskedValue a | JSONDecode{|*|} upd
basicUpdate toV target upd (v,vmask)
	| isEmpty target
        # mbV   = maybe Nothing (\u -> toV u v) (fromJSON upd)
        # v     = fromMaybe v mbV
        # vmask = if (upd === JSONNull) Blanked (if (isNothing mbV) (TouchedUnparsed upd) Touched)
        = (v,vmask)
	| otherwise
		= (v,vmask)

basicUpdateSimple :: !DataPath !JSONNode !(MaskedValue a) -> MaskedValue a | JSONDecode{|*|} a
basicUpdateSimple target upd val = basicUpdate (\json old -> fromJSON json) target upd val
