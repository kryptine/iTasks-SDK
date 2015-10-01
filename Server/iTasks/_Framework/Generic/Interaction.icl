implementation module iTasks._Framework.Generic.Interaction

from Data.Map import :: Map
import qualified Data.Map as DM
from StdFunc import const
import StdList, StdBool, StdTuple, StdMisc
import Data.Maybe, Data.Either, Data.Error, Data.Generic, Data.Functor, Data.Tuple
import Text, Text.JSON
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import iTasks._Framework.Util
import iTasks.API.Core.Types
import iTasks.UI.Layout
import iTasks.UI.Editor, iTasks.UI.Diff

generic gEditor a | gText a, gDefault a, gEditMeta a, JSONEncode a, JSONDecode a :: Editor a
derive bimap Editor,(,,),(,,,)

gEditor{|UNIT|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp _ _ _ _ vst = (NormalEditor [],vst)
	genDiff up _ _ vst = (DiffPossible [],vst)
	appDiff dp e val mask ust = (val,mask,ust)

gEditor{|EITHER|} ex _ dx mx _ _ ey _ dy my _ _  = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (LEFT x) mask ver meta vst = ex.Editor.genUI dp x mask ver (mx x) vst
	genUI dp (RIGHT y) mask ver meta vst =  ey.Editor.genUI dp y mask ver (my y) vst	

	genDiff up (LEFT old) (LEFT new) vst = ex.Editor.genDiff up old new vst
	genDiff up (RIGHT old) (RIGHT new) vst = ey.Editor.genDiff up old new vst
	genDiff up _ _ vst = (DiffImpossible, vst)

	appDiff [d:ds] e either mask ust
		| d == -1 = case ds of
        	[] = (LEFT dx, Untouched, ust)
			_ 
				# (x,mask,ust) = ex.Editor.appDiff ds e dx Untouched ust
				= (LEFT x, mask, ust)
		| d == -2 = case ds of
			[] = (RIGHT dy, Untouched, ust)
			_ 
				# (y,mask,ust) = ey.Editor.appDiff ds e dy Untouched ust
				= (RIGHT y, mask, ust)
		| otherwise
			= case either of
				(LEFT x)
					# (x,mask,ust) = ex.Editor.appDiff [d:ds] e x mask ust
					= (LEFT x, mask, ust)
				(RIGHT y)
					# (y,mask,ust) = ey.Editor.appDiff [d:ds] e y mask ust
					= (RIGHT y, mask, ust)

gEditor{|RECORD of {grd_arity}|} ex _ _ mx _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (RECORD x) mask ver meta vst=:{VSt|optional,disabled,taskId}
		//When optional and no value yet, just show the checkbox
		| optional && not (isTouched mask)
			= if disabled (OptionalEditor [],vst) (OptionalEditor [checkbox False], vst)
		# (fieldsViz,vst) = ex.Editor.genUI (pairPath grd_arity dp) x (toPairMask grd_arity mask) (toPairVerification grd_arity ver) (mx x) {VSt|vst & optional = False}
		//For optional records we add the checkbox to clear the entire record
		# viz = if (optional && not disabled) (OptionalEditor [checkbox True:editorControls fieldsViz]) fieldsViz	
		= (viz,vst)
	where
		checkbox checked = (UIEditCheckbox defaultFSizeOpts {UIEditOpts|taskId = taskId, editorId = editorId dp, value = Just (JSONBool checked)},'DM'.newMap)

	genDiff up (RECORD old) (RECORD new) vst = ex.Editor.genDiff up old new vst

	appDiff [] e (RECORD record) mask ust
    	# mask = case e of
        	JSONBool False  = Blanked
        	_               = Touched
    	= (RECORD record,mask,ust)
	appDiff [d:ds] e (RECORD record) mask ust
		| d >= grd_arity
			= (RECORD record,mask,ust)
		# childMasks = subMasks grd_arity mask
		# (record,targetMask,ust) = ex.Editor.appDiff (updPairPath d grd_arity ++ ds) e record (childMasks !! d) ust
		= (RECORD record,CompoundMask (updateAt d targetMask childMasks),ust)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|FIELD of {gfd_name}|} ex _ _ mx _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (FIELD x) mask ver _ vst=:{VSt|disabled}
		# (vizBody,vst)		= ex.Editor.genUI dp x mask ver (mx x) vst
		= case vizBody of
			HiddenEditor			= (HiddenEditor,vst)
			NormalEditor controls
				= (NormalEditor [(c,addLabel disabled gfd_name a) \\ (c,a) <- controls],vst)
			OptionalEditor controls	
				= (OptionalEditor [(c,addLabel True gfd_name a) \\ (c,a) <- controls], vst)

	genDiff up (FIELD old) (FIELD new) vst = ex.Editor.genDiff up old new vst

	appDiff dp e (FIELD field) mask ust
		# (field,mask,ust) = ex.Editor.appDiff dp e field mask ust
		= (FIELD field,mask,ust)

gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} ex _ _ mx _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (OBJECT x) mask ver meta vst=:{selectedConsIndex = oldSelectedConsIndex,disabled,taskId,layout}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		# (items, vst=:{selectedConsIndex}) = ex.Editor.genUI dp x mask ver meta {VSt|vst & optional=False}
        # (controls,choice) = case mask of
            Untouched   = ([],[])
            Blanked     = ([],[])
            _           = (editorControls items,[selectedConsIndex])
		# content	= layout.layoutSubEditor {UIForm|attributes = 'DM'.newMap, controls = controls,size = defaultSizeOpts}
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
	//ADT with one constructor or static render: 'DM'.put content into container, if empty show cons name
	| otherwise
		# (vis,vst) = ex.Editor.genUI dp x mask ver meta vst
		# vis = case vis of
			HiddenEditor 	= HiddenEditor
			NormalEditor []
                = NormalEditor [(stringDisplay (if (isTouched mask) (gtd_conses !! vst.selectedConsIndex).gcd_name ""),'DM'.newMap)]
				//= if (isTouched mask) (NormalEditor [((stringDisplay ((gtd_conses !! vst.selectedConsIndex).gcd_name)),'DM'.newMap)]) (NormalEditor [])			
			NormalEditor items
				= NormalEditor (layout.layoutSubEditor {UIForm|attributes = 'DM'.newMap, controls = items, size = defaultSizeOpts})
			OptionalEditor items
				= OptionalEditor (layout.layoutSubEditor {UIForm|attributes = 'DM'.newMap, controls = items, size = defaultSizeOpts})
		= (vis,{vst & selectedConsIndex = oldSelectedConsIndex})

	genDiff up (OBJECT old) (OBJECT new) vst = ex.Editor.genDiff up old new vst

	appDiff [] e (OBJECT val) mask ust //Update is a constructor switch
		# consIdx = case e of
			JSONInt i	= i
			_			= 0
		# mask	        = case e of
			JSONNull	= Blanked	//Reset
			_			= CompoundMask (repeatn (gtd_conses !! consIdx).gcd_arity Untouched)
    	# (val,_,ust)	= ex.Editor.appDiff (updConsPath (if (consIdx < gtd_num_conses) consIdx 0) gtd_num_conses) e val mask ust
		= (OBJECT val, mask, ust)
	appDiff dp e (OBJECT val) mask ust //Update is targeted somewhere in a substructure of this value
		# (val,mask,ust) = ex.Editor.appDiff dp e val mask ust
		= (OBJECT val,mask,ust)

gEditor{|CONS of {gcd_index,gcd_arity}|} ex _ _ mx _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (CONS x) mask ver meta vst=:{VSt|taskId,optional,disabled}
		# (viz,vst)	= ex.Editor.genUI (pairPath gcd_arity dp) x (toPairMask gcd_arity mask) (toPairVerification gcd_arity ver) meta vst
    	= (viz,{VSt | vst & selectedConsIndex = gcd_index})

	genDiff up (CONS old) (CONS new) vst = ex.Editor.genDiff up old new vst

	appDiff [d:ds] e (CONS val) mask ust
		| d >= gcd_arity
			= (CONS val,mask,ust)	
		# childMasks = subMasks gcd_arity mask
		# (val,targetMask,ust) = ex.Editor.appDiff (updPairPath d gcd_arity ++ ds) e val (childMasks !! d) ust
		= (CONS val,CompoundMask (updateAt d targetMask childMasks),ust)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|PAIR|} ex _ _ mx _ _ ey _ _ my _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (PAIR x y) (CompoundMask [xmask,ymask]) (CompoundVerification [xver,yver]) meta vst
	# (dpx,dpy)		= pairPathSplit dp
	# (vizx, vst)	= ex.Editor.genUI dpx x xmask xver (mx x) vst
	# (vizy, vst)	= ey.Editor.genUI dpy y ymask yver (my y) vst
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

	genDiff up (PAIR oldx oldy) (PAIR newx newy) vst
		# (diffx,vst) = ex.Editor.genDiff up oldx newx vst
		# (diffy,vst) = ey.Editor.genDiff up oldy newy vst
		= case (diffx,diffy) of
			(DiffPossible x,DiffPossible y) = (DiffPossible (x ++ y),vst)
			_ 								= (DiffImpossible,vst)

	appDiff [0:ds] e (PAIR x y) xmask ust
		# (x,xmask,ust) = ex.Editor.appDiff ds e x xmask ust
		= (PAIR x y,xmask,ust)
	appDiff [1:ds] e (PAIR x y) ymask ust
		# (y,ymask,ust) = ey.Editor.appDiff ds e y ymask ust
		= (PAIR x y,ymask,ust)
	appDiff _ _ val mask ust = (val,mask,ust)

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

gEditor{|Int|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where 
	genUI dp val mask ver meta vst=:{VSt|taskId,disabled}
	| disabled	
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},'DM'.newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditInt defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes (val,mask,ver) meta)],vst)

	genDiff up old new vst=:{VSt|disabled}
		= (DiffPossible (if (old === new) [] [UIUpdate up [(if disabled "setValue" "setEditorValue",[encodeUIValue new])]]),vst)

	appDiff dp e val mask ust = basicUpdateSimple dp e val mask ust

gEditor{|Real|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp val mask ver meta vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},'DM'.newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditDecimal defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes (val,mask,ver) meta)],vst)

	genDiff up old new vst=:{VSt|disabled}
		= (DiffPossible (if (old === new) [] [UIUpdate up [(if disabled "setValue" "setEditorValue",[encodeUIValue new])]]),vst)

	appDiff dp e val mask ust = basicUpdateSimple dp e val mask ust

gEditor{|Char|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp val mask ver meta vst=:{VSt|taskId,disabled}
	| disabled
    	= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString (checkMask mask val)},'DM'.newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditString defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes (val,mask,ver) meta)],vst)

	genDiff up old new vst=:{VSt|disabled}
		= (DiffPossible (if (old === new) [] [UIUpdate up [(if disabled "setValue" "setEditorValue",[encodeUIValue (toString new)])]]),vst)

	appDiff dp e val mask ust = basicUpdateSimple dp e val mask ust

gEditor{|String|} = {Editor|genUI=genUI,genDiff,appDiff=appDiff}
where
	genUI dp val mask ver meta vst=:{VSt|taskId,disabled}
	| disabled
    	= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value= checkMask mask val},'DM'.newMap)],vst)
	| otherwise
        = (NormalEditor [(UIEditString defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes (val,mask,ver) meta)],vst)
	genDiff up old new vst=:{VSt|disabled}
		= (DiffPossible (if (old === new) [] [UIUpdate up [(if disabled "setValue" "setEditorValue",[encodeUIValue new])]]),vst)

	appDiff dp e val mask ust = basicUpdateSimple dp e val mask ust

gEditor{|Bool|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp val mask ver meta vst=:{VSt|taskId,disabled} //Bools are shown as optional by default, because a mandatory bool makes little sense
	| disabled		
		= (OptionalEditor [(UIViewCheckbox defaultFSizeOpts {UIViewOpts|value =checkMask mask val},'DM'.newMap)],vst)
	| otherwise	
		= (OptionalEditor [(UIEditCheckbox defaultFSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=checkMaskValue mask val},editorAttributes (val,mask,ver) meta)],vst)

	genDiff up old new vst=:{VSt|disabled}
		= (DiffPossible (if (old === new) [] [UIUpdate up [(if disabled "setValue" "setEditorValue",[encodeUIValue new])]]),vst)

	appDiff dp e val mask ust = basicUpdateSimple dp e val mask ust

gEditor{|[]|} ex _ dx mx _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff} 
where 
	genUI dp val mask ver meta vst=:{VSt|taskId,disabled,layout}
	# (items,vst)	= listControl dp val (subMasks (length val) mask) (subVerifications (length val) ver) vst
	= (NormalEditor [(listContainer items,'DM'.newMap)],vst)
	where
			listControl dp items masks vers vst=:{VSt|optional,disabled}
				# (itemsVis,vst)	= childVisualizations2 ex.Editor.genUI mx dp items masks vers vst
				# numItems = length items
				| disabled
					= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
				| otherwise
					//# (newItem,vst)		= newChildVisualization fx True vst
					//= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [newItemControl newItem],vst)
					= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
								
			listItemControl disabled numItems idx item
				# controls	= decorateControls (layout.layoutSubEditor {UIForm| attributes = 'DM'.newMap, controls = editorControls item, size = defaultSizeOpts})
				# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mup_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=idx == 0}
							  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mdn_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= idx == numItems - 1}
							  ,UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("rem_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=False}
							  ]
				= setHeight WrapSize (setDirection Horizontal (defaultContainer (if disabled controls (controls ++ buttons))))
					
			addItemControl numItems
				# controls	= [UIViewString /*{*/defaultSizeOpts /* & width=Just FlexSize }*/ {UIViewOpts|value= Just (numItemsText numItems)}]
				# buttons	= [UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString "add")} {UIButtonOpts|text=Nothing,iconCls=Just "icon-add",disabled=False}]
				= setHeight WrapSize (setDirection Horizontal (defaultContainer (controls ++ buttons)))
			
			listContainer items
				= setHeight WrapSize (defaultContainer items)
					
			numItemsText 1 = "1 item"
			numItemsText n = toString n +++ " items"

	genDiff up old new vst = (DiffImpossible,vst)

	appDiff dp e l listMask ust
		# (l,childMasks) = case ((not (isEmpty dp)) && (hd dp >= (length l))) of
			True
				= (l++[dx], subMasks (length l) listMask ++ [Untouched])
			False
				= (l, subMasks (length l) listMask)
		# (l,childMasks,ust) = updateElements ex.Editor.appDiff dp e l childMasks ust
		| isEmpty dp
			//Process the reordering commands
			# split = split "_" (fromMaybe "" (fromJSON e))
			# index = toInt (last split)
			# (l,childMasks) = case hd split of	
				"mup" = (swap l index,swap childMasks index) 
				"mdn" = (swap l (index+1),swap childMasks (index+1))
				"rem" = (removeAt index l,removeAt index childMasks)	
				"add"
					= (insertAt (length l) dx l, insertAt (length l) Untouched childMasks)
				_	
					= (l,childMasks)
			= (l,CompoundMask childMasks,ust)
		| otherwise
			= (l,CompoundMask childMasks,ust)
	where
		updateElements fx [i:target] upd elems masks ust
			| i >= (length elems)
				= (elems,masks,ust)
				# (nx,nm,ust)	= fx target upd (elems !! i) (masks !! i) ust
				= (updateAt i nx elems, updateAt i nm masks,ust)
		updateElements fx target upd elems masks ust
			= (elems,masks,ust)
			
swap []	  _		= []
swap list index
	| index == 0 			= list //prevent move first element up
	| index >= length list 	= list //prevent move last element down
	| otherwise				
		# f = list !! (index-1)
		# l = list !! (index)
		= updateAt (index-1) l (updateAt index f list)

gEditor{|EditableList|} ex _ dx mx _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp {EditableList|items,add,remove,reorder,count} mask ver meta vst=:{VSt|taskId,disabled,layout}
		# (controls,vst) = listControls dp items (subMasks (length items) mask) (subVerifications (length items) ver) vst
		= (NormalEditor [(listContainer controls,'DM'.newMap)],vst)
	where
			enableAdd = case add of ELNoAdd = False ; _ = True;

			listControls dp items masks vers vst=:{VSt|optional,disabled}
				# (itemsVis,vst)	= childVisualizations2 ex.Editor.genUI mx dp items masks vers vst
				# numItems = length items
				| not disabled && (enableAdd || count)
					= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
				| otherwise
					= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
								
			listItemControl disabled numItems idx item
				# controls	= map (setWidth FlexSize) (decorateControls (layout.layoutSubEditor {UIForm| attributes = 'DM'.newMap, controls = editorControls item, size = defaultSizeOpts}))
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

	genDiff up old new vst = (DiffImpossible,vst)

	appDiff dp e l=:{EditableList|items,add,remove,reorder} listMask ust
		# (items,childMasks)
			= case ((not (isEmpty dp)) && (hd dp >= (length items))) of
				True
					= (items++[dx], subMasks (length items) listMask ++ [Untouched])
				False
					= (items, subMasks (length items) listMask)
		# (items,childMasks,ust) = updateElements ex.Editor.appDiff dp e items childMasks ust
		| isEmpty dp
			//Process the reordering commands
			# split = split "_" (fromMaybe "" (fromJSON e))
			# index = toInt (last split)
			# (items,childMasks) = case hd split of	
				"mup" = if reorder (swap items index,swap childMasks index) (items,childMasks)
				"mdn" = if reorder (swap items (index+1),swap childMasks (index+1)) (items,childMasks)
				"rem" = if remove  (removeAt index items,removeAt index childMasks)	(items,childMasks)
				"add"
                	= case add of
                    	ELAddBlank      = (insertAt (length items) dx items, insertAt (length items) Untouched childMasks)
                    	ELAddValue f    = (insertAt (length items) (f items) items, insertAt (length items) Touched childMasks)
                    	_               = (items,childMasks)
				_	
					= (items,childMasks)
			= ({EditableList|l & items = items},CompoundMask childMasks,ust)
		| otherwise
			= ({EditableList|l & items = items},CompoundMask childMasks,ust)
	where
		updateElements fx [i:target] upd elems masks ust
			| i >= (length elems)
				= (elems,masks,ust)
			# (nx,nm,ust)	= fx target upd (elems !! i) (masks !! i) ust
			= (updateAt i nx elems, updateAt i nm masks,ust)
		updateElements fx target upd elems masks ust
			= (elems,masks,ust)
	
gEditor{|()|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp _ _ _ meta vst = (HiddenEditor,vst)
	genDiff up _ _ vst = (DiffPossible [],vst)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|(,)|} ex _ _ mx _ _ ey _ _ my _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (x,y) mask ver meta vst
		# (vizx, vst)	= ex.Editor.genUI (dp ++ [0]) x (subMasks 2 mask !! 0) (subVerifications 2 ver !! 0) (mx x) vst
		# (vizy, vst)	= ey.Editor.genUI (dp ++ [1]) y (subMasks 2 mask !! 1) (subVerifications 2 ver !! 1) (my y) vst
		# viz = case (vizx,vizy) of
			(HiddenEditor,HiddenEditor) = HiddenEditor
			_							= NormalEditor (editorControls vizx ++ editorControls vizy)
	= (viz, vst)

	genDiff up old new vst = (DiffImpossible,vst)

	appDiff [0:ds] e (x,y) xmask ust
		# (x,xmask,ust) = ex.Editor.appDiff ds e x xmask ust
		= ((x,y),xmask,ust)
	appDiff [1:ds] e (x,y) ymask ust
		# (y,ymask,ust) = ey.Editor.appDiff ds e y ymask ust
		= ((x,y),ymask,ust)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|(->)|} _ _ _ _ _ _ _ _ _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ _ vst = (HiddenEditor,vst)
	genDiff up _ _ vst = (DiffPossible [],vst)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|Dynamic|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ _ vst = (HiddenEditor,vst)
	genDiff up _ _ vst = (DiffPossible [],vst)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|Maybe|} ex _ dx mx _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp val mask ver meta vst=:{VSt|optional,disabled}
	# (viz,vst) = case val of
		(Just x)	= ex.Editor.genUI dp x mask ver (mx x) {VSt|vst & optional = True}
		_			= ex.Editor.genUI dp dx Untouched ver (mx dx) {VSt|vst & optional = True}
	= (toOptional viz, {VSt|vst & optional = optional})
	where
		toOptional	(NormalEditor ex)	= OptionalEditor ex
		toOptional	viz					= viz

	genDiff up old new vst = (DiffImpossible,vst)

	appDiff dp e val mask ust
		| isEmpty dp && (e === JSONNull || e === JSONBool False)
			= (Nothing, Blanked,ust) //Reset
		| otherwise
			# (x,xmask) = maybe (dx,Untouched) (\x -> (x,mask)) val
			# (x,xmask,ust) = ex.Editor.appDiff dp e x xmask ust
			= (Just x,xmask,ust)

gEditor{|HtmlTag|}	= {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp val mask ver meta vst
		= (NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just val},'DM'.newMap)], vst)

	genDiff up old new vst = (DiffPossible [], vst)

	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|RWShared|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ _ vst = (HiddenEditor,vst)

	genDiff up old new vst = (DiffPossible [], vst)
	appDiff _ _ val mask ust = (val,mask,ust)

derive gEditor JSONNode, Either, MaybeError, (,,), (,,,), (,,,,), Timestamp, Map //TODO Make specializations for (,,) and (,,,)

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

gEditMeta{|()|} _                    = []
gEditMeta{|(,)|} fa fb _             = fa undef ++ fb undef
gEditMeta{|(,,)|} fa fb fc _         = fa undef ++ fb undef ++ fc undef
gEditMeta{|(,,,)|} fa fb fc fd _     = fa undef ++ fb undef ++ fc undef ++ fd undef
gEditMeta{|(,,,,)|} fa fb fc fd fe _ = fa undef ++ fb undef ++ fc undef ++ fd undef ++ fe undef
gEditMeta{|RWShared|} _ _ _ _ = [{label=Nothing,hint=Nothing,unit=Nothing}]

derive gEditMeta Either, MaybeError, Map, JSONNode, Timestamp, EditableListAdd

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

derive gVerify (,), (,,), (,,,), (,,,,), Either, MaybeError, Timestamp, Map

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
        [{EditMeta|unit=Just (Left unit)}:_]    = 'DM'.put PREFIX_ATTRIBUTE unit 'DM'.newMap
        [{EditMeta|unit=Just (Right unit)}:_]   = 'DM'.put POSTFIX_ATTRIBUTE unit 'DM'.newMap
        _                                       = 'DM'.newMap
    | isTouched mask = case ver of
        (CorrectValue msg)		= 'DM'.put VALID_ATTRIBUTE (fromMaybe "This value is ok" msg) attr
        (WarningValue msg)		= 'DM'.put WARNING_ATTRIBUTE msg attr
        (IncorrectValue msg)	= 'DM'.put ERROR_ATTRIBUTE msg attr
        (UnparsableValue)		= 'DM'.put ERROR_ATTRIBUTE "This value not in the required format" attr
        (MissingValue)			= 'DM'.put ERROR_ATTRIBUTE "This value is required" attr
        _						= attr
    | otherwise
        = maybe attr (\h -> 'DM'.put HINT_ATTRIBUTE h attr) hint

addLabel :: !Bool !String !UIAttributes -> UIAttributes
addLabel optional label attr = putCond LABEL_ATTRIBUTE (format optional label) attr
where
	format optional label = camelCaseToWords label +++ if optional "" "*" +++ ":" //TODO: Move to layout
    putCond k v m = maybe ('DM'.put k v m) (const m) ('DM'.get k m)

childVisualizations :: !(DataPath (VerifiedValue a) [EditMeta] -> .(*VSt -> *(!VisualizationResult,*VSt))) !(a -> [EditMeta]) !DataPath ![a] ![InteractionMask] ![Verification] !*VSt -> *(![VisualizationResult],!*VSt)
childVisualizations fx mx dp children masks vers vst = childVisualizations` 0 children masks vers [] vst
where
	childVisualizations` i [] [] [] acc vst
		= (reverse acc,vst)
	childVisualizations` i [child:children] [mask:masks] [ver:vers] acc vst
		# (childV,vst) = fx (dp ++ [i]) (child,mask,ver) (mx child) vst
		= childVisualizations` (i + 1) children masks vers [childV:acc] vst

childVisualizations2 :: !(DataPath a InteractionMask Verification  [EditMeta] -> .(*VSt -> *(!VisualizationResult,*VSt))) !(a -> [EditMeta]) !DataPath ![a] ![InteractionMask] ![Verification] !*VSt -> *(![VisualizationResult],!*VSt)
childVisualizations2 fx mx dp children masks vers vst = childVisualizations` 0 children masks vers [] vst
where
	childVisualizations` i [] [] [] acc vst
		= (reverse acc,vst)
	childVisualizations` i [child:children] [mask:masks] [ver:vers] acc vst
		# (childV,vst) = fx (dp ++ [i]) child mask ver (mx child) vst
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

basicUpdate :: !(upd a -> Maybe a) !DataPath !JSONNode !a !InteractionMask !*USt -> *(!a, !InteractionMask, !*USt) | JSONDecode{|*|} upd
basicUpdate toV target upd v vmask ust
	| isEmpty target
        # mbV   = maybe Nothing (\u -> toV u v) (fromJSON upd)
        # v     = fromMaybe v mbV
        # vmask = if (upd === JSONNull) Blanked (if (isNothing mbV) (TouchedUnparsed upd) Touched)
        = (v,vmask,ust)
	| otherwise
		= (v,vmask,ust)

basicUpdateSimple :: !DataPath !JSONNode !a !InteractionMask !*USt -> *(!a,!InteractionMask,!*USt) | JSONDecode{|*|} a
basicUpdateSimple target upd val mask iworld = basicUpdate (\json old -> fromJSON json) target upd val mask iworld
