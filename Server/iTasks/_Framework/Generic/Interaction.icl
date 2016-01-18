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

generic gEditor a | gText a, gDefault a, JSONEncode a, JSONDecode a :: Editor a
derive bimap Editor,(,,),(,,,)
derive gEq EditMask

gEditor{|UNIT|} = emptyEditor

gEditor{|RECORD of {grd_arity}|} ex _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (RECORD x) mask vst=:{VSt|optional,disabled,taskId}
		# (viz,vst) = ex.Editor.genUI (pairPath grd_arity dp) x (toPairMask grd_arity mask) {VSt|vst & optional = False}
		# viz = flattenPairUI grd_arity viz
		//When optional we add a checkbox show the checkbox
		| optional && not disabled
			= (uic (UIEditor {UIEditor|optional=True}) [checkbox (isTouched mask),viz], vst)
		| otherwise 
			= (viz,vst)
	where
		checkbox checked = uic (UIEditor {UIEditor|optional=True})
			[ui (UIEditCheckbox defaultFSizeOpts {UIEditOpts|taskId = taskId, editorId = editorId dp, value = Just (JSONBool checked)})]

	genDiff dp (RECORD old) om (RECORD new) nm vst 
		# (diff,vst) = ex.Editor.genDiff (pairPath grd_arity dp) old (toPairMask grd_arity om) new (toPairMask grd_arity nm) vst
		= (flattenPairDiff 0 grd_arity diff,vst)

	appDiff [] e (RECORD record) mask ust //Enabling or disabling of a record
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

gEditor{|FIELD of {gfd_name}|} ex _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (FIELD x) mask vst=:{VSt|disabled}
		# (viz,vst)		= ex.Editor.genUI dp x mask vst
		= case viz of
			//Add the field name as a label
			UI (UIEditor edit) attr ctrl
				= (UI (UIEditor edit) (addLabel gfd_name attr) ctrl,vst)
			def
				= (def,vst)

	genDiff dp (FIELD old) om (FIELD new) nm vst = ex.Editor.genDiff dp old om new nm vst

	appDiff dp e (FIELD field) mask ust
		# (field,mask,ust) = ex.Editor.appDiff dp e field mask ust
		= (FIELD field,mask,ust)

gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} ex _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (OBJECT x) mask vst=:{selectedConsIndex = curSelectedConsIndex,disabled,taskId}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		# (items, vst=:{selectedConsIndex}) = ex.Editor.genUI dp x mask {VSt|vst & optional=False}
        # choice = case mask of
            Untouched   = []
            Blanked     = []
            _           = [selectedConsIndex]
		| allConsesArityZero gtd_conses //If all constructors have arity 0, we only need the constructor dropwdown
			= (consDropdown choice, {vst & selectedConsIndex = curSelectedConsIndex})
		| otherwise
			= (uic (UIEditor {UIEditor|optional=False}) [consDropdown choice,items]
			, {vst & selectedConsIndex = curSelectedConsIndex})
	//ADT with one constructor or static render: 'DM'.put content into container, if empty show cons name
	| otherwise
		# (viz,vst) = ex.Editor.genUI dp x mask vst
		# viz = case viz of
			(UI UIEmpty _ _) = uic (UIEditor {UIEditor|optional=False}) [stringDisplay (if (isTouched mask) (gtd_conses !! vst.selectedConsIndex).gcd_name "")]
			_			= viz
		= (viz,{vst & selectedConsIndex = curSelectedConsIndex})
	where
		consDropdown choice = uiac (UIEditor 
			{UIEditor|optional=False})
				(attributes mask)
				[ui (UIDropdown defaultHSizeOpts
								{UIChoiceOpts
								| taskId = taskId
								, editorId = editorId dp
								, value = choice
								, options = [gdc.gcd_name \\ gdc <- gtd_conses]})]
		attributes mask
			| isTouched mask	= 'DM'.fromList[(HINT_TYPE_ATTRIBUTE,HINT_TYPE_VALID),(HINT_ATTRIBUTE, "You have correctly selected an option")]
								= 'DM'.fromList[(HINT_TYPE_ATTRIBUTE,HINT_TYPE_INFO),(HINT_ATTRIBUTE, "Select an option")]

	genDiff dp (OBJECT old) om (OBJECT new) nm vst=:{VSt|disabled,selectedConsIndex=curSelectedConsIndex}
		| gtd_num_conses > 1 && not disabled
			# (diff,vst=:{VSt|selectedConsIndex}) = ex.Editor.genDiff dp old om new nm {vst & selectedConsIndex = 0}
			| selectedConsIndex < 0 //A cons was changed
				# selectedCons = ~selectedConsIndex - 1
				# consChange = ChangeUI [("setValue",[toJSON selectedCons,JSONBool True])] []
				| allConsesArityZero gtd_conses
					= (consChange,{vst & selectedConsIndex = curSelectedConsIndex})
				| otherwise
					= (ChangeUI [] [ChangeChild 0 consChange,ChangeChild 1 diff],{vst & selectedConsIndex = curSelectedConsIndex})
			| otherwise
				= (diff,{vst & selectedConsIndex = curSelectedConsIndex})
		| otherwise
			= ex.Editor.genDiff dp old om new nm vst

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

	allConsesArityZero [] = True
	allConsesArityZero [{gcd_arity}:cs]
		| gcd_arity > 0 = False
						= allConsesArityZero cs

gEditor{|EITHER|} ex _ dx _ _ ey _ dy _ _  = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (LEFT x) mask vst = ex.Editor.genUI dp x mask vst
	genUI dp (RIGHT y) mask vst =  ey.Editor.genUI dp y mask vst	

	genDiff dp (LEFT old) om (LEFT new) nm vst = ex.Editor.genDiff dp old om new nm vst
	genDiff dp (RIGHT old) om (RIGHT new) nm vst = ey.Editor.genDiff dp old om new nm vst

	//A different constructor is selected -> generate a new UI
	//We use a negative selConsIndex to encode that the constructor was changed
	genDiff dp (LEFT old) om (RIGHT new) nm vst 
		# (viz,vst=:{selectedConsIndex}) = ey.Editor.genUI dp new nm vst
		= (ReplaceUI viz, {vst & selectedConsIndex = -1 - selectedConsIndex}) 

	genDiff dp (RIGHT old) om (LEFT new) nm vst 
		# (viz,vst=:{selectedConsIndex}) = ex.Editor.genUI dp new nm vst
		= (ReplaceUI viz, {vst & selectedConsIndex = -1 - selectedConsIndex})

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

gEditor{|CONS of {gcd_index,gcd_arity}|} ex _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (CONS x) mask vst=:{VSt|taskId,optional,disabled}
		# (viz,vst)	= ex.Editor.genUI (pairPath gcd_arity dp) x (toPairMask gcd_arity mask) vst
    	= (flattenPairUI gcd_arity viz,{VSt| vst & selectedConsIndex = gcd_index})

	genDiff dp (CONS old) om (CONS new) nm vst 
		//Diff all fields of the constructor
		# (diff,vst) = ex.Editor.genDiff (pairPath gcd_arity dp) old (toPairMask gcd_arity om) new (toPairMask gcd_arity nm) vst 	
		//Flatten the binary tree of ChangeUI constructors created from
		//the PAIR's into a single ChangeUI constructor
		= (flattenPairDiff 0 gcd_arity diff,vst)
																				
	appDiff [d:ds] e (CONS val) mask ust
		| d >= gcd_arity
			= (CONS val,mask,ust)	
		# childMasks = subMasks gcd_arity mask
		# (val,targetMask,ust) = ex.Editor.appDiff (updPairPath d gcd_arity ++ ds) e val (childMasks !! d) ust
		= (CONS val,CompoundMask (updateAt d targetMask childMasks),ust)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|PAIR|} ex _ _ _ _ ey _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (PAIR x y) mask vst
		# (xmask,ymask) = case mask of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (Untouched,Untouched)
		# (dpx,dpy)		= pairPathSplit dp
		# (vizx, vst)	= ex.Editor.genUI dpx x xmask vst
		# (vizy, vst)	= ey.Editor.genUI dpy y ymask vst
		= (uic (UIEditor {UIEditor|optional=isOptional vizx && isOptional vizy}) [vizx,vizy],vst)

	genDiff dp (PAIR oldx oldy) om (PAIR newx newy) nm vst
		# (dpx,dpy)		= pairPathSplit dp
		# (oxmask,oymask) = case om of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (Untouched,Untouched)
		# (nxmask,nymask) = case nm of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (Untouched,Untouched)
		# (diffx,vst) 	= ex.Editor.genDiff dpx oldx oxmask newx nxmask vst
		# (diffy,vst) 	= ey.Editor.genDiff dpx oldy oymask newy nymask vst
		= (ChangeUI [] [ChangeChild 0 diffx,ChangeChild 1 diffy],vst)

	appDiff [0:ds] e (PAIR x y) xmask ust
		# (x,xmask,ust) = ex.Editor.appDiff ds e x xmask ust
		= (PAIR x y,xmask,ust)
	appDiff [1:ds] e (PAIR x y) ymask ust
		# (y,ymask,ust) = ey.Editor.appDiff ds e y ymask ust
		= (PAIR x y,ymask,ust)
	appDiff _ _ val mask ust = (val,mask,ust)

//The maybe editor makes it content optional
gEditor{|Maybe|} ex _ dx _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp val mask vst=:{VSt|optional,disabled}
		# (viz,vst) = case val of
			(Just x)	= ex.Editor.genUI dp x mask {VSt|vst & optional = True}
			_			= ex.Editor.genUI dp dx Untouched {VSt|vst & optional = True}
		= (toOptional viz, {VSt|vst & optional = optional})
	where
		toOptional	(UI (UIEditor e) attr c)          = UI (UIEditor {UIEditor|e & optional = True}) attr c
		toOptional	viz                               = viz

	genDiff dp Nothing om Nothing nm vst = (NoChange,vst)
	genDiff dp Nothing om (Just new) nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.genDiff dp dx Untouched new nm {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})
	genDiff dp (Just old) om Nothing nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.genDiff dp old om dx Untouched {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})
	genDiff dp (Just old) om (Just new) nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.genDiff dp old om new nm {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})

	appDiff dp e val mask ust
		| isEmpty dp && (e === JSONNull || e === JSONBool False)
			= (Nothing, Blanked,ust) //Reset
		| otherwise
			# (x,xmask) = maybe (dx,Untouched) (\x -> (x,mask)) val
			# (x,xmask,ust) = ex.Editor.appDiff dp e x xmask ust
			= (Just x,xmask,ust)

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

//When UIs, or UI differences are aggregated in PAIR's they form a binary tree 
//These functions flatten this tree back to a single CompoundEditor or ChangeUI definition

flattenPairUI 0 d = d
flattenPairUI 1 d = d
flattenPairUI 2 d = d
flattenPairUI 3 (UI (UIEditor e) a [l, UI (UIEditor _) _ [m,r]]) = UI (UIEditor e) a [l,m,r]
flattenPairUI n (UI (UIEditor e) a [l,r])
	# (UI (UIEditor _) _ l) = flattenPairUI half l
	# (UI (UIEditor _) _ r) = flattenPairUI (n - half) r
	= UI (UIEditor e) a (l ++ r)
where
	half = n / 2

//No pairs are introduced for 0 or 1 fields
flattenPairDiff s 0 d = d 
flattenPairDiff s 1 d = d
//For two and three fields, set the correct child index values 
flattenPairDiff s 2 (ChangeUI _ [ChangeChild _ l,ChangeChild _ r]) = ChangeUI [] [ChangeChild s l,ChangeChild (s + 1) r]
flattenPairDiff s 3 (ChangeUI _ [ChangeChild _ l,ChangeChild _ (ChangeUI _ [ChangeChild _ m,ChangeChild _ r])])
	= ChangeUI [] [ChangeChild s l, ChangeChild (s + 1) m, ChangeChild (s + 2) r]
//For more fields we aggregate both sides
flattenPairDiff s n (ChangeUI _ [ChangeChild _ l, ChangeChild _ r]) 
	# (ChangeUI _ l) = flattenPairDiff s half l
	# (ChangeUI _ r) = flattenPairDiff (s + half) (n - half) r 
	= ChangeUI [] (l ++ r)
where
	half = n / 2

isOptional (UI (UIEditor {UIEditor|optional}) _ _) 		 = optional
isOptional _ 										       = False

gEditor{|Int|} = primitiveTypeEditor (Just "whole number")
					(\viewOpts -> ui (UIViewString defaultSizeOpts (fmap toString viewOpts)))
					(\editOpts -> ui (UIEditInt defaultHSizeOpts editOpts))
gEditor{|Real|} = primitiveTypeEditor (Just "decimal number")
					(\viewOpts -> ui (UIViewString defaultSizeOpts (fmap toString viewOpts)))
					(\editOpts -> ui (UIEditDecimal defaultHSizeOpts editOpts))
gEditor{|Char|} = primitiveTypeEditor (Just "single character")
					(\viewOpts -> ui (UIViewString defaultSizeOpts (fmap toString viewOpts)))
					(\editOpts -> ui (UIEditString defaultHSizeOpts editOpts))
gEditor{|String|} = primitiveTypeEditor (Just "single line of text")
					(\viewOpts -> ui (UIViewString defaultSizeOpts (fmap toString viewOpts)))
					(\editOpts -> ui (UIEditString defaultHSizeOpts editOpts))
gEditor{|Bool|} = primitiveTypeEditor Nothing 
					(\viewOpts -> ui (UIViewCheckbox defaultFSizeOpts viewOpts))
					(\editOpts -> ui (UIEditCheckbox defaultFSizeOpts editOpts))

primitiveTypeEditor mbTypeDesc mkViewControl mkEditControl = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where 
	genUI dp val mask vst=:{VSt|taskId,optional,disabled}
		| disabled	
			= (uic (UIEditor {UIEditor|optional=optional})
				[mkViewControl {UIViewOpts|value = checkMask mask val}],vst)
		| otherwise
        	= (uiac (UIEditor {UIEditor|optional=optional})
				(maybe 'DM'.newMap (\typeDesc -> stdAttributes typeDesc optional mask) mbTypeDesc)
				[mkEditControl {UIEditOpts|taskId=taskId, editorId=editorId dp,value = checkMaskValue mask val}], vst)

	genDiff dp ov om nv nm vst=:{VSt|optional,disabled}
		= (if (vEq && mEq) NoChange (ChangeUI (valueChange ++ attrChanges) []),vst)
	where
		vEq = ov === nv
		mEq = om === nm
		valueChange = if vEq [] [(if disabled "setValue" "setEditorValue",[encodeUI nv])]
		attrChanges = maybe [] (\typeDesc ->stdAttributeChanges typeDesc optional om nm) mbTypeDesc

	appDiff dp e val mask ust = basicUpdateSimple dp e val mask ust

gEditor{|EditableList|} ex _ dx _ _
	= listEditor ex dx
		(\{EditableList|items} -> items)
		(\{EditableList|add} -> add)
		(\{EditableList|remove} -> remove)
		(\{EditableList|reorder} -> reorder)
		(\{EditableList|count} -> count)
		(\items el -> {EditableList|el & items = items})

gEditor{|[]|} ex _ dx _ _
	= listEditor ex dx
		(\x -> x)
		(const ELAddBlank)
		(const True)
		(const True)
		(const True)
		(\x _ -> x)

listEditor ex dx getItems getAdd getRemove getReorder getCount setItems
	= {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp el mask vst=:{VSt|taskId,disabled}
		# (controls,vst) = listControls dp items (subMasks (length items) mask) vst
		= (uic (UIEditor {UIEditor|optional=False}) [listContainer controls], vst)
	where
		items = getItems el
		add = getAdd el
		remove = getRemove el
		reorder = getReorder el
		count = getCount el

		enableAdd = case add of ELNoAdd = False ; _ = True;

		listControls dp items masks vst=:{VSt|optional,disabled}
			# (itemsVis,vst)	= childVisualizations ex.Editor.genUI dp items masks vst
			# numItems = length items
			| not disabled && (enableAdd || count)
				= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
			| otherwise
				= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
								
		listItemControl disabled numItems idx item
			//# controls	= map (setWidth FlexSize) (decorateControls (layout.layoutSubEditor {UIForm| attributes = 'DM'.newMap, controls = editorControls item, size = defaultSizeOpts}))
			# controls = []
			# buttons	= (if reorder
							  [ui (UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mup_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-up",disabled=idx == 0})
							  ,ui (UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("mdn_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-down",disabled= idx == numItems - 1})
							  ] []) ++
							  (if remove
							  [ui (UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString ("rem_" +++ toString idx))} {UIButtonOpts|text=Nothing,iconCls=Just "icon-remove",disabled=False})
							  ] [])
			= setHalign AlignRight (setHeight WrapSize (setDirection Horizontal (uic defaultContainer (if disabled controls (controls ++ buttons)))))
		addItemControl numItems
			# counter   = if count [ui (UIViewString {UISizeOpts|defaultSizeOpts & width=Just FlexSize} {UIViewOpts|value= Just (numItemsText numItems)})] []
			# button	= if enableAdd [ui (UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString "add")} {UIButtonOpts|text=Nothing,iconCls=Just "icon-add",disabled=False})] []
			= (setHalign AlignRight (setHeight WrapSize (setDirection Horizontal (uic defaultContainer (counter ++ button)))))
			
		listContainer controls
			= setHeight WrapSize (uic defaultContainer controls)
			
		numItemsText 1 = "1 item"
		numItemsText n = toString n +++ " items"

	genDiff dp ov om nv nm vst
		# (viz,vst) = genUI dp nv nm vst
		= (ReplaceUI viz,vst)

	appDiff dp e el listMask ust
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
			= (setItems items el,CompoundMask childMasks,ust)
		| otherwise
			= (setItems items el,CompoundMask childMasks,ust)
	where
		items = getItems el
		add = getAdd el
		remove = getRemove el
		reorder = getReorder el
		count = getCount el

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

gEditor{|()|} = emptyEditor
gEditor{|(->)|} _ _ _ _ _ _ _ _ _ _ = emptyEditor
gEditor{|Dynamic|} = emptyEditor

gEditor{|HtmlTag|}	= {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp val mask vst
		= (uic (UIEditor {UIEditor|optional=False}) [ui (UIViewHtml defaultSizeOpts {UIViewOpts|value = Just val})], vst)

	genDiff dp ov om nv nm vst = (NoChange,vst)

	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|RWShared|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = emptyEditor

derive gEditor JSONNode, Either, MaybeError, (,), (,,), (,,,), (,,,,), Timestamp, Map

generic gEditMeta a :: a -> [EditMeta]

gEditMeta{|UNIT|} _			= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|PAIR|} fx fy _	= fx undef ++ fy undef
gEditMeta{|EITHER|} fx fy _	= fx undef //Only consider first constructor
gEditMeta{|OBJECT|} fx _	= fx undef
gEditMeta{|CONS|} fx _		= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|RECORD|} fx _ 	= fx undef
gEditMeta{|FIELD of {gfd_name}|} fx _
							= [{EditMeta|m & label = Just (fromMaybe gfd_name label)} \\ m=:{EditMeta|label} <- fx undef]
gEditMeta{|Int|}	_		= [{label=Nothing,hint=Just "You may enter a whole number",unit=Nothing}]
gEditMeta{|Real|} _			= [{label=Nothing,hint=Just "You may enter a decimal number",unit=Nothing}]
gEditMeta{|Char|} _			= [{label=Nothing,hint=Just "You may enter a single character",unit=Nothing}]
gEditMeta{|String|} _		= [{label=Nothing,hint=Just "You may enter a single line of text",unit=Nothing}]
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
generic gVerify a :: !VerifyOptions (Masked a) -> Verification

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

checkMask :: !EditMask a -> Maybe a
checkMask mask val
    | isTouched mask    = Just val
                        = Nothing

checkMaskValue :: !EditMask a -> Maybe JSONNode | JSONEncode{|*|} a
checkMaskValue Touched val               = Just (toJSON val)
checkMaskValue (TouchedWithState s) val  = Just (toJSON val)
checkMaskValue (TouchedUnparsed r) _  	 = Just r
checkMaskValue _ _                       = Nothing

/**
* Set basic hint and error information based on the verification
*/
stdAttributes :: String Bool EditMask -> UIAttributes
stdAttributes typename optional mask
	| not (isTouched mask)
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,HINT_TYPE_INFO),(HINT_ATTRIBUTE,"Please enter a " +++ typename +++ if optional "" " (this value is required)")]
	| mask =:(CompoundMask _) 
		= 'DM'.newMap
	| otherwise
		# (hintType,hint) = case mask of
        	Touched					= (HINT_TYPE_VALID, "You have correctly entered a " +++ typename) 
        	(TouchedWithState _)	= (HINT_TYPE_VALID, "You have correctly entered a " +++ typename) 
        	(TouchedUnparsed _)		= (HINT_TYPE_INVALID, "This value not in the required format of a " +++ typename)
        	Blanked					= if optional 
											(HINT_TYPE_INFO, "Please enter a " +++ typename)
											(HINT_TYPE_INVALID, "You need to enter a "+++ typename +++ " (this value is required)")
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,hintType),(HINT_ATTRIBUTE,hint)]

stdAttributeChanges :: String Bool EditMask EditMask -> [UILocalChange]
stdAttributeChanges typename optional om nm 
	| om === nm = [] //Nothing to change
	| otherwise = [("setAttribute",[JSONString k,JSONString v]) \\ (k,v) <- 'DM'.toList (stdAttributes typename optional nm)]

addLabel :: !String !UIAttributes -> UIAttributes
addLabel label attr = putCond LABEL_ATTRIBUTE label attr
where
    putCond k v m = maybe ('DM'.put k v m) (const m) ('DM'.get k m)

childVisualizations :: !(DataPath a EditMask -> .(*VSt -> *(!UI,*VSt))) !DataPath ![a] ![EditMask] !*VSt -> *(![UI],!*VSt)
childVisualizations fx dp children masks vst = childVisualizations` 0 children masks [] vst
where
	childVisualizations` i [] [] acc vst
		= (reverse acc,vst)
	childVisualizations` i [child:children] [mask:masks] acc vst
		# (childV,vst) = fx (dp ++ [i]) child mask vst
		= childVisualizations` (i + 1) children masks [childV:acc] vst

verifyValue :: !a -> Verification | gVerify{|*|} a
verifyValue val = verifyMaskedValue (val,Touched)
	
verifyMaskedValue :: !(Masked a) -> Verification | gVerify{|*|} a
verifyMaskedValue mv = gVerify{|*|} {VerifyOptions|optional = False, disabled = False} mv 

isValid :: !Verification -> Bool
isValid (CorrectValue _) = True
isValid (WarningValue _) = True
isValid (CompoundVerification vs) = foldr (\v t -> t && isValid v) True vs 
isValid _ = False

alwaysValid :: !(Masked a) -> Verification
alwaysValid _ = CorrectValue Nothing

simpleVerify :: !VerifyOptions !(Masked a) -> Verification
simpleVerify options mv = customVerify (const True) (const undef) options mv

customVerify :: !(a -> Bool) !(a -> String) !VerifyOptions (Masked a) -> Verification
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

basicUpdate :: !(upd a -> Maybe a) !DataPath !JSONNode !a !EditMask !*USt -> *(!a, !EditMask, !*USt) | JSONDecode{|*|} upd
basicUpdate toV [] upd v vmask ust
        # mbV   = maybe Nothing (\u -> toV u v) (fromJSON upd)
        # v     = fromMaybe v mbV
        # vmask = if (upd === JSONNull) Blanked (if (isNothing mbV) (TouchedUnparsed upd) Touched)
        = (v,vmask,ust)
basicUpdate toV _ upd v vmask ust
		= (v,vmask,ust)

basicUpdateSimple :: !DataPath !JSONNode !a !EditMask !*USt -> *(!a,!EditMask,!*USt) | JSONDecode{|*|} a
basicUpdateSimple target upd val mask iworld = basicUpdate (\json old -> fromJSON json) target upd val mask iworld
