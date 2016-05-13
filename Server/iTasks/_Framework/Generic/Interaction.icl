implementation module iTasks._Framework.Generic.Interaction

from Data.Map import :: Map
import qualified Data.Map as DM
from StdFunc import const
import StdList, StdBool, StdTuple, StdMisc, StdArray
import Data.Maybe, Data.Either, Data.Error, Data.Generic, Data.Functor, Data.Tuple
import Text, Text.JSON
import iTasks._Framework.IWorld
import iTasks.UI.Definition
import iTasks._Framework.Util
import iTasks.API.Core.Types
import iTasks.UI.Layout
import iTasks.UI.Editor, iTasks.UI.Definition
import iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Common, iTasks.UI.Editor.Combinators

generic gEditor a | gText a, gDefault a, JSONEncode a, JSONDecode a :: Editor a
derive bimap Editor,(,,),(,,,)
derive gEq EditMask

gEditor{|UNIT|} = emptyEditor

gEditor{|RECORD of {grd_arity}|} ex _ _ _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp (RECORD x) mask vst=:{VSt|optional,disabled,taskId}
		= case ex.Editor.genUI (pairPath grd_arity dp) x (toPairMask grd_arity mask) {VSt|vst & optional = False} of
			(Ok viz,vst) 
				# viz = flattenPairUI grd_arity viz
				//When optional we add a checkbox show the checkbox
				| optional && not disabled
					# attr = optionalAttr True
					= (Ok (uiac UICompoundContent attr [checkbox (isTouched mask),viz]), vst)
				| otherwise 
					= (Ok viz,vst)
			(Error e,vst) = (Error e,vst)
	where
		checkbox checked = uia UIEditCheckbox (editAttrs taskId (editorId dp) (Just (JSONBool checked)))

	updUI dp (RECORD old) om (RECORD new) nm vst 
		# (diff,vst) = ex.Editor.updUI (pairPath grd_arity dp) old (toPairMask grd_arity om) new (toPairMask grd_arity nm) vst
		= (fmap (flattenPairDiff 0 grd_arity) diff,vst)

	onEdit [] e (RECORD record) mask ust //Enabling or disabling of a record
    	# mask = case e of
        	JSONBool False  = Blanked
        	_               = Touched
    	= (RECORD record,mask,ust)

	onEdit [d:ds] e (RECORD record) mask ust
		| d >= grd_arity
			= (RECORD record,mask,ust)
		# childMasks = subMasks grd_arity mask
		# (record,targetMask,ust) = ex.Editor.onEdit (updPairPath d grd_arity ++ ds) e record (childMasks !! d) ust
		= (RECORD record,CompoundMask (updateAt d targetMask childMasks),ust)
	onEdit _ _ val mask ust = (val,mask,ust)

gEditor{|FIELD of {gfd_name}|} ex _ _ _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp (FIELD x) mask vst=:{VSt|disabled}
		= case ex.Editor.genUI dp x mask vst of
			(Ok (UI type attr items),vst)
				= (Ok (UI type (addLabel gfd_name attr) items),vst) //Add the field name as a label
			(Error e,vst) = (Error e,vst)

	updUI dp (FIELD old) om (FIELD new) nm vst = ex.Editor.updUI dp old om new nm vst

	onEdit dp e (FIELD field) mask ust
		# (field,mask,ust) = ex.Editor.onEdit dp e field mask ust
		= (FIELD field,mask,ust)

gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} ex _ _ _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp (OBJECT x) mask vst=:{selectedConsIndex = curSelectedConsIndex,disabled,taskId}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		= case ex.Editor.genUI dp x mask {VSt|vst & optional=False} of
			(Ok items, vst=:{selectedConsIndex}) 
        		# choice = case mask of
            		Untouched   = []
            		Blanked     = []
            		_           = [selectedConsIndex]
				| allConsesArityZero gtd_conses //If all constructors have arity 0, we only need the constructor dropwdown
					= (Ok (consDropdown choice), {vst & selectedConsIndex = curSelectedConsIndex})
				| otherwise
					= (Ok (uic UICompoundContent [consDropdown choice,items])
						, {vst & selectedConsIndex = curSelectedConsIndex})
			(Error e,vst) = (Error e,vst)
	//ADT with one constructor or static render: 'DM'.put content into container, if empty show cons name
	| otherwise
		= case ex.Editor.genUI dp x mask vst of
			(Ok (UI UIEmpty _ _),vst)
				= (Ok (stringDisplay (if (isTouched mask) (gtd_conses !! vst.selectedConsIndex).gcd_name "")),{vst & selectedConsIndex = curSelectedConsIndex})
			(Ok viz, vst)
				= (Ok viz, {vst & selectedConsIndex = curSelectedConsIndex})
			(Error e, vst) = (Error e,vst)
	where
		consDropdown choice 
			# options = [JSONString gdc.gcd_name \\ gdc <- gtd_conses]
			# attr    = 'DM'.unions [choiceAttrs taskId (editorId dp) choice options,attributes mask]
			= uia UIDropdown attr
		attributes mask
			| isTouched mask	= 'DM'.fromList[(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_VALID),(HINT_ATTRIBUTE, JSONString "You have correctly selected an option")]
								= 'DM'.fromList[(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INFO),(HINT_ATTRIBUTE, JSONString "Select an option")]

	updUI dp (OBJECT old) om (OBJECT new) nm vst=:{VSt|disabled,selectedConsIndex=curSelectedConsIndex}
		| gtd_num_conses > 1 && not disabled
			= case ex.Editor.updUI dp old om new nm {vst & selectedConsIndex = 0} of
				(Ok diff,vst=:{VSt|selectedConsIndex}) 
					| selectedConsIndex < 0 //A cons was changed
						# selectedCons = ~selectedConsIndex - 1
						# consChange = ChangeUI [SetAttribute "value" (JSONArray [toJSON selectedCons,JSONBool True])] []
						| allConsesArityZero gtd_conses
							= (Ok consChange,{vst & selectedConsIndex = curSelectedConsIndex})
						| otherwise
							= (Ok (ChangeUI [] [(0,ChangeChild consChange),(1,ChangeChild diff)]),{vst & selectedConsIndex = curSelectedConsIndex})
					| otherwise
						= (Ok diff,{vst & selectedConsIndex = curSelectedConsIndex})
				(Error e,vst) = (Error e,vst)
		| otherwise
			= ex.Editor.updUI dp old om new nm vst

	onEdit [] e (OBJECT val) mask ust //Update is a constructor switch
		# consIdx = case e of
			JSONInt i	= i
			_			= 0
		# mask	        = case e of
			JSONNull	= Blanked	//Reset
			_			= CompoundMask (repeatn (gtd_conses !! consIdx).gcd_arity Untouched)
    	# (val,_,ust)	= ex.Editor.onEdit (updConsPath (if (consIdx < gtd_num_conses) consIdx 0) gtd_num_conses) e val mask ust
		= (OBJECT val, mask, ust)
	onEdit dp e (OBJECT val) mask ust //Update is targeted somewhere in a substructure of this value
		# (val,mask,ust) = ex.Editor.onEdit dp e val mask ust
		= (OBJECT val,mask,ust)

	allConsesArityZero [] = True
	allConsesArityZero [{gcd_arity}:cs]
		| gcd_arity > 0 = False
						= allConsesArityZero cs

gEditor{|EITHER|} ex _ dx _ _ ey _ dy _ _  = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp (LEFT x) mask vst = ex.Editor.genUI dp x mask vst
	genUI dp (RIGHT y) mask vst =  ey.Editor.genUI dp y mask vst	

	updUI dp (LEFT old) om (LEFT new) nm vst = ex.Editor.updUI dp old om new nm vst
	updUI dp (RIGHT old) om (RIGHT new) nm vst = ey.Editor.updUI dp old om new nm vst

	//A different constructor is selected -> generate a new UI
	//We use a negative selConsIndex to encode that the constructor was changed
	updUI dp (LEFT old) om (RIGHT new) nm vst 
		# (viz,vst=:{selectedConsIndex}) = ey.Editor.genUI dp new nm vst
		= (fmap ReplaceUI viz, {vst & selectedConsIndex = -1 - selectedConsIndex}) 

	updUI dp (RIGHT old) om (LEFT new) nm vst 
		# (viz,vst=:{selectedConsIndex}) = ex.Editor.genUI dp new nm vst
		= (fmap ReplaceUI viz, {vst & selectedConsIndex = -1 - selectedConsIndex})

	onEdit [d:ds] e either mask ust
		| d == -1 = case ds of
        	[] = (LEFT dx, Untouched, ust)
			_ 
				# (x,mask,ust) = ex.Editor.onEdit ds e dx Untouched ust
				= (LEFT x, mask, ust)
		| d == -2 = case ds of
			[] = (RIGHT dy, Untouched, ust)
			_ 
				# (y,mask,ust) = ey.Editor.onEdit ds e dy Untouched ust
				= (RIGHT y, mask, ust)
		| otherwise
			= case either of
				(LEFT x)
					# (x,mask,ust) = ex.Editor.onEdit [d:ds] e x mask ust
					= (LEFT x, mask, ust)
				(RIGHT y)
					# (y,mask,ust) = ey.Editor.onEdit [d:ds] e y mask ust
					= (RIGHT y, mask, ust)

gEditor{|CONS of {gcd_index,gcd_arity}|} ex _ _ _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp (CONS x) mask vst=:{VSt|taskId,optional,disabled}
		# (viz,vst)	= ex.Editor.genUI (pairPath gcd_arity dp) x (toPairMask gcd_arity mask) vst
    	= (fmap (flattenPairUI gcd_arity) viz,{VSt| vst & selectedConsIndex = gcd_index})

	updUI dp (CONS old) om (CONS new) nm vst 
		//Diff all fields of the constructor
		# (diff,vst) = ex.Editor.updUI (pairPath gcd_arity dp) old (toPairMask gcd_arity om) new (toPairMask gcd_arity nm) vst 	
		//Flatten the binary tree of ChangeUI constructors created from
		//the PAIR's into a single ChangeUI constructor
		= (fmap (flattenPairDiff 0 gcd_arity) diff,vst)
																				
	onEdit [d:ds] e (CONS val) mask ust
		| d >= gcd_arity
			= (CONS val,mask,ust)	
		# childMasks = subMasks gcd_arity mask
		# (val,targetMask,ust) = ex.Editor.onEdit (updPairPath d gcd_arity ++ ds) e val (childMasks !! d) ust
		= (CONS val,CompoundMask (updateAt d targetMask childMasks),ust)
	onEdit _ _ val mask ust = (val,mask,ust)

gEditor{|PAIR|} ex _ _ _ _ ey _ _ _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp (PAIR x y) mask vst
		# (xmask,ymask) = case mask of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (Untouched,Untouched)
		# (dpx,dpy)		= pairPathSplit dp
		# (vizx, vst)	= ex.Editor.genUI dpx x xmask vst
		| vizx =: (Error _)
			= (vizx,vst)
		# (vizy, vst)	= ey.Editor.genUI dpy y ymask vst
		| vizy =: (Error _)
			= (vizy,vst)
		# (vizx,vizy)   = (fromOk vizx,fromOk vizy)
		# optional 		= isOptional vizx && isOptional vizy
		# attr 			= optionalAttr optional
		= (Ok (uiac UICompoundContent attr [vizx,vizy]),vst)

	updUI dp (PAIR oldx oldy) om (PAIR newx newy) nm vst
		# (dpx,dpy)		= pairPathSplit dp
		# (oxmask,oymask) = case om of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (Untouched,Untouched)
		# (nxmask,nymask) = case nm of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (Untouched,Untouched)
		# (diffx,vst) 	= ex.Editor.updUI dpx oldx oxmask newx nxmask vst
		| diffx =: (Error _) = (diffx,vst)
		# (diffy,vst) 	= ey.Editor.updUI dpy oldy oymask newy nymask vst
		| diffy =: (Error _) = (diffy,vst)
		# (diffx,diffy)   = (fromOk diffx,fromOk diffy)
		= (Ok (ChangeUI [] [(0,ChangeChild diffx),(1,ChangeChild diffy)]),vst)

	onEdit [0:ds] e (PAIR x y) xmask ust
		# (x,xmask,ust) = ex.Editor.onEdit ds e x xmask ust
		= (PAIR x y,xmask,ust)
	onEdit [1:ds] e (PAIR x y) ymask ust
		# (y,ymask,ust) = ey.Editor.onEdit ds e y ymask ust
		= (PAIR x y,ymask,ust)
	onEdit _ _ val mask ust = (val,mask,ust)

//The maybe editor makes it content optional
gEditor{|Maybe|} ex _ dx _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val mask vst=:{VSt|optional,disabled}
		# (viz,vst) = case val of
			(Just x)	= ex.Editor.genUI dp x mask {VSt|vst & optional = True}
			_			= ex.Editor.genUI dp dx Untouched {VSt|vst & optional = True}
		# viz = fmap (\(UI type attr items) -> UI type ('DM'.union (optionalAttr True) attr) items) viz
		= (viz, {VSt|vst & optional = optional})

	updUI dp Nothing om Nothing nm vst = (Ok NoChange,vst)
	updUI dp Nothing om (Just new) nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.updUI dp dx Untouched new nm {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})
	updUI dp (Just old) om Nothing nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.updUI dp old om dx Untouched {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})
	updUI dp (Just old) om (Just new) nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.updUI dp old om new nm {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})

	onEdit dp e val mask ust
		| isEmpty dp && (e === JSONNull || e === JSONBool False)
			= (Nothing, Blanked,ust) //Reset
		| otherwise
			# (x,xmask) = maybe (dx,Untouched) (\x -> (x,mask)) val
			# (x,xmask,ust) = ex.Editor.onEdit dp e x xmask ust
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
flattenPairUI 3 (UI t a [l, UI _ _ [m,r]]) = UI t a [l,m,r]
flattenPairUI n (UI t a [l,r])
	# (UI _ _ l) = flattenPairUI half l
	# (UI _ _ r) = flattenPairUI (n - half) r
	= UI t a (l ++ r)
where
	half = n / 2

//No pairs are introduced for 0 or 1 fields
flattenPairDiff s 0 d = d 
flattenPairDiff s 1 d = d
//For two and three fields, set the correct child index values 
flattenPairDiff s 2 (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild r)]) = ChangeUI [] [(s,ChangeChild l),(s+1,ChangeChild r)]
flattenPairDiff s 3 (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild (ChangeUI _ [(_,ChangeChild m),(_,ChangeChild r)]))])
	= ChangeUI [] [(s,ChangeChild l), (s+1,ChangeChild m), (s+2,ChangeChild r)]
//For more fields we aggregate both sides
flattenPairDiff s n (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild r)]) 
	# (ChangeUI _ l) = flattenPairDiff s half l
	# (ChangeUI _ r) = flattenPairDiff (s + half) (n - half) r 
	= ChangeUI [] (l ++ r)
where
	half = n / 2

gEditor{|Int|}    = whenDisabled (liftEditor toInt toString textView) (withHintAttributes "whole number" integerField)
gEditor{|Real|}   = whenDisabled (liftEditor toReal toString textView) (withHintAttributes "decimal number" decimalField)
gEditor{|Char|}   = liftEditor (\c -> c.[0]) toString (whenDisabled textView (withHintAttributes "single character" textField))
gEditor{|String|} = whenDisabled textView (withHintAttributes "single line of text" textField)
gEditor{|Bool|}   = checkBox

gEditor{|[]|} ex _ dx _ _ = listEditor (Just (const dx)) True True (Just (\l -> toString (length l) +++ " items")) ex

/*
listEditor ex dx getItems getAdd getRemove getReorder getCount setItems
	= {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp el mask vst=:{VSt|taskId,disabled}
		= case listControls dp items (subMasks (length items) mask) vst of
			(Ok controls,vst) = (Ok (listContainer controls), vst)
			(Error e,vst) = (Error e,vst)
	where
		items = getItems el
		add = getAdd el
		remove = getRemove el
		reorder = getReorder el
		count = getCount el

		enableAdd = case add of ELNoAdd = False ; _ = True;

		listControls dp items masks vst=:{VSt|optional,disabled}
			= case childVisualizations ex.Editor.genUI dp items masks vst of
				(Ok itemsVis,vst)
					# numItems = length items
					| not disabled && (enableAdd || count)
						= (Ok ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems]),vst)	
					| otherwise
						= (Ok [listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
				(Error e,vst) = (Error e,vst)
								
		listItemControl disabled numItems idx item
			//# controls	= map (setWidth FlexSize) (decorateControls (layout.layoutSubEditor {UIForm| attributes = 'DM'.newMap, controls = editorControls item, size = defaultSizeOpts}))
			# controls = []
			# buttons	= (if reorder
							  [uia UIEditButton ('DM'.unions [iconClsAttr "icon-up", enabledAttr (idx <> 0), editAttrs taskId (editorId dp) (Just (JSONString ("mup_" +++ toString idx)))])
							  ,uia UIEditButton ('DM'.unions [iconClsAttr "icon-down", enabledAttr (idx <> numItems - 1), editAttrs taskId (editorId dp) (Just (JSONString ("mdn_" +++ toString idx)))])
							  ] []) ++
							  (if remove
							  [uia UIEditButton ('DM'.unions [iconClsAttr "icon-remove",editAttrs taskId (editorId dp) (Just (JSONString ("rem_" +++ toString idx)))])
							  ] [])

			# attr = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
			= uiac UIContainer attr (if disabled controls (controls ++ buttons))
		addItemControl numItems
			# counter   = if count [uia UIViewString ('DM'.unions [widthAttr FlexSize, valueAttr (JSONString (numItemsText numItems))])] []
			# button	= if enableAdd [uia UIEditButton ('DM'.unions [iconClsAttr "icon-add",editAttrs taskId (editorId dp) (Just (JSONString "add"))])] []
			# attr      = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
			= uiac UIContainer attr (counter ++ button)
			
		listContainer controls
			= uiac UIContainer (heightAttr WrapSize) controls
			
		numItemsText 1 = "1 item"
		numItemsText n = toString n +++ " items"

	updUI dp ov om nv nm vst = appFst (fmap ReplaceUI) (genUI dp nv nm vst)

	onEdit dp e el listMask ust
		# (items,childMasks)
			= case ((not (isEmpty dp)) && (hd dp >= (length items))) of
				True
					= (items++[dx], subMasks (length items) listMask ++ [Untouched])
				False
					= (items, subMasks (length items) listMask)
		# (items,childMasks,ust) = updateElements ex.Editor.onEdit dp e items childMasks ust
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
*/
gEditor{|()|} = emptyEditor
gEditor{|(->)|} _ _ _ _ _ _ _ _ _ _ = emptyEditor
gEditor{|Dynamic|} = emptyEditor

gEditor{|HtmlTag|}	= {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val mask vst = (Ok (uia UIViewHtml ('DM'.fromList [("value",JSONString (toString val))])), vst)

	updUI dp ov om nv nm vst = (Ok NoChange,vst)

	onEdit _ _ val mask ust = (val,mask,ust)

gEditor{|RWShared|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = emptyEditor

derive gEditor JSONNode, Either, MaybeError, (,), (,,), (,,,), (,,,,), (,,,,,), Timestamp, Map

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

derive gVerify (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), Void, Either, MaybeError, Timestamp, Map

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
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INFO),(HINT_ATTRIBUTE,JSONString ("Please enter a " +++ typename +++ if optional "" " (this value is required)"))]
	| mask =:(CompoundMask _) 
		= 'DM'.newMap
	| otherwise
		# (hintType,hint) = case mask of
        	Touched					= (HINT_TYPE_VALID, "You have correctly entered a " +++ typename) 
        	(TouchedWithState _)	= (HINT_TYPE_VALID, "You have correctly entered a " +++ typename)
        	(TouchedUnparsed _)		= (HINT_TYPE_INVALID,"This value not in the required format of a " +++ typename)
        	Blanked					= if optional 
											(HINT_TYPE_INFO, "Please enter a " +++ typename)
											(HINT_TYPE_INVALID, "You need to enter a "+++ typename +++ " (this value is required)")
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString hintType),(HINT_ATTRIBUTE,JSONString hint)]

stdAttributeChanges :: String Bool EditMask EditMask -> [UIAttributeChange]
stdAttributeChanges typename optional om nm 
	| om === nm = [] //Nothing to change
	| otherwise = [SetAttribute k v \\ (k,v) <- 'DM'.toList (stdAttributes typename optional nm)]

addLabel :: !String !UIAttributes -> UIAttributes
addLabel label attr = putCond LABEL_ATTRIBUTE (JSONString label) attr
where
    putCond k v m = maybe ('DM'.put k v m) (const m) ('DM'.get k m)

/*
childVisualizations :: !(DataPath a EditMask -> .(*VSt -> *(!MaybeErrorString UI,*VSt))) !DataPath ![a] ![EditMask] !*VSt -> *(!MaybeErrorString [UI],!*VSt)
childVisualizations fx dp children masks vst = childVisualizations` 0 children masks [] vst
where
	childVisualizations` i [] [] acc vst
		= (Ok (reverse acc),vst)
	childVisualizations` i [child:children] [mask:masks] acc vst
		= case fx (dp ++ [i]) child mask vst of
			(Ok childV,vst) = childVisualizations` (i + 1) children masks [childV:acc] vst
			(Error e,vst) = (Error e,vst)
*/

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
