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

gEditor{|UNIT|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp _ _ _ vst = (UIEmpty {UIEmpty|actions=[]},vst)
	genDiff dp _ _ vst = (NoChange,vst)
	appDiff dp e val mask ust = (val,mask,ust)

gEditor{|RECORD of {grd_arity}|} ex _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (RECORD x) mask ver vst=:{VSt|optional,disabled,taskId}
		# (viz,vst) = ex.Editor.genUI (pairPath grd_arity dp) x (toPairMask grd_arity mask) (toPairVerification grd_arity ver)
			 {VSt|vst & optional = False}
		# viz = flattenPairUI grd_arity viz
		//When optional we add a checkbox show the checkbox
		| optional && not disabled
			= (UICompoundEditor {UIEditor|optional=True,attributes='DM'.newMap} [checkbox (isTouched mask),viz], vst)
		| otherwise 
			= (viz,vst)
	where
		checkbox checked = UIEditor {UIEditor|optional=True,attributes='DM'.newMap}
			(UIEditCheckbox defaultFSizeOpts {UIEditOpts|taskId = taskId, editorId = editorId dp, value = Just (JSONBool checked)}) 

	genDiff dp (RECORD old) (RECORD new) vst 
		# (diff,vst) = ex.Editor.genDiff (pairPath grd_arity dp) old new vst
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
	genUI dp (FIELD x) mask ver vst=:{VSt|disabled}
		# (viz,vst)		= ex.Editor.genUI dp x mask ver vst
		= case viz of
			//Add the field name as a label
			UIEditor edit=:{UIEditor|optional,attributes} ctrl
				= (UIEditor {UIEditor|edit & attributes = addLabel (optional || disabled) gfd_name attributes} ctrl,vst)
			UICompoundEditor edit=:{UIEditor|optional,attributes} fields
				= (UICompoundEditor {UIEditor|edit & attributes = addLabel (optional || disabled) gfd_name attributes} fields,vst)
			def
				= (def,vst)

	genDiff dp (FIELD old) (FIELD new) vst = ex.Editor.genDiff dp old new vst

	appDiff dp e (FIELD field) mask ust
		# (field,mask,ust) = ex.Editor.appDiff dp e field mask ust
		= (FIELD field,mask,ust)

gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} ex _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (OBJECT x) mask ver vst=:{selectedConsIndex = curSelectedConsIndex,disabled,taskId}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		# (items, vst=:{selectedConsIndex}) = ex.Editor.genUI dp x mask ver {VSt|vst & optional=False}
        # choice = case mask of
            Untouched   = []
            Blanked     = []
            _           = [selectedConsIndex]
		| allConsesArityZero gtd_conses //If all constructors have arity 0, we only need the constructor dropwdown
			= (consDropdown choice, {vst & selectedConsIndex = curSelectedConsIndex})
		| otherwise
			= (UICompoundEditor {UIEditor|optional=False,attributes='DM'.newMap} [consDropdown choice,items]
			, {vst & selectedConsIndex = curSelectedConsIndex})
	//ADT with one constructor or static render: 'DM'.put content into container, if empty show cons name
	| otherwise
		# (viz,vst) = ex.Editor.genUI dp x mask ver vst
		# viz = case viz of
			UIEmpty _	= UIEditor {UIEditor|optional=False,attributes='DM'.newMap} (stringDisplay (if (isTouched mask) (gtd_conses !! vst.selectedConsIndex).gcd_name "")) 
			_			= viz
		= (viz,{vst & selectedConsIndex = curSelectedConsIndex})
	where
		consDropdown choice = UIEditor 
			{UIEditor|optional=False,attributes=attributes mask}
				(UIDropdown defaultHSizeOpts
								{UIChoiceOpts
								| taskId = taskId
								, editorId = editorId dp
								, value = choice
								, options = [gdc.gcd_name \\ gdc <- gtd_conses]}) 
		attributes mask
			| isTouched mask	= 'DM'.fromList[(HINT_TYPE_ATTRIBUTE,HINT_TYPE_VALID),(HINT_ATTRIBUTE, "You have correctly selected an option")]
								= 'DM'.fromList[(HINT_TYPE_ATTRIBUTE,HINT_TYPE_INFO),(HINT_ATTRIBUTE, "Select an option")]

	genDiff dp (OBJECT old) (OBJECT new) vst=:{VSt|disabled,selectedConsIndex=curSelectedConsIndex}
		| gtd_num_conses > 1 && not disabled
			# (diff,vst=:{VSt|selectedConsIndex}) = ex.Editor.genDiff dp old new {vst & selectedConsIndex = 0}
			| selectedConsIndex < 0 //A cons was changed
				# selectedCons = ~selectedConsIndex - 1
				# consChange = ChangeUI [("setValue",[toJSON selectedCons,JSONBool True])] []
				| allConsesArityZero gtd_conses
					= (consChange,{vst & selectedConsIndex = curSelectedConsIndex})
				| otherwise
					= (ChangeUI [] [(0,consChange),(1,diff)],{vst & selectedConsIndex = curSelectedConsIndex})
			| otherwise
				= (diff,{vst & selectedConsIndex = curSelectedConsIndex})
		| otherwise
			= ex.Editor.genDiff dp old new vst

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
	genUI dp (LEFT x) mask ver vst = ex.Editor.genUI dp x mask ver vst
	genUI dp (RIGHT y) mask ver vst =  ey.Editor.genUI dp y mask ver vst	

	genDiff dp (LEFT old) (LEFT new) vst = ex.Editor.genDiff dp old new vst
	genDiff dp (RIGHT old) (RIGHT new) vst = ey.Editor.genDiff dp old new vst

	//A different constructor is selected -> generate a new UI
	//We use a negative selConsIndex to encode that the constructor was changed
	genDiff dp (LEFT old) (RIGHT new) vst 
		# (viz,vst=:{selectedConsIndex}) = ey.Editor.genUI dp new Untouched (alwaysValid (new,Untouched)) vst
		= (ReplaceUI viz, {vst & selectedConsIndex = -1 - selectedConsIndex}) 

	genDiff dp (RIGHT old) (LEFT new) vst 
		# (viz,vst=:{selectedConsIndex}) = ex.Editor.genUI dp new Untouched (alwaysValid (new,Untouched)) vst
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
	genUI dp (CONS x) mask ver vst=:{VSt|taskId,optional,disabled}
		# (viz,vst)	= ex.Editor.genUI (pairPath gcd_arity dp) x (toPairMask gcd_arity mask) (toPairVerification gcd_arity ver) vst
    	= (flattenPairUI gcd_arity viz,{VSt| vst & selectedConsIndex = gcd_index})

	genDiff dp (CONS old) (CONS new) vst 
		# (diff,vst) = ex.Editor.genDiff (pairPath gcd_arity dp) old new vst 	//Diff all fields of the constructor
		= (flattenPairDiff 0 gcd_arity diff,vst) 								//Flatten the binary tree of ChangeUI constructors created from
																				//the PAIR's into a single ChangeUI constructor
	appDiff [d:ds] e (CONS val) mask ust
		| d >= gcd_arity
			= (CONS val,mask,ust)	
		# childMasks = subMasks gcd_arity mask
		# (val,targetMask,ust) = ex.Editor.appDiff (updPairPath d gcd_arity ++ ds) e val (childMasks !! d) ust
		= (CONS val,CompoundMask (updateAt d targetMask childMasks),ust)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|PAIR|} ex _ _ _ _ ey _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp (PAIR x y) mask ver vst
		# (xmask,ymask) = case mask of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (Untouched,Untouched)
		# (xver,yver) = case ver of
			CompoundVerification [xver,yver] 	= (xver,yver)
			_ 									= (CorrectValue Nothing,CorrectValue Nothing)
		# (dpx,dpy)		= pairPathSplit dp
		# (vizx, vst)	= ex.Editor.genUI dpx x xmask xver vst
		# (vizy, vst)	= ey.Editor.genUI dpy y ymask yver vst
		= (UICompoundEditor {UIEditor|optional=isOptional vizx && isOptional vizy,attributes='DM'.newMap} [vizx,vizy],vst)

	genDiff dp (PAIR oldx oldy) (PAIR newx newy) vst
		# (dpx,dpy)		= pairPathSplit dp
		# (diffx,vst) 	= ex.Editor.genDiff dpx oldx newx vst
		# (diffy,vst) 	= ey.Editor.genDiff dpx oldy newy vst
		= (ChangeUI [] [(0,diffx),(1,diffy)],vst)

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
	genUI dp val mask ver vst=:{VSt|optional,disabled}
		# (viz,vst) = case val of
			(Just x)	= ex.Editor.genUI dp x mask ver {VSt|vst & optional = True}
			_			= ex.Editor.genUI dp dx Untouched ver {VSt|vst & optional = True}
		= (toOptional viz, {VSt|vst & optional = optional})
	where
		toOptional	(UIEditor e c)			= UIEditor {UIEditor|e & optional = True} c
		toOptional	(UICompoundEditor e ex)	= UICompoundEditor {UIEditor|e & optional = True} ex
		toOptional	viz						= viz

	genDiff dp Nothing Nothing vst = (NoChange,vst)
	genDiff dp Nothing (Just new) vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.genDiff dp dx new {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})
	genDiff dp (Just old) Nothing vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.genDiff dp old dx {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})
	genDiff dp (Just old) (Just new) vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.genDiff dp old new {VSt|vst & optional = True}
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
flattenPairUI 3 (UICompoundEditor e [l,UICompoundEditor _ [m,r]]) = UICompoundEditor e [l,m,r]
flattenPairUI n (UICompoundEditor e [l,r])
	# (UICompoundEditor _ l) = flattenPairUI half l
	# (UICompoundEditor _ r) = flattenPairUI (n - half) r
	= UICompoundEditor e (l ++ r)
where
	half = n / 2

//No pairs are introduced for 0 or 1 fields
flattenPairDiff s 0 d = d 
flattenPairDiff s 1 d = d
//For two and three fields, set the correct child index values 
flattenPairDiff s 2 (ChangeUI _ [(_,l),(_,r)]) = ChangeUI [] [(s,l),(s + 1,r)]
flattenPairDiff s 3 (ChangeUI _ [(_,l),(_,ChangeUI _ [(_,m),(_,r)])])
	= ChangeUI [] [(s,l),(s + 1,m),(s + 2,r)]
//For more fields we aggregate both sides
flattenPairDiff s n (ChangeUI _ [(_,l),(_,r)]) 
	# (ChangeUI _ l) = flattenPairDiff s half l
	# (ChangeUI _ r) = flattenPairDiff (s + half) (n - half) r 
	= ChangeUI [] (l ++ r)
where
	half = n / 2

isOptional (UIEditor {UIEditor|optional} _) 		= optional
isOptional (UICompoundEditor {UIEditor|optional} _) = optional
isOptional _ 										= False

gEditor{|Int|} = primitiveTypeEditor (Just "whole number")
					(\viewOpts -> UIViewString defaultSizeOpts (fmap toString viewOpts))
					(\editOpts -> UIEditInt defaultHSizeOpts editOpts)
gEditor{|Real|} = primitiveTypeEditor (Just "decimal number")
					(\viewOpts -> UIViewString defaultSizeOpts (fmap toString viewOpts))
					(\editOpts -> UIEditDecimal defaultHSizeOpts editOpts)
gEditor{|Char|} = primitiveTypeEditor (Just "single character")
					(\viewOpts -> UIViewString defaultSizeOpts (fmap toString viewOpts))
					(\editOpts -> UIEditString defaultHSizeOpts editOpts)
gEditor{|String|} = primitiveTypeEditor (Just "single line of text")
					(\viewOpts -> UIViewString defaultSizeOpts (fmap toString viewOpts))
					(\editOpts -> UIEditString defaultHSizeOpts editOpts)
gEditor{|Bool|} = primitiveTypeEditor Nothing 
					(\viewOpts -> UIViewCheckbox defaultFSizeOpts viewOpts)
					(\editOpts -> UIEditCheckbox defaultFSizeOpts editOpts)

primitiveTypeEditor mbTypeDesc mkViewControl mkEditControl = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where 
	genUI dp val mask ver vst=:{VSt|taskId,optional,disabled}
		| disabled	
			= (UIEditor {UIEditor|optional=optional,attributes='DM'.newMap} 
				(mkViewControl {UIViewOpts|value = checkMask mask val}),vst)
		| otherwise
        	= (UIEditor {UIEditor|optional=optional,attributes=maybe 'DM'.newMap (\typeDesc -> stdAttributes typeDesc (val,mask,ver)) mbTypeDesc}
				(mkEditControl {UIEditOpts|taskId=taskId, editorId=editorId dp,value = checkMaskValue mask val}), vst)

	genDiff dp old new vst=:{VSt|disabled}
		= (if (old === new) NoChange (ChangeUI [(if disabled "setValue" "setEditorValue",[encodeUI new])] []),vst)

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
	genUI dp el mask ver vst=:{VSt|taskId,disabled}
		# (controls,vst) = listControls dp items (subMasks (length items) mask) (subVerifications (length items) ver) vst
		= (UIEditor {UIEditor|optional=False,attributes='DM'.newMap} (listContainer controls), vst)
	where
		items = getItems el
		add = getAdd el
		remove = getRemove el
		reorder = getReorder el
		count = getCount el

		enableAdd = case add of ELNoAdd = False ; _ = True;

		listControls dp items masks vers vst=:{VSt|optional,disabled}
			# (itemsVis,vst)	= childVisualizations ex.Editor.genUI dp items masks vers vst
			# numItems = length items
			| not disabled && (enableAdd || count)
				= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]] ++ [addItemControl numItems],vst)	
			| otherwise
				= ([listItemControl disabled numItems idx dx \\ dx <- itemsVis & idx <- [0..]],vst)
								
		listItemControl disabled numItems idx item
			//# controls	= map (setWidth FlexSize) (decorateControls (layout.layoutSubEditor {UIForm| attributes = 'DM'.newMap, controls = editorControls item, size = defaultSizeOpts}))
			# controls = []
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

	genDiff dp old new vst
		# (viz,vst) = genUI dp new Untouched (alwaysValid (new,Untouched)) vst
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

gEditor{|()|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp _ _ _ vst = (UIEmpty {UIEmpty|actions=[]},vst)
	genDiff _ _ _ vst = (NoChange,vst)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|(->)|} _ _ _ _ _ _ _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ vst = (UIEmpty {UIEmpty|actions=[]},vst)
	genDiff _ _ _ vst = (NoChange,vst)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|Dynamic|} = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ vst = (UIEmpty {UIEmpty|actions=[]},vst)
	genDiff _ _ _ vst = (NoChange,vst)
	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|HtmlTag|}	= {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI dp val mask ver vst
		= (UIEditor {UIEditor|optional=False,attributes='DM'.newMap} (UIViewHtml defaultSizeOpts {UIViewOpts|value = Just val}), vst)

	genDiff dp old new vst = (NoChange,vst)

	appDiff _ _ val mask ust = (val,mask,ust)

gEditor{|RWShared|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = {Editor|genUI=genUI,genDiff=genDiff,appDiff=appDiff}
where
	genUI _ _ _ _ vst = (UIEmpty {UIEmpty|actions=[]},vst)

	genDiff dp old new vst = (NoChange, vst)
	appDiff _ _ val mask ust = (val,mask,ust)

derive gEditor JSONNode, Either, MaybeError, (,), (,,), (,,,), (,,,,), Timestamp, Map

generic gEditMeta a :: a -> [EditMeta]

gEditMeta{|UNIT|} _			= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|PAIR|} fx fy _	= fx undef ++ fy undef
gEditMeta{|EITHER|} fx fy _	= fx undef //Only consider first constructor
gEditMeta{|OBJECT|} fx _	= fx undef
gEditMeta{|CONS|} fx _		= [{label=Nothing,hint=Nothing,unit=Nothing}]
gEditMeta{|RECORD|} fx _ 	= fx undef
gEditMeta{|FIELD of {gfd_name}|} fx _
							= [{EditMeta|m & label = Just (fromMaybe (camelCaseToWords gfd_name) label)} \\ m=:{EditMeta|label} <- fx undef]
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

/**
* Set basic hint and error information based on the verification
*/
stdAttributes :: String (VerifiedValue a) -> UIAttributes
stdAttributes typename (val,mask,ver)
	| not (isTouched mask)
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,HINT_TYPE_INFO),(HINT_ATTRIBUTE,"Please enter a " +++ typename)]
	| otherwise
		# (hintType,hint) = case ver of
        	(CorrectValue msg)		= (HINT_TYPE_VALID, fromMaybe ("You have correctly entered a " +++ typename) msg) 
        	(WarningValue msg)		= (HINT_TYPE_WARNING, msg)
        	(IncorrectValue msg)	= (HINT_TYPE_INVALID, msg)
        	(UnparsableValue)		= (HINT_TYPE_INVALID, "This value not in the required format of a" +++ typename)
        	(MissingValue)			= (HINT_TYPE_INVALID, "You need to enter a "+++ typename +++ " (this value is required)")
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,hintType),(HINT_ATTRIBUTE,hint)]

addLabel :: !Bool !String !UIAttributes -> UIAttributes
addLabel optional label attr = putCond LABEL_ATTRIBUTE (format optional label) attr
where
	format optional label = camelCaseToWords label +++ if optional "" "*" +++ ":" //TODO: Move to layout
    putCond k v m = maybe ('DM'.put k v m) (const m) ('DM'.get k m)

childVisualizations :: !(DataPath a InteractionMask Verification -> .(*VSt -> *(!UIDef,*VSt))) !DataPath ![a] ![InteractionMask] ![Verification] !*VSt -> *(![UIDef],!*VSt)
childVisualizations fx dp children masks vers vst = childVisualizations` 0 children masks vers [] vst
where
	childVisualizations` i [] [] [] acc vst
		= (reverse acc,vst)
	childVisualizations` i [child:children] [mask:masks] [ver:vers] acc vst
		# (childV,vst) = fx (dp ++ [i]) child mask ver vst
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
