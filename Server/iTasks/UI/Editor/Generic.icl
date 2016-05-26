implementation module iTasks.UI.Editor.Generic

import iTasks.UI.Definition
import iTasks.UI.Editor
import iTasks.UI.Editor.Builtin
import iTasks.UI.Editor.Combinators
import iTasks.UI.Editor.Common
import iTasks._Framework.SDS

import qualified Data.Map as DM
import StdArray, StdBool, StdFunc
import Text.JSON
import System.Time

generic gEditor a | gText a, gDefault a, JSONEncode a, JSONDecode a :: Editor a
derive bimap Editor,(,,),(,,,)

gEditor{|UNIT|} = emptyEditor

gEditor{|RECORD of {grd_arity}|} ex _ _ _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp (RECORD x) upd vst=:{VSt|optional,disabled,taskId}
		= case ex.Editor.genUI (pairPath grd_arity dp) x upd {VSt|vst & optional = False} of
			(Ok (viz,mask),vst) 
				# viz = flattenPairUI grd_arity viz
				# mask = flattenPairMask grd_arity mask
				//When optional we add a checkbox show the checkbox
				| optional && not disabled
					# attr = optionalAttr True
					= (Ok (uiac UICompoundContent attr [checkbox (isTouched mask),viz],mask), vst)
				| otherwise 
					= (Ok (viz,mask),vst)
			(Error e,vst) = (Error e,vst)
	where
		checkbox checked = uia UIEditCheckbox (editAttrs taskId (editorId dp) (Just (JSONBool checked)))

	updUI dp (RECORD old) om (RECORD new) nm vst 
		# (diff,vst) = ex.Editor.updUI (pairPath grd_arity dp) old (toPairMask grd_arity om) new (toPairMask grd_arity nm) vst
		= (fmap (flattenPairDiff 0 grd_arity) diff,vst)

	onEdit [] e (RECORD record) mask ust //Enabling or disabling of a record
    	# mask = case e of
        	JSONBool False  = CompoundMask [] 
        	_               = mask
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
	genUI dp (FIELD x) upd vst=:{VSt|disabled}
		= case ex.Editor.genUI dp x upd vst of
			(Ok (UI type attr items,mask),vst)
				= (Ok (UI type (addLabel gfd_name attr) items,mask),vst) //Add the field name as a label
			(Error e,vst) = (Error e,vst)

	updUI dp (FIELD old) om (FIELD new) nm vst = ex.Editor.updUI dp old om new nm vst

	onEdit dp e (FIELD field) mask ust
		# (field,mask,ust) = ex.Editor.onEdit dp e field mask ust
		= (FIELD field,mask,ust)

gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} ex _ _ _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp (OBJECT x) upd vst=:{selectedConsIndex = curSelectedConsIndex,disabled,taskId}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| gtd_num_conses > 1 && not disabled
		= case ex.Editor.genUI dp x upd {VSt|vst & optional=False} of
			(Ok (items,mask), vst=:{selectedConsIndex}) 
        		# choice = case mask of
					(FieldMask {FieldMask|state=JSONNull}) = []
            		_          = [selectedConsIndex]
				| allConsesArityZero gtd_conses //If all constructors have arity 0, we only need the constructor dropwdown
					= (Ok (consDropdown choice mask,mask), {vst & selectedConsIndex = curSelectedConsIndex})
				| otherwise
					= (Ok (uic UICompoundContent [consDropdown choice mask,items],CompoundMask [mask])
						, {vst & selectedConsIndex = curSelectedConsIndex})
			(Error e,vst) = (Error e,vst)
	//ADT with one constructor or static render: 'DM'.put content into container, if empty show cons name
	| otherwise
		= case ex.Editor.genUI dp x upd vst of
			(Ok (UI UIEmpty _ _,mask),vst)
				= (Ok (stringDisplay (if (isTouched mask) (gtd_conses !! vst.selectedConsIndex).gcd_name ""),mask),{vst & selectedConsIndex = curSelectedConsIndex})
			(Ok viz, vst)
				= (Ok viz, {vst & selectedConsIndex = curSelectedConsIndex})
			(Error e, vst) = (Error e,vst)
	where
		consDropdown choice mask
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
			JSONNull	= newCompoundMask //Reset
			_			= CompoundMask (repeatn (gtd_conses !! consIdx).gcd_arity newFieldMask)
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
		= case ey.Editor.genUI dp new False vst of
			(Ok (ui,mask),vst=:{selectedConsIndex}) = (Ok (ReplaceUI ui), {vst & selectedConsIndex = -1 - selectedConsIndex})
			(Error e,vst=:{selectedConsIndex}) = (Error e,{vst & selectedConsIndex = -1 - selectedConsIndex})

	updUI dp (RIGHT old) om (LEFT new) nm vst 
		= case ex.Editor.genUI dp new False vst of
			(Ok (ui,mask),vst=:{selectedConsIndex}) = (Ok (ReplaceUI ui), {vst & selectedConsIndex = -1 - selectedConsIndex})
			(Error e,vst=:{selectedConsIndex}) = (Error e,{vst & selectedConsIndex = -1 - selectedConsIndex})

	onEdit [d:ds] e either mask ust
		| d == -1 = case ds of
        	[] = (LEFT dx, newFieldMask, ust)
			_ 
				# (x,mask,ust) = ex.Editor.onEdit ds e dx newFieldMask ust
				= (LEFT x, mask, ust)
		| d == -2 = case ds of
			[] = (RIGHT dy, newFieldMask, ust)
			_ 
				# (y,mask,ust) = ey.Editor.onEdit ds e dy newFieldMask ust
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
	genUI dp (CONS x) upd vst=:{VSt|taskId,optional,disabled}
		= case ex.Editor.genUI (pairPath gcd_arity dp) x upd vst of
			(Ok (ui,mask),vst) = (Ok (flattenPairUI gcd_arity ui,flattenPairMask gcd_arity mask), {VSt| vst & selectedConsIndex = gcd_index})
			(Error e,vst) = (Error e,{VSt| vst & selectedConsIndex = gcd_index})

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
	genUI dp (PAIR x y) update vst
		# (dpx,dpy)		= pairPathSplit dp
		# (vizx, vst)	= ex.Editor.genUI dpx x update vst
		| vizx =: (Error _)
			= (vizx,vst)
		# (vizy, vst)	= ey.Editor.genUI dpy y update vst
		| vizy =: (Error _)
			= (vizy,vst)
		# ((vizx,maskx),(vizy,masky)) = (fromOk vizx,fromOk vizy)
		# optional 		= isOptional vizx && isOptional vizy
		# attr 			= optionalAttr optional
		= (Ok (uiac UICompoundContent attr [vizx,vizy],CompoundMask [maskx,masky]),vst)

	updUI dp (PAIR oldx oldy) om (PAIR newx newy) nm vst
		# (dpx,dpy)		= pairPathSplit dp
		# (oxmask,oymask) = case om of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (newFieldMask,newFieldMask)
		# (nxmask,nymask) = case nm of
			CompoundMask [xmask,ymask] 	= (xmask,ymask)
			_							= (newFieldMask,newFieldMask)
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
import StdMisc
gEditor{|Maybe|} ex _ dx _ _ = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val upd vst=:{VSt|optional,disabled}
		# (viz,vst) = case val of
			(Just x)	= ex.Editor.genUI dp x upd {VSt|vst & optional = True}
			_			= ex.Editor.genUI dp dx False {VSt|vst & optional = True}
		= case viz of
			(Ok (UI type attr items, mask)) = (Ok (UI type ('DM'.union (optionalAttr True) attr) items,mask), {VSt|vst & optional = optional})
			(Error e) = (Error e, {VSt|vst & optional = optional})

	updUI dp Nothing om Nothing nm vst = (Ok NoChange,vst)
	updUI dp Nothing om (Just new) nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.updUI dp dx newFieldMask new nm {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})
	updUI dp (Just old) om Nothing nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.updUI dp old om dx newFieldMask {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})
	updUI dp (Just old) om (Just new) nm vst=:{VSt|optional}
		# (diff,vst) = ex.Editor.updUI dp old om new nm {VSt|vst & optional = True}
		= (diff,{VSt|vst & optional = optional})

	onEdit dp e val mask vst=:{VSt|optional}
		| isEmpty dp && (e === JSONNull || e === JSONBool False)
			# mask = case mask of
				(FieldMask fmask) = FieldMask {FieldMask|fmask & state = JSONNull}
				(CompoundMask m) = CompoundMask []
			= (Nothing, mask,vst) //Reset
		| otherwise
			# (x,xmask) = maybe (dx,CompoundMask []) (\x -> (x,mask)) val
			# (x,xmask,vst) = ex.Editor.onEdit dp e x xmask {VSt|vst & optional = True}
			= (Just x,xmask,{VSt|vst & optional = optional})

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

flattenPairMask n m = m //TODO

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

gEditor{|Int|}    = whenDisabled (liftEditor toString toInt textView) (withHintAttributes "whole number" integerField)
gEditor{|Real|}   = whenDisabled (liftEditor toString toReal textView) (withHintAttributes "decimal number" decimalField)
gEditor{|Char|}   = liftEditor toString (\c -> c.[0]) (whenDisabled textView (withHintAttributes "single character" textField))
gEditor{|String|} = whenDisabled textView (withHintAttributes "single line of text" textField)
gEditor{|Bool|}   = checkBox

gEditor{|[]|} ex _ dx _ _ = listEditor (Just (const dx)) True True (Just (\l -> toString (length l) +++ " items")) ex

gEditor{|()|} = emptyEditor
gEditor{|(->)|} _ _ _ _ _ _ _ _ _ _ = emptyEditor
gEditor{|Dynamic|} = emptyEditor
gEditor{|HtmlTag|} = htmlView
gEditor{|RWShared|} _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = emptyEditor

derive gEditor JSONNode, Either, MaybeError, (,), (,,), (,,,), (,,,,), (,,,,,), Timestamp, Map

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

addLabel :: !String !UIAttributes -> UIAttributes
addLabel label attr = putCond LABEL_ATTRIBUTE (JSONString label) attr
where
    putCond k v m = maybe ('DM'.put k v m) (const m) ('DM'.get k m)


