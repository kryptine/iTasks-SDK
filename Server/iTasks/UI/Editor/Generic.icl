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
derive bimap Editor,(,),(,,),(,,,), MaybeError

gEditor{|UNIT|} = emptyEditor

gEditor{|RECORD of {grd_arity}|} ex _ _ _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (RECORD x) vst=:{VSt|taskId,mode,optional}
		= case mode of
			Enter 
				| optional //Just show the checkbox, to enable the remaining fields
					= (Ok (UI UIRecord 'DM'.newMap [checkbox False], CompoundMask [newFieldMask]), vst)
				| otherwise
					= case ex.Editor.genUI (pairPath grd_arity dp) x vst of
						(Ok viz,vst) = (Ok (flattenUIPairs UIRecord grd_arity viz),vst)
						(Error e,vst) = (Error e,vst)
			Update
				= case ex.Editor.genUI (pairPath grd_arity dp) x {VSt|vst & optional = False} of
					(Ok viz,vst) 
						# (UI type attr items, CompoundMask masks) = flattenUIPairs UIRecord grd_arity viz 
						//When optional we add a checkbox
						| optional
							= (Ok (UI type attr [checkbox True:items],CompoundMask [newFieldMask:masks]), vst)
						| otherwise 
							= (Ok (UI type attr items,CompoundMask masks),vst)
					(Error e,vst) = (Error e,vst)
			View 
				= case ex.Editor.genUI (pairPath grd_arity dp) x vst of
					(Ok viz,vst)  = (Ok (flattenUIPairs UIRecord grd_arity viz),vst)
					(Error e,vst) = (Error e,vst)
	where
		checkbox checked = uia UICheckbox (editAttrs taskId (editorId dp) (Just (JSONBool checked)))

	onEdit dp ([],JSONBool True) (RECORD val) (CompoundMask [enableMask:masks]) vst=:{VSt|mode,optional} //Enabling an optional record
		| not optional
			= (Error "Enabling non-optional record",RECORD val,vst)
		//Create and add the fields
		= case ex.Editor.genUI (pairPath grd_arity dp) val {vst & mode = Enter} of
			(Ok viz,vst)
				# (UI type attr items, CompoundMask masks) = flattenUIPairs UIRecord grd_arity viz 
				# change = ChangeUI [] [(i,InsertChild ui) \\ ui <- items & i <- [1..]]
				# enableMask = FieldMask {touched=True,valid=True,state=JSONBool True}
				= (Ok (change,CompoundMask [enableMask:masks]), RECORD val, {vst & mode = mode})
			(Error e,vst) = (Error e, RECORD val, {vst & mode = mode})
	onEdit dp ([],JSONBool False) (RECORD val) (CompoundMask [enableMask:masks]) vst=:{VSt|optional} //Disabling an optional record
		| not optional
			= (Error "Disabling non-optional record",RECORD val,vst)
		//Remove all fields except the enable/disable checkbox
		# change = ChangeUI [] (repeatn grd_arity (1,RemoveChild))
		# enableMask = FieldMask {touched=True,valid=True,state=JSONBool False}
		= (Ok (change,CompoundMask [enableMask:masks]), RECORD val, vst)
	onEdit dp ([],_) (RECORD val) mask vst
		= (Error "Unknown edit event for record",RECORD val,vst)
	
	onEdit dp ([d:ds],e) (RECORD val) (CompoundMask masks) vst=:{VSt|optional}
		| d >= grd_arity
			= (Error "Edit aimed at non-existent record field",RECORD val,vst)
		//When optional we need to adjust for the added checkbox, so we need to offset the record field index with one
		//In the generated UI and mask (but not in the paths when targeting the edit!!).
		# idx = if optional (d + 1) d
		= case ex.Editor.onEdit (pairPath grd_arity dp) (pairSelectPath d grd_arity ++ ds,e) val (masks !! idx) vst of
			(Ok (change,mask),val,vst)
				//Extend the change
				# change = case change of NoChange = NoChange; _ = ChangeUI [] [(idx,ChangeChild change)]
				//Update the mask
				# mask = CompoundMask (updateAt idx mask masks)
				= (Ok (change,mask),RECORD val,vst)
			(Error e,val ,vst) = (Error e, RECORD val, vst)
				
	onEdit _ _ val mask vst = (Ok (NoChange,mask),val,vst)

	onRefresh dp (RECORD new) (RECORD old) mask vst 
		# (change,val,vst) = ex.Editor.onRefresh (pairPath grd_arity dp) new old (toPairMask grd_arity mask) vst
		= (fmap (flattenPairDiff 0 grd_arity) change,RECORD val,vst)

gEditor{|FIELD of {gfd_name}|} ex _ _ _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (FIELD x) vst = case ex.Editor.genUI dp x vst of //Just add the field name as a label
		(Ok (UI type attr items, mask),vst) = (Ok (UI type ('DM'.union attr (labelAttr gfd_name)) items, mask),vst) 
		(Error e,vst)                       = (Error e,vst)

	onEdit dp (tp,e) (FIELD field) mask vst
		# (mbmask,field,vst) = ex.Editor.onEdit dp (tp,e) field mask vst
		= (mbmask,FIELD field,vst)

	onRefresh dp (FIELD new) (FIELD old) mask vst
		# (change,val,vst) = ex.Editor.onRefresh dp new old mask vst
		= (change,FIELD val,vst)

gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} ex _ _ _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (OBJECT x) vst=:{VSt|taskId,mode,optional,selectedConsIndex}
		= case mode of
			Enter
				//If there is only one constructor, just generate the UI for that constructor
				| gtd_num_conses == 1
					# (viz,vst) = ex.Editor.genUI dp x vst
					= (viz,{vst & selectedConsIndex = selectedConsIndex})
				| otherwise
					//Initially only generate a UI to choose a constructor
					# consChooseUI = uia UIDropdown (choiceAttrs taskId (editorId dp) [] [JSONString gdc.gcd_name \\ gdc <- gtd_conses])
					# consChooseMask = FieldMask {touched=False,valid=optional,state=JSONNull}
					= (Ok (UI UIVarCons 'DM'.newMap [consChooseUI],CompoundMask [consChooseMask]),{vst & selectedConsIndex = selectedConsIndex})
			Update
				| gtd_num_conses == 1
					# (viz,vst) = ex.Editor.genUI dp x vst
					= (viz,{vst & selectedConsIndex = selectedConsIndex})
				| otherwise
					= case ex.Editor.genUI dp x vst of
						(Ok (UI UICons attr items, CompoundMask masks),vst)
							# consChooseUI = uia UIDropdown (choiceAttrs taskId (editorId dp) [vst.selectedConsIndex] [JSONString gdc.gcd_name \\ gdc <- gtd_conses])
							# consChooseMask = FieldMask {touched=False,valid=True,state=JSONInt vst.selectedConsIndex}
							= (Ok (UI UIVarCons attr [consChooseUI:items],CompoundMask [consChooseMask:masks]),{vst & selectedConsIndex = selectedConsIndex})
						(Error e,vst) = (Error e,vst)
			View
				//If there is only one constructor we don't need to show the constructor name
				= case ex.Editor.genUI dp x vst of
					(Ok (UI UICons attr items, CompoundMask masks),vst)
						| gtd_num_conses == 1
							= (Ok (UI UICons attr items, CompoundMask masks),{vst & selectedConsIndex = selectedConsIndex})
						| otherwise
							# consNameUI = uia UIViewString (valueAttr (JSONString (gtd_conses !! vst.selectedConsIndex).gcd_name))
							= (Ok (UI UIVarCons attr [consNameUI:items],CompoundMask [newFieldMask:masks]),{vst & selectedConsIndex = selectedConsIndex})
					(Error e,vst) = (Error e,vst)

	onEdit dp ([],JSONNull) (OBJECT val) (CompoundMask [FieldMask {FieldMask|touched,valid,state}:masks]) vst=:{VSt|optional} //Update is a constructor reset
		//If necessary remove the fields of the previously selected constructor
		# change = case state of
			(JSONInt prevConsIdx) = ChangeUI [] (repeatn (gtd_conses !! prevConsIdx).gcd_arity (1,RemoveChild))
			_                     = NoChange	
		# consChooseMask = FieldMask {touched=True,valid=optional,state=JSONNull}
		= (Ok (change,CompoundMask [consChooseMask:masks]),OBJECT val, vst)

	onEdit dp ([],JSONInt consIdx) (OBJECT val) (CompoundMask [FieldMask {FieldMask|touched,valid,state}:masks]) vst=:{VSt|mode} //Update is a constructor switch
		| consIdx < 0 || consIdx >= gtd_num_conses
			= (Error "Constructor selection out of bounds",OBJECT val,vst)
		//Create a default value for the selected constructor
		//This is a rather ugly trick: We create a special target path that consists only of negative values that is
		//decoded by the the onEdit instance of EITHER to create a value that consists of the correct nesting of LEFT's and RIGHT's
    	# (_,val,vst)	= ex.Editor.onEdit dp (consCreatePath consIdx gtd_num_conses,JSONNull) val (CompoundMask []) vst 
		//Create an UI for the new constructor 
		= case ex.Editor.genUI dp val {vst & mode = Enter} of
			(Ok (UI UICons attr items, CompoundMask masks),vst)
				//Construct a UI change that does the following: 
				//1: If necessary remove the fields of the previously selected constructor
				# removals = case state of
					(JSONInt prevConsIdx) = repeatn (gtd_conses !! prevConsIdx).gcd_arity (1,RemoveChild)
					_                 = []
				//2: Inserts the fields of the newly created ui
				# inserts = [(i,InsertChild ui) \\ ui <- items & i <- [1..]]
				# change = ChangeUI [] (removals ++ inserts)
				//Create a new mask for the constructor selection
				# consChooseMask = FieldMask {touched=True,valid=True,state=JSONInt consIdx}
				= (Ok (change,CompoundMask [consChooseMask:masks]), OBJECT val, {vst & mode = mode})
			(Error e,vst) = (Error e, OBJECT val, {vst & mode = mode})

	onEdit dp ([],_) (OBJECT val) mask vst
		= (Error "Unknown constructor select event",OBJECT val,vst)

	onEdit dp (tp,e) (OBJECT val) mask vst  //Update is targeted somewhere in a substructure of this value
		| gtd_num_conses == 1
			//Just call onEdit for the inner value
			= case ex.Editor.onEdit dp (tp,e) val mask vst of
				(Ok (change,mask),val,vst) = (Ok (change,mask),OBJECT val,vst)
				(Error e,val,vst) = (Error e, OBJECT val, vst)
		| otherwise
			//Adjust for the added constructor switch UI
			# (CompoundMask [consChooseMask:masks]) = mask
			= case ex.Editor.onEdit dp (tp,e) val (CompoundMask masks) vst of
				(Ok (change,CompoundMask masks),val,vst)
					# change = case change of
						(ChangeUI attrChanges itemChanges) = ChangeUI attrChanges [(i + 1,c) \\ (i,c) <- itemChanges]
						_                                  = change
					= (Ok (change,CompoundMask [consChooseMask:masks]),OBJECT val,vst)
				(Error e,val,vst) = (Error e, OBJECT val, vst)
	
	onRefresh dp (OBJECT new) (OBJECT old) mask vst=:{VSt|mode,selectedConsIndex=curSelectedConsIndex}
		| gtd_num_conses > 1 && (mode =:Enter || mode =: Update)
			= case ex.Editor.onRefresh dp new old mask {vst & selectedConsIndex = 0} of
				(Ok (change,mask),val,vst=:{VSt|selectedConsIndex}) 
					| selectedConsIndex < 0 //A cons was changed
						# selectedCons = ~selectedConsIndex - 1
						# consChange = ChangeUI [SetAttribute "value" (JSONArray [toJSON selectedCons,JSONBool True])] []
						| allConsesArityZero gtd_conses
							= (Ok (consChange,mask),OBJECT val,{vst & selectedConsIndex = curSelectedConsIndex})
						| otherwise
							= (Ok (ChangeUI [] [(0,ChangeChild consChange),(1,ChangeChild change)],mask),OBJECT val,{vst & selectedConsIndex = curSelectedConsIndex})
					| otherwise
						= (Ok (change,mask),OBJECT val,{vst & selectedConsIndex = curSelectedConsIndex})
				(Error e,val,vst) = (Error e,OBJECT val,vst)
		| otherwise
			# (change,val,vst) = ex.Editor.onRefresh dp new old mask vst
			= (change,OBJECT val,vst)

	allConsesArityZero [] = True
	allConsesArityZero [{gcd_arity}:cs]
		| gcd_arity > 0 = False
						= allConsesArityZero cs

gEditor{|EITHER|} ex _ dx _ _ ey _ dy _ _  = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (LEFT x) vst = ex.Editor.genUI dp x vst
	genUI dp (RIGHT y) vst = ey.Editor.genUI dp y vst	

	//Special case to create a LEFT, after a constructor switch
	onEdit dp ([-1],e) _ mask vst = (Ok (NoChange,mask),LEFT dx,vst)
	onEdit dp ([-1:ds],e) _ mask vst
		# (mask,x,vst) = ex.Editor.onEdit dp (ds,e) dx mask vst
		= (mask,LEFT x,vst)
	//Special cases to create a RIGHT, after a constructor switch
	onEdit dp ([-2],e) _ mask vst = (Ok (NoChange,mask),RIGHT dy,vst)
	onEdit dp ([-2:ds],e) _ mask vst 
		# (mask,y,vst) = ey.Editor.onEdit dp (ds,e) dy mask vst
		= (mask,RIGHT y,vst)
	//Just pass the edit event through 
	onEdit dp (tp,e) (LEFT x) mask vst
		# (mask,x,vst) = ex.Editor.onEdit dp (tp,e) x mask vst
		= (mask,LEFT x,vst)
	onEdit dp (tp,e) (RIGHT y) mask vst
		# (mask,y,vst) = ey.Editor.onEdit dp (tp,e) y mask vst
		= (mask,RIGHT y,vst)

	onRefresh dp (LEFT new) (LEFT old) mask vst 
		# (change,val,vst) = ex.Editor.onRefresh dp new old mask vst
		= (change,LEFT val,vst)
	onRefresh dp (RIGHT new) (RIGHT old) mask vst
		# (change,val,vst) = ey.Editor.onRefresh dp new old mask vst
		= (change,RIGHT val,vst)

	//A different constructor is selected -> generate a new UI
	//We use a negative selConsIndex to encode that the constructor was changed
	onRefresh dp (RIGHT new) (LEFT old) mask vst 
		= case ey.Editor.genUI dp new vst of
			(Ok (ui,mask),vst=:{selectedConsIndex}) = (Ok (ReplaceUI ui,mask),RIGHT new,{vst & selectedConsIndex = -1 - selectedConsIndex})
			(Error e,vst=:{selectedConsIndex}) = (Error e,LEFT old,{vst & selectedConsIndex = -1 - selectedConsIndex})

	onRefresh dp (LEFT new) (RIGHT old) mask vst 
		= case ex.Editor.genUI dp new vst of
			(Ok (ui,mask),vst=:{selectedConsIndex}) = (Ok (ReplaceUI ui,mask),LEFT new,{vst & selectedConsIndex = -1 - selectedConsIndex})
			(Error e,vst=:{selectedConsIndex}) = (Error e,RIGHT old,{vst & selectedConsIndex = -1 - selectedConsIndex})

gEditor{|CONS of {gcd_index,gcd_arity}|} ex _ _ _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (CONS x) vst = case ex.Editor.genUI (pairPath gcd_arity dp) x vst of
		(Ok viz,vst)  = (Ok (flattenUIPairs UICons gcd_arity viz), {VSt| vst & selectedConsIndex = gcd_index})
		(Error e,vst) = (Error e,{VSt| vst & selectedConsIndex = gcd_index})

	onEdit dp ([d:ds],e) (CONS val) (CompoundMask masks) vst
		| d >= gcd_arity
			= (Error "Edit aimed at non-existent constructor field",CONS val,vst)
		//Update the targeted field in the constructor
		= case ex.Editor.onEdit (pairPath gcd_arity dp) (pairSelectPath d gcd_arity ++ ds,e) val (masks !! d) vst of	
			(Ok (change,mask),val,vst)
				//Extend the change
				# change = case change of NoChange = NoChange; _ = ChangeUI [] [(d,ChangeChild change)]
				//Update the mask
				# mask = CompoundMask (updateAt d mask masks)
				= (Ok (change,mask),CONS val,vst)
			(Error e,val,vst) = (Error e,CONS val,vst)

	onRefresh dp (CONS new) (CONS old) mask vst 
		//Diff all fields of the constructor
		# (change,val,vst) = ex.Editor.onRefresh (pairPath gcd_arity dp) new old (toPairMask gcd_arity mask) vst 	
		//Flatten the binary tree of ChangeUI constructors created from
		//the PAIR's into a single ChangeUI constructor
		= (fmap (flattenPairDiff 0 gcd_arity) change,CONS val,vst)
	
gEditor{|PAIR|} ex _ _ _ _ ey _ _ _ _ = {Editor|genUI=genUI,onRefresh=onRefresh,onEdit=onEdit}
where
	genUI dp (PAIR x y) vst
		# (dpx,dpy)		= pairPathSplit dp
		# (vizx, vst)	= ex.Editor.genUI dpx x vst
		| vizx =: (Error _) = (vizx,vst)
		# (vizy, vst)	= ey.Editor.genUI dpy y vst
		| vizy =: (Error _) = (vizy,vst)
		# ((vizx,maskx),(vizy,masky)) = (fromOk vizx,fromOk vizy)
		= (Ok (uic UIPair [vizx,vizy],CompoundMask [maskx,masky]),vst)

	onEdit dp ([0:ds],e) (PAIR x y) xmask ust
		# (dpx,_)		= pairPathSplit dp
		# (xmask,x,ust) = ex.Editor.onEdit dpx (ds,e) x xmask ust
		= (xmask,PAIR x y,ust)
	onEdit dp ([1:ds],e) (PAIR x y) ymask ust
		# (_,dpy)		= pairPathSplit dp
		# (ymask,y,ust) = ey.Editor.onEdit dpy (ds,e) y ymask ust
		= (ymask,PAIR x y,ust)
	onEdit _ _ val mask ust = (Ok (NoChange,mask),val,ust)

	onRefresh dp (PAIR newx newy) (PAIR oldx oldy) mask vst
		# (dpx,dpy)		= pairPathSplit dp
		# (maskx,masky) = case mask of
			CompoundMask [maskx,masky] 	= (maskx,masky)
			_							= (newFieldMask,newFieldMask)
		# (changex,newx,vst) 	= ex.Editor.onRefresh dpx newx oldx maskx vst
		| changex=: (Error _) = (changex,PAIR oldx oldy,vst)
		# (changey,newy,vst) 	= ey.Editor.onRefresh dpy newy oldy masky vst
		| changey =: (Error _) = (changey,PAIR oldx oldy,vst)
		# ((changex,maskx),(changey,masky)) = (fromOk changex,fromOk changey)
		= (Ok (ChangeUI [] [(0,ChangeChild changex),(1,ChangeChild changey)],CompoundMask [maskx,masky]),PAIR newx newy, vst)

//The maybe editor makes it content optional
gEditor{|Maybe|} ex _ dx _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|mode,optional}
		| mode =: View && val =: Nothing // Viewing Nothing is always the empty UI
			= (Ok (ui UIEmpty,newFieldMask),vst)
		= case ex.Editor.genUI dp (fromMaybe dx val) {VSt|vst & optional = True} of
			(Ok (UI type attr items, mask),vst) = (Ok (UI type ('DM'.union (optionalAttr True) attr) items,mask), {VSt|vst & optional = optional})
			(Error e,vst) = (Error e, {VSt|vst & optional = optional})

	onEdit dp (tp,e) val mask vst=:{VSt|optional}
		= case ex.Editor.onEdit dp (tp,e) (fromMaybe dx val) mask {VSt|vst & optional = True} of
			(Ok (change, mask),val,vst)
				| isEmpty tp && (e === JSONNull || e === JSONBool False)
					= (Ok (change, mask),Nothing, {VSt|vst & optional = optional}) //The event was a direct reset (switch to nothing)
				| otherwise
					= (Ok (change, mask),Just val, {VSt|vst & optional = optional}) //The event edited the value in the maybe
			(Error e,val,vst) = (Error e,Nothing,{VSt|vst & optional = optional})

	onRefresh dp Nothing Nothing mask vst = (Ok (NoChange,mask),Nothing,vst)
	onRefresh dp (Just new) Nothing mask vst=:{VSt|optional}
		# (change,val,vst) = ex.Editor.onRefresh dp new dx newFieldMask {VSt|vst & optional = True}
		= (change,Just val,{VSt|vst & optional = optional})
	onRefresh dp Nothing (Just old) mask vst=:{VSt|optional}
		# (change,val,vst) = ex.Editor.onRefresh dp dx old mask {VSt|vst & optional = True}
		= (change,Nothing,{VSt|vst & optional = optional})
	onRefresh dp (Just new) (Just old) mask vst=:{VSt|optional}
		# (change,val,vst) = ex.Editor.onRefresh dp new old mask {VSt|vst & optional = True}
		= (change,Just val,{VSt|vst & optional = optional})

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

//Make a special datapath that encodes the desired nesting of EITHER's to create a constructor
consCreatePath i n
 	| i >= n     = []
	| n == 1     = []
	| i < (n /2) = [ -1: consCreatePath i (n/2) ]
	| otherwise  = [ -2: consCreatePath (i - (n/2)) (n - (n/2)) ]

//Create a path that encodes a sequence of choices (0 for the first, 1 for the second) between the elements of nested PAIR's
pairSelectPath i n
	| i >= n     = []
	| n == 1     = []
	| i < (n /2) = [0: pairSelectPath i (n /2)]
	| otherwise  = [1: pairSelectPath (i - (n/2)) (n - (n/2))]

//When UIs, or UI differences are aggregated in PAIR's they form a binary tree 
//These functions flatten this tree back to a single CompoundEditor or ChangeUI definition
flattenUIPairs type 0 (ui,mask) = (UI type 'DM'.newMap [],CompoundMask [])
flattenUIPairs type 1 (ui,mask) = (UI type 'DM'.newMap [ui],CompoundMask [mask])
flattenUIPairs type 2 (UI UIPair _ [ul,ur], CompoundMask [ml,mr]) = (UI type 'DM'.newMap [ul,ur],CompoundMask [ml,mr])
flattenUIPairs type 3 (UI UIPair _ [ul,UI UIPair _ [um,ur]], CompoundMask [ml,CompoundMask [mm,mr]]) = (UI type 'DM'.newMap [ul,um,ur],CompoundMask [ml,mm,mr])
flattenUIPairs type n (UI UIPair _ [ul,ur], CompoundMask [ml,mr])
	# half = n / 2
	  (UI _ _ ul,CompoundMask ml) = flattenUIPairs type half (ul,ml)
	  (UI _ _ ur,CompoundMask mr) = flattenUIPairs type (n - half) (ur,mr)
	= (UI type 'DM'.newMap (ul ++ ur),CompoundMask (ml ++ mr))

//No pairs are introduced for 0 or 1 fields
flattenPairDiff s 0 (change,mask) = (change,mask)
flattenPairDiff s 1 (change,mask) = (change,mask)
//For two and three fields, set the correct child index values 
flattenPairDiff s 2 (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild r)],CompoundMask [ml,mr]) 
	= (ChangeUI [] [(s,ChangeChild l),(s+1,ChangeChild r)],CompoundMask [ml,mr])
flattenPairDiff s 3 (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild (ChangeUI _ [(_,ChangeChild m),(_,ChangeChild r)]))],CompoundMask [ml,CompoundMask [mm,mr]])
	= (ChangeUI [] [(s,ChangeChild l), (s+1,ChangeChild m), (s+2,ChangeChild r)],CompoundMask [ml,mm,mr])
//For more fields we aggregate both sides
flattenPairDiff s n (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild r)],CompoundMask [ml,mr]) 
	# (ChangeUI _ l,CompoundMask ml) = flattenPairDiff s half (l,ml)
	# (ChangeUI _ r,CompoundMask mr) = flattenPairDiff (s + half) (n - half) (r,mr)
	= (ChangeUI [] (l ++ r),CompoundMask (ml ++ mr))
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

