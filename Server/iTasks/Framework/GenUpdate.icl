implementation module iTasks.Framework.GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, Data.Maybe, StdGeneric, StdEnum, Data.Tuple, Data.List, Text, Text.JSON, Data.Functor
import iTasks.Framework.Util
import iTasks.API.Core.SystemTypes

from StdFunc import id, flip, const, o
from iTasks.Framework.UIDefinition import :: UISize(..)

generic gDefault a :: [ConsPos] -> a

gDefault{|UNIT|} _							= UNIT
gDefault{|PAIR|} fa fb path					= PAIR (fa []) (fb [])
gDefault{|EITHER|} fa fb []					= LEFT (fa [])
gDefault{|EITHER|} fa fb [ConsLeft:path]	= LEFT (fa path)
gDefault{|EITHER|} fa fb [ConsRight:path]	= RIGHT (fb path)
gDefault{|OBJECT|} fa _						= OBJECT (fa [])
gDefault{|CONS|} fa	_						= CONS (fa [])
gDefault{|RECORD|} fa _						= RECORD (fa [])
gDefault{|FIELD|} fa _						= FIELD (fa [])

gDefault{|Int|}	_							= 0
gDefault{|Real|} _							= 0.0
gDefault{|Char|} _							= '\0'
gDefault{|Bool|} _							= False
gDefault{|String|} _						= ""
gDefault{|[]|} _ _							= []
gDefault{|(,)|} fa fb _						= (fa [],fb [])
gDefault{|(,,)|} fa fb fc _					= (fa [],fb [],fc [])
gDefault{|(,,,)|} fa fb fc fd _				= (fa [],fb [],fc [],fd [])
gDefault{|(->)|} fa fb _					= const (fb [])
gDefault{|Dynamic|}	_						= dynamic 42
gDefault{|Maybe|} fa _						= Nothing

gDefault{|HtmlTag|} _						= Html ""

derive gDefault Either, Void, Map, JSONNode, Timestamp

defaultValue :: a | gDefault{|*|} a
defaultValue = gDefault{|*|} []

updateValueAndMask :: !DataPath !JSONNode !(MaskedValue a) -> MaskedValue a | gUpdate{|*|} a
updateValueAndMask path update (a,mask) = gUpdate{|*|} path update (a,mask)

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

gUpdate{|EITHER|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd (LEFT x,mask) = appFst LEFT (gUpdx target upd (x,mask))
gUpdate{|EITHER|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd (RIGHT y,mask) = appFst RIGHT (gUpdy target upd (y,mask))

gUpdate{|OBJECT of {gtd_num_conses,gtd_conses}|} gUpdx gDefx jDecx [] upd (OBJECT x, _) //Update is a constructor switch
	# consIdx = case upd of
		JSONInt i	= i
		_			= 0
	# mask	        = case upd of
		JSONNull	= Blanked	//Reset
		_			= CompoundMask (repeatn (gtd_conses !! consIdx).gcd_arity Untouched)
	= (OBJECT (gDefx (path consIdx)), mask)
where
	path consIdx = if (consIdx < gtd_num_conses) (consPath consIdx gtd_num_conses) []

gUpdate{|OBJECT|} gUpdx gDefx jDecx target upd (OBJECT object, mask) //Update is targeted somewhere in a substructure of this value
	= appFst OBJECT (gUpdx target upd (object,mask))

gUpdate{|CONS of {gcd_arity,gcd_index}|} gUpdx gDefx jDecx [index:target] upd (CONS cons,mask)
	| index >= gcd_arity
		= (CONS cons,mask)	
	# childMasks = subMasks gcd_arity mask
	# (cons,targetMask) = gUpdx (pairPath index gcd_arity ++ target) upd (cons,childMasks !! index)
	= (CONS cons,CompoundMask (updateAt index targetMask childMasks))
gUpdate{|CONS|} gUpdx gDefx jDecx target upd val = val

gUpdate{|RECORD of {grd_arity}|} gUpdx gDefx jDecx [index:target] upd (RECORD record,mask)
	| index >= grd_arity
		= (RECORD record,mask)
	# childMasks = subMasks grd_arity mask
	# (record,targetMask) = gUpdx (pairPath index grd_arity ++ target) upd (record,childMasks !! index)
	= (RECORD record,CompoundMask (updateAt index targetMask childMasks))

gUpdate{|RECORD|} gUpdx gDefx jDecx _ _ val = val
	
gUpdate{|FIELD|} gUpdx gDefx jDecx target upd (FIELD field,mask)= appFst FIELD (gUpdx target upd (field,mask))

consPath i n
	| i >= n	
		= []
	| n == 1
		= []
	| i < (n/2)
		= [ ConsLeft : consPath i (n/2) ]
	| otherwise
		= [ ConsRight : consPath (i - (n/2)) (n - (n/2)) ]

pairPath i n
	| i >= n
		= []
	| n == 1
		= []
	| i < (n /2)
		= [0: pairPath i (n /2)]
	| otherwise
		= [1: pairPath (i - (n/2)) (n - (n/2))]

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
				# x  	= gDefx []
				// Search in the default value
				# (x,mmask)	= gUpdx target upd (x,Untouched)
				= (Just x, mmask)
			Just x
				= appFst Just (gUpdx target upd (x,mmask))

gUpdate{|[]|} gUpdx gDefx jDecx target upd (l,listMask)
	# (l,childMasks)
		= case ((not (isEmpty target)) && (hd target >= (length l))) of
			True
				# nv = gDefx []
				= (l++[nv], subMasks (length l) listMask ++ [Untouched])
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
				= (insertAt (length l) (gDefx []) l, insertAt (length l) Untouched childMasks)
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
		
gUpdate{|Dynamic|}		target upd val = basicUpdate (\Void v -> Just v) target upd val
gUpdate{|(->)|} _ _ gUpdy _ _ _ target upd val = basicUpdate (\Void v -> Just v) target upd val

gUpdate{|HtmlTag|} target upd val = val

derive gUpdate Either, (,), (,,), (,,,), JSONNode, Void, Timestamp, Map

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

