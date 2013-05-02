implementation module iTasks.Framework.GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, Maybe, StdGeneric, StdEnum, Tuple, List, Text
import iTasks.Framework.Util
import SystemTypes

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

updateValueAndMask :: !DataPath !JSONNode !(!a,!InteractionMask) -> (!a,!InteractionMask) | gUpdate{|*|} a
updateValueAndMask path update (a,oldMask)
	# (a,[newMask:_]) = gUpdate{|*|} path update (a,[oldMask])
	= (a,newMask)

//Generic updater
generic gUpdate a | gDefault a :: ![Int] !JSONNode !(!a,![InteractionMask]) -> (!a, ![InteractionMask])

gUpdate{|UNIT|} _ _ val = val

gUpdate{|PAIR|} gUpdx gDefx gUpdy gDefy [0:target] upd (PAIR x y, mask)
	# (x,mask) = gUpdx target upd (x,mask)
	= (PAIR x y,mask)
gUpdate{|PAIR|} gUpdx gDefx gUpdy gDefy [1:target] upd (PAIR x y, mask)
	# (y,mask) = gUpdy target upd (y,mask)
	= (PAIR x y,mask)
gUpdate{|PAIR|} gUpdx gDefx gUpdy gDefy target upd val = val

gUpdate{|EITHER|} gUpdx gDefx gUpdy gDefy target upd (LEFT x,mask) = appFst LEFT (gUpdx target upd (x,mask))
gUpdate{|EITHER|} gUpdx gDefx gUpdy gDefy target upd (RIGHT y,mask) = appFst RIGHT (gUpdy target upd (y,mask))

gUpdate{|OBJECT of {gtd_num_conses,gtd_conses}|} gUpdx gDefx [] upd (OBJECT x, [_:mask]) //Update is a constructor switch
	# consIdx = case upd of
		JSONInt i	= i
		_			= 0
	# objectMask	= case upd of
		JSONNull	= Blanked	//Reset
		_			= PartiallyTouched (repeatn (gtd_conses !! consIdx).gcd_arity Untouched)
	= (OBJECT (gDefx (path consIdx)),[objectMask:mask])
where
	path consIdx = if (consIdx < gtd_num_conses) (consPath consIdx gtd_num_conses) []

gUpdate{|OBJECT|} gUpdx gDefx target upd (OBJECT object, mask) //Update is targeted somewhere in a substructure of this value
	= appFst OBJECT (gUpdx target upd (object,mask))

gUpdate{|CONS of {gcd_arity,gcd_index}|} gUpdx gDefx [index:target] upd (CONS cons,[consMask:mask])
	| index >= gcd_arity
		= (CONS cons,[consMask:mask])	
	# childMasks = childMasksN consMask gcd_arity
	# (cons,[targetMask:_]) = gUpdx (pairPath index gcd_arity ++ target) upd (cons,[childMasks !! index])
	= (CONS cons,[PartiallyTouched (updateAt index targetMask childMasks):mask])
gUpdate{|CONS|} gUpdx gDefx target upd val = val

gUpdate{|RECORD of {grd_arity}|} gUpdx gDefx [index:target] upd (RECORD record,[recMask:mask])
	| index >= grd_arity
		= (RECORD record,[recMask:mask])
	# childMasks = childMasksN recMask grd_arity
	# (record,[targetMask:_]) = gUpdx (pairPath index grd_arity ++ target) upd (record,[childMasks !! index])
	= (RECORD record,[PartiallyTouched (updateAt index targetMask childMasks):mask])

gUpdate{|RECORD|} gUpdx gDefx _ _ val = val
	
gUpdate{|FIELD|} gUpdx gDefx target upd (FIELD field,mask)= appFst FIELD (gUpdx target upd (field,mask))

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
			
gUpdate{|Maybe|} gUpdx gDefx target upd (m,[mmask:mask])
	| isEmpty target && (upd === JSONNull || upd === JSONBool False)
		= (Nothing, [Blanked:mask]) //Reset
	| otherwise
		= case m of
			Nothing
				// Create a default value
				# x  	= gDefx []
				// Search in the default value
				# (x,[mmask:_])	= gUpdx target upd (x,[Untouched])
				= (Just x, [mmask:mask])
			Just x
				= appFst Just (gUpdx target upd (x,[mmask:mask]))

gUpdate{|[]|} gUpdx gDefx target upd (l,[listMask:mask])
	# (l,childMasks)
		= case ((not (isEmpty target)) && (hd target >= (length l))) of
			True
				# nv = gDefx []
				= (l++[nv],childMasksN listMask (length l) ++ [Untouched])
			False
				= (l, childMasksN listMask (length l))
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
				= (insertAt (length l) /*(index+1)*/ (gDefx []) l, insertAt (length l)/*(index+1)*/ Untouched childMasks)
			_ 	
				= (l,childMasks)
		= (l,[PartiallyTouched childMasks:mask])
	| otherwise
		= (l,[PartiallyTouched childMasks:mask])
where
	updateElements fx [i:target] upd elems masks
		| i >= (length elems)
			= (elems,masks)
		# (nx,[nm:_])	= fx target upd (elems !! i,[masks !! i])
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
		
gUpdate{|Dynamic|}		target upd val = basicUpdate (\Void v -> v) target upd val
gUpdate{|(->)|} _ _ gUpdy _ target upd val = basicUpdate (\Void v -> v) target upd val

gUpdate{|HtmlTag|} target upd val = val

derive gUpdate Either, (,), (,,), (,,,), JSONNode, Void, Timestamp, Map

basicUpdate :: !(upd a -> a) ![Int] !JSONNode !(!a,![InteractionMask]) -> (!a,![InteractionMask]) | JSONDecode{|*|} upd
basicUpdate toV target upd (v,[vmask:mask])
	| isEmpty target
		= (fromMaybe v (fmap (\u -> toV u v) (fromJSON upd)), [if (upd === JSONNull) Blanked Touched:mask])
	| otherwise
		= (v,[vmask:mask])

basicUpdateSimple :: ![Int] !JSONNode !(!a,![InteractionMask]) -> (!a,![InteractionMask]) | JSONDecode{|*|} a
basicUpdateSimple target upd val = basicUpdate (\json old -> fromMaybe old (fromJSON json)) target upd val

instance GenMask InteractionMask
where
	popMask :: ![InteractionMask] -> (!InteractionMask, ![InteractionMask])
	popMask []			= (Untouched, [])
	popMask [c:cm]		= (c,cm)

	appendToMask :: ![InteractionMask] !InteractionMask -> [InteractionMask]
	appendToMask l m	= l ++ [m]

	childMasks :: !InteractionMask -> [InteractionMask]
	childMasks (PartiallyTouched  cm)	= cm
	childMasks _						= []

	childMasksN :: !InteractionMask !Int -> [InteractionMask]
	childMasksN (PartiallyTouched cm) n	= cm
	childMasksN um n					= repeatn n um
	
	isTouched :: !InteractionMask -> Bool
	isTouched  Touched					= True
	isTouched (TouchedWithState _)		= True
	isTouched (PartiallyTouched _)		= True
	isTouched _							= False
