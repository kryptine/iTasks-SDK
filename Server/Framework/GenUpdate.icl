implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, Maybe, StdGeneric, StdEnum, Tuple, List_NG
import SystemTypes, Text, Util
from StdFunc import id, const, o
from UIDefinition import :: UISize(..)

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

gDefault{|Int|}	_				= 0
gDefault{|Real|} _				= 0.0
gDefault{|Char|} _				= '\0'
gDefault{|Bool|} _				= False
gDefault{|String|} _			= ""
gDefault{|[]|} _ _				= []
gDefault{|(,)|} fa fb _			= (fa [],fb [])
gDefault{|(,,)|} fa fb fc _		= (fa [],fb [],fc [])
gDefault{|(,,,)|} fa fb fc fd _	= (fa [],fb [],fc [],fd [])
gDefault{|(->)|} fa fb _		= const (fb [])
gDefault{|Dynamic|}	_			= dynamic 42
gDefault{|Maybe|} fa _			= Nothing

gDefault{|HtmlTag|} _			= Html ""

derive gDefault Either, Void, Map, JSONNode, Timestamp

defaultValue :: a | gDefault{|*|} a
defaultValue = gDefault{|*|} []

updateValueAndMask :: !DataPath !JSONNode !(!a,!InteractionMask) -> (!a,!InteractionMask)	| gUpdate{|*|} a
updateValueAndMask path update (a,oldMask)
	# (a,ust=:{newMask}) = gUpdate{|*|} a {searchPath = path, currentPath = startDataPath, consPath = [], update = update, oldMask = [oldMask], newMask = []}
	= (a,hd newMask)

//Generic updater
generic gUpdate a | gDefault a :: !a !*USt -> (!a,!*USt)

gUpdate{|UNIT|} _ ust = (UNIT,ust)

gUpdate{|PAIR|} gUpdx gDefx gUpdy gDefy (PAIR x y) ust
	# (nx,ust) = gUpdx x ust
	# (ny,ust) = gUpdy y ust
	= (PAIR nx ny, ust)


gUpdate{|EITHER|} gUpdx gDefx gUpdy gDefy e ust	
	= case e of
		LEFT x
			# (nx,ust) = gUpdx x ust
			= (LEFT nx, ust)
		RIGHT y
			# (ny,ust) = gUpdy y ust
			= (RIGHT ny,ust)


gUpdate{|OBJECT of {gtd_num_conses,gtd_conses}|} gUpdx gDefx (OBJECT x) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		//Update is a constructor switch
		= (OBJECT (gDefx path), {ust & oldMask = om, currentPath = stepDataPath currentPath, newMask = appendToMask newMask (PartiallyTouched consMask)/*(toggleMask update)*/}) 
	| searchPath <== currentPath
		//Update is targeted somewhere in a substructure of this value
		# (nx,ust=:{newMask=childMask}) = gUpdx x {ust & currentPath = shiftDataPath currentPath, oldMask = [cm:om], newMask = []}
		= (OBJECT nx, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask (PartiallyTouched childMask)})
	| otherwise
		//Not on the path, so just put back the current mask (cm)	
		= (OBJECT x, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}) 
where
	path = case update of
		JSONInt consIdx | consIdx < gtd_num_conses
				= consPath consIdx gtd_num_conses
		_		= []

	consPath i n
		| i >= n	
			= []
		| n == 1
			= []
		| i < (n/2)
			= [ ConsLeft : consPath i (n/2) ]
		| otherwise
			= [ ConsRight : consPath (i - (n/2)) (n - (n/2)) ]

	consMask = case update of
		JSONInt consIdx | consIdx < gtd_num_conses
			= repeatn ((gtd_conses !! consIdx).gcd_arity) Untouched
		_	= repeatn (hd gtd_conses).gcd_arity Untouched


gUpdate{|CONS of {gcd_arity}|} gUpdx gDefx (CONS c) ust=:{oldMask}
	# (cm,om)	= popMask oldMask
	# (nx,ust)	= gUpdx c {ust & oldMask = childMasksN cm gcd_arity}
	= (CONS nx,ust)

gUpdate{|RECORD of {grd_arity}|} gUpdx gDefx (RECORD x) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| searchPath <== currentPath
		//Update is targeted somewhere in a substructure of this value
		# (nx,ust=:{newMask=childMask}) = gUpdx x {ust & currentPath = shiftDataPath currentPath, oldMask = childMasksN cm grd_arity, newMask = []}
		= (RECORD nx, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask (PartiallyTouched childMask)})
	| otherwise
		//Not on the path, so just put back the current mask (cm)	
		= (RECORD x, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}) 

gUpdate{|FIELD|} gUpdx gDefx (FIELD c)	ust = appFst FIELD (gUpdx c ust)

gUpdate{|Int|}		val ust = basicUpdateSimple val ust
gUpdate{|Real|}		val ust = basicUpdateSimple val ust
gUpdate{|Char|}		val ust = basicUpdateSimple val ust
gUpdate{|Bool|}		val ust = basicUpdateSimple val ust
gUpdate{|String|}	val ust = basicUpdateSimple val ust
			

gUpdate{|Maybe|} gUpdx gDefx m ust=:{currentPath,searchPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath && (update === JSONNull || update === JSONBool False)
		= (Nothing, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask Blanked, oldMask = om}) //Reset
	| otherwise
		= case m of
			Nothing
				| searchPath <== currentPath
					// Create a default value
					# x  	= gDefx []
					// Search in the default value
					# (x,ust=:{newMask=[nmSearch:_]}) 	= gUpdx x {ust & currentPath = currentPath, searchPath = searchPath, update = update, oldMask = [Untouched], newMask = []}
					= (Just x, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask nmSearch, oldMask = om})
				| otherwise
					= (Nothing, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
			Just x
				# (x,ust) = gUpdx x ust //all mask transformations are made here..
				= (Just x,ust)


gUpdate{|[]|} gUpdx gDefx l ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# (l,childMasks,ust)
		= case isNew currentPath searchPath (length l) of
			True
				# nv = gDefx []
				= (l++[nv],childMasksN cm (length l) ++ [Untouched],ust)
			False
				= (l,childMasksN cm (length l), ust)
	# (lx,ust=:{newMask=cMasks})	= updateElements gUpdx l {ust & currentPath = shiftDataPath currentPath, oldMask = childMasks, newMask = []}
	# ust							= {ust & newMask = cMasks}
	| currentPath == searchPath
		//Process the reordering commands 
		# split = split "_" (fromMaybe "" (fromJSON update))
		# index = toInt (last split)
		# (lx,cMasks,ust) = case hd split of	
			"mup" = (swap lx index,swap cMasks index,ust) 
			"mdn" = (swap lx (index+1),swap cMasks (index+1),ust)
			"rem" = (removeAt index lx,removeAt index cMasks,ust)	
			"add"
				# nv = gDefx []
				= (insertAt (index+1) nv lx, insertAt (index+1) Untouched cMasks, ust)
			_ 	
				= (lx,cMasks,ust)
		= (lx,{ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (PartiallyTouched cMasks), oldMask = om})
	| otherwise
		= (lx, {ust & currentPath  = stepDataPath currentPath, newMask = appendToMask newMask (PartiallyTouched cMasks), oldMask = om})
where
	//Check if search path is equal or below [datapath:(length list)]
	isNew cp sp l = sp <== dataPathFromList [l:dataPathList cp] 
	
	updateElements fx []     ust 
		= ([],ust)
	updateElements fx [l:ls] ust
		# (lx,ust)  = fx l ust
		# (lxs,ust) = updateElements fx ls ust
		= ([lx:lxs],ust)
		
	swap []	  _		= []
	swap list index
		| index == 0 			= list //prevent move first element up
		| index >= length list 	= list //prevent move last element down
		| otherwise				
			# f = list !! (index-1)
			# l = list !! (index)
			= updateAt (index-1) l (updateAt index f list)
		
gUpdate{|Dynamic|}		val ust = basicUpdate val unchanged ust
gUpdate{|(->)|} _ _ gUpdy _ val ust = basicUpdate val unchanged ust

gUpdate{|HtmlTag|} val ust = noUpdate val ust

derive gUpdate Either, (,), (,,), (,,,), JSONNode, Void, Timestamp, Map

noUpdate :: !a !*USt -> *(!a,!*USt)
noUpdate v ust=:{currentPath,oldMask,newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}
	= (v,ust)

basicUpdateSimple :: !a !*USt -> *(!a,!*USt) | JSONDecode{|*|} a
basicUpdateSimple val ust = basicUpdate val (\json old -> fromMaybe old (fromJSON json)) ust

basicUpdate :: !a !(upd a -> a) !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
basicUpdate v toV ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm, om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= (fromMaybe v (fmap (\u -> toV u v) (fromJSON update)), {ust & newMask = appendToMask newMask (toggleMask update)})
	| otherwise
		= (v, {ust & newMask = appendToMask newMask cm})

//Masking and unmasking of fields
toggleMask :: !JSONNode -> InteractionMask
toggleMask update = case update of
	JSONNull	= Blanked
	_			= PartiallyTouched []
	
unchanged :: !Void !a -> a
unchanged _ v = v

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

allUntouched :: ![InteractionMask] -> Bool
allUntouched children = and [isUntouched c \\ c <- children]
where
	isUntouched Untouched	= True
	isUntouched _			= False

allBlanked :: ![InteractionMask] -> Bool
allBlanked children = and [isBlanked c \\ c <- children]
where
	isBlanked Blanked   = True
	isBlanked _			= False
