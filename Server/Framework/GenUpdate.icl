implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, Maybe, StdGeneric, StdEnum, Tuple, List_NG
import SystemTypes, Text, Util
from StdFunc import id, const, o
from UIDefinition import :: UISize(..)



defaultValue :: a | gUpdate{|*|} a
defaultValue = fst (gUpdate{|*|} UDCreate {searchPath = emptyDataPath, currentPath = emptyDataPath, consPath = [], update = JSONNull, oldMask = [Untouched], newMask = [], iworld = Nothing})

updateValueAndMask :: !DataPath !JSONNode !a !InteractionMask !*IWorld -> (!a,!InteractionMask,!*IWorld) | gUpdate{|*|} a
updateValueAndMask path update a oldMask iworld	
	# (a,ust=:{newMask,iworld}) = gUpdate{|*|} (UDSearch a) {searchPath = path, currentPath = startDataPath, consPath = [], update = update, oldMask = [oldMask], newMask = [], iworld = Just iworld}
	= (a,hd newMask,fromJust iworld)

appIWorldUSt :: !.(*IWorld -> *IWorld)!*USt -> *USt
appIWorldUSt f ust=:{iworld} = case iworld of
	Just iworld	= {ust & iworld = Just (f iworld)}
	Nothing		= abort "no IWorld in USt"
	
accIWorldUSt :: !.(*IWorld -> *(!.a,!*IWorld))!*USt -> (!.a,!*USt)
accIWorldUSt f ust=:{iworld} = case iworld of
	Just iworld
		# (a,iworld) = f iworld
		= (a,{ust & iworld = Just iworld})
	Nothing
		= abort "no IWorld in USt"

//Generic updater
generic gUpdate a :: !(UpdateMode a) !*USt -> (!a,!*USt)

gUpdate{|UNIT|} _ ust = (UNIT,ust)

gUpdate{|OBJECT|} fx UDCreate ust=:{newMask}
	//Empty object mask
	# (nx,ust=:{newMask=objectMask}) = fx UDCreate {ust & newMask = []}
	= (OBJECT nx, {ust & newMask = newMask ++ objectMask})
gUpdate{|OBJECT of {gtd_num_conses,gtd_conses}|} fx (UDSearch (OBJECT x)) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		//Update is a constructor switch
		# (nx,ust) = fx UDCreate {ust & consPath = path}
		= (OBJECT nx, {ust & oldMask = om, currentPath = stepDataPath currentPath, newMask = appendToMask newMask (PartiallyTouched consMask)/*(toggleMask update)*/}) 
	| searchPath <== currentPath
		//Update is targeted somewhere in a substructure of this value
		# (nx,ust=:{newMask=childMask}) = fx (UDSearch x) {ust & currentPath = shiftDataPath currentPath, oldMask = [cm:om], newMask = []}
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

gUpdate{|CONS|}	fx UDCreate	ust = appFst CONS (fx UDCreate ust)
gUpdate{|CONS of {gcd_arity}|}	fx (UDSearch (CONS c)) ust=:{oldMask}
	# (cm,om)	= popMask oldMask
	# (nx,ust)	= fx (UDSearch c) {ust & oldMask = childMasksN cm gcd_arity}
	= (CONS nx,ust)

gUpdate{|RECORD|} fx UDCreate ust = appFst RECORD (fx UDCreate ust)
gUpdate{|RECORD of {grd_arity}|} fx (UDSearch (RECORD x)) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| searchPath <== currentPath
		//Update is targeted somewhere in a substructure of this value
		# (nx,ust=:{newMask=childMask}) = fx (UDSearch x) {ust & currentPath = shiftDataPath currentPath, oldMask = childMasksN cm grd_arity, newMask = []}
		= (RECORD nx, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask (PartiallyTouched childMask)})
	| otherwise
		//Not on the path, so just put back the current mask (cm)	
		= (RECORD x, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}) 

gUpdate{|FIELD|}	fx UDCreate				ust = appFst FIELD	(fx UDCreate ust)
gUpdate{|FIELD|}	fx (UDSearch (FIELD c))	ust = appFst FIELD	(fx (UDSearch c) ust)

gUpdate{|PAIR|} fx fy UDCreate ust
	# (nx,ust) = fx UDCreate ust
	# (ny,ust) = fy UDCreate ust
	= (PAIR nx ny, ust)
gUpdate{|PAIR|} fx fy (UDSearch (PAIR x y)) ust
	# (nx,ust) = fx (UDSearch x) ust
	# (ny,ust) = fy (UDSearch y) ust
	= (PAIR nx ny, ust)

gUpdate{|EITHER|} fx fy UDCreate ust=:{consPath}
	= case consPath of
		[ConsLeft:cl]
			# (nx,ust) = fx UDCreate {ust & consPath = cl}
			= (LEFT nx, ust)
		[ConsRight:cl]
			# (ny,ust) = fy UDCreate {ust & consPath = cl}
			= (RIGHT ny, ust)
		[]
			# (nx,ust) = fx UDCreate ust
			= (LEFT nx, ust)
gUpdate{|EITHER|} fx fy (UDSearch e) ust	
	= case e of
		LEFT x
			# (nx,ust) = fx (UDSearch x) ust
			= (LEFT nx, ust)
		RIGHT y
			# (ny,ust) = fy (UDSearch y) ust
			= (RIGHT ny,ust)


gUpdate{|Int|}		mode ust = basicUpdateSimple mode 0 ust
gUpdate{|Real|}		mode ust = basicUpdateSimple mode 0.0 ust
gUpdate{|Char|}		mode ust = basicUpdateSimple mode ' ' ust
gUpdate{|Bool|}		mode ust = basicUpdateSimple mode False ust
gUpdate{|String|}	mode ust = basicUpdateSimple mode "" ust
			
gUpdate{|Maybe|} _ UDCreate ust=:{newMask}
	//Specialized instance Maybe that chooses the non-recursive constructor 
	= (Nothing,{ust & newMask = appendToMask newMask Untouched})
gUpdate{|Maybe|} fx (UDSearch m) ust=:{currentPath,searchPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath && (update === JSONNull || update === JSONBool False)
		= (Nothing, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask Blanked, oldMask = om}) //Reset
	| otherwise
		= case m of
			Nothing
				| searchPath <== currentPath
					// Create a default value
					# (x,ust) 	= fx UDCreate ust
					// Search in the default value
					# (x,ust=:{newMask=[nmSearch:_]}) 	= fx (UDSearch x) {ust & currentPath = currentPath, searchPath = searchPath, update = update, oldMask = [Untouched], newMask = []}
					= (Just x, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask nmSearch, oldMask = om})
				| otherwise
					= (Nothing, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
			Just x
				# (x,ust) = fx (UDSearch x) ust //all mask transformations are made here..
				= (Just x,ust)

gUpdate{|[]|} _ UDCreate ust = basicCreate [] ust
gUpdate{|[]|} fx (UDSearch l) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# (l,childMasks,ust)
		= case isNew currentPath searchPath (length l) of
			True
				# (nv,ust) = fx UDCreate {ust & oldMask = [], newMask = []}
				= (l++[nv],childMasksN cm (length l) ++ [Untouched],ust)
			False
				= (l,childMasksN cm (length l), ust)
	# (lx,ust=:{newMask=cMasks})	= updateElements fx l {ust & currentPath = shiftDataPath currentPath, oldMask = childMasks, newMask = []}
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
				# (nv,ust) = fx UDCreate {ust & oldMask = [], newMask = []}
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
		# (lx,ust)  = fx (UDSearch l) ust
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
		
gUpdate{|Dynamic|}		mode ust = basicUpdate mode unchanged (dynamic 42) ust
gUpdate{|(->)|} _ fy	mode ust
	# (def,ust) = fy UDCreate ust
	= basicUpdate mode unchanged (const def) ust

gUpdate{|HtmlTag|} mode ust = noUpdate mode (Html "") ust

derive gUpdate Either, (,), (,,), (,,,), JSONNode, Void, Timestamp, Map

noUpdate :: !(UpdateMode a) a !*USt -> *(!a,!*USt)
noUpdate UDCreate def ust	= basicCreate def ust
noUpdate (UDSearch v) _ ust=:{currentPath,oldMask,newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}
	= (v,ust)

basicUpdate :: !(UpdateMode a) (upd a -> a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
basicUpdate mode toV def ust = case mode of
	UDCreate	= basicCreate def ust
	UDSearch v	= basicSearch v toV ust
	
basicUpdateSimple :: !(UpdateMode a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} a
basicUpdateSimple mode def ust = case mode of
	UDCreate	= basicCreate def ust
	UDSearch v	= basicSearch v (\json old -> fromMaybe old (fromJSON json)) ust	

basicCreate :: !a !*USt -> *(!a,!*USt)
basicCreate def ust=:{newMask} = (def,{ust & newMask = appendToMask newMask Untouched})

basicSearch :: !a !(upd a -> a) !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
basicSearch v toV ust=:{searchPath,currentPath,update,oldMask,newMask}
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
