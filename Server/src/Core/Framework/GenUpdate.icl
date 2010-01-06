implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, GenBimap
import Void, Either
import Text

defaultValue :: !*World -> (!a,!*World) | gUpdate{|*|} a
defaultValue world  
	# (a,ust=:{world}) = gUpdate{|*|} (abort "gUpdate accessed value during create") {USt|mode = UDCreate, searchPath = [], currentPath = [], consPath = [], update = "", mask = [], world = world}
	= (a,world)
	
defaultMask :: a !*World -> (DataMask,*World) | gUpdate{|*|} a
defaultMask a world
	# (_,ust=:{world,mask}) = gUpdate{|*|} a {USt| mode = UDMask, searchPath = [], currentPath = [0], consPath = [], update = "", mask = [], world = world}
	= (mask,world)
	
updateValue	:: String String a !*World -> (a,!*World)	| gUpdate{|*|} a  
updateValue path update a world
	# (a,_,world) = updateValueAndMask path update a [] world
	= (a,world)

updateValueAndMask :: String String a DataMask !*World -> (a,DataMask,!*World)	| gUpdate{|*|} a
updateValueAndMask path update a mask world
	//Only try to update when the 'path' string is a datapath formatted string
	| isdps path	
		# (a,ust=:{world,mask}) = gUpdate{|*|} a {USt| mode = UDSearch, searchPath = s2dp path, currentPath = [0], consPath = [], update = update, mask = mask, world = world}
		= (a,mask,world)
	| otherwise	
		= (a,mask,world)

//Generic updater
generic gUpdate a :: a *USt ->  (a, *USt)

gUpdate{|UNIT|} _ ust=:{mode=UDCreate} = (UNIT, ust)
gUpdate{|UNIT|} u ust = (u, ust)

gUpdate{|PAIR|} fx fy _ ust=:{mode=UDCreate}
	# (nx,ust) = fx (abort "PAIR create with undef") ust
	# (ny,ust) = fy (abort "PAIR create with undef") ust
	= (PAIR nx ny, ust)
	
gUpdate{|PAIR|} fx fy p ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	# (ny,ust) = fy y ust
	= (PAIR nx ny, ust)
where
	(PAIR x y) = p
	
gUpdate{|PAIR|} fx fy p ust=:{mode=UDMask}
	# (nx,ust) = fx x ust
	# (ny,ust) = fy y ust
	= (PAIR nx ny, ust)
where
	(PAIR x y) = p	
	
gUpdate{|PAIR|} fx fy p ust = (p, ust)

gUpdate{|EITHER|} fx fy _ ust=:{mode=UDCreate,consPath}
	= case consPath of
		[ConsLeft:cl]
			# (nx,ust) = fx (abort "EITHER create with undef") {ust & consPath = cl}
			= (LEFT nx, ust)
		[ConsRight:cl]
			# (ny,ust) = fy (abort "EITHER create with undef") {ust & consPath = cl}
			= (RIGHT ny, ust)
		[]
			# (nx,ust) = fx (abort "EITHER create with undef") ust
			= (LEFT nx, ust)

gUpdate{|EITHER|} fx fy e ust=:{mode=UDSearch}
	= case e of
		(LEFT x)	
			# (nx,ust) = fx x ust
			= (LEFT nx, ust)
		(RIGHT y)
			# (ny,ust) = fy y ust
			= (RIGHT ny,ust)
			
gUpdate{|EITHER|} fx fy e ust=:{mode=UDMask}
	= case e of
		(LEFT x)	
			# (nx,ust) = fx x ust
			= (LEFT nx, ust)
		(RIGHT y)
			# (ny,ust) = fy y ust
			= (RIGHT ny,ust)
					
gUpdate{|EITHER|} fx fy e ust = (e, ust)

gUpdate{|CONS|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx (abort "CONS create with undef") ust
	= (CONS nx, ust)
		
gUpdate{|CONS|} fx c ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	= (CONS nx, ust)
where
	(CONS x) = c
	
gUpdate{|CONS|} fx c ust=:{mode=UDMask}
	# (nx,ust) = fx x ust
	= (CONS nx, ust)
where
	(CONS x) = c
	
gUpdate{|CONS|} fx c ust = (c, ust)

gUpdate{|OBJECT|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx (abort "OBJECT create with undef") ust
	= (OBJECT nx, ust)

gUpdate{|OBJECT of d|} fx o ust=:{mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		# (nx,ust)	= fx (abort "OBJECT create with undef") {USt|ust & mode = UDCreate, consPath = path}
		= (OBJECT nx, toggleMask {USt|ust & mode = UDDone})			 
	| otherwise
		# (nx,ust) = fx x {USt|ust & currentPath = shiftDataPath currentPath}
		= (OBJECT nx, {USt|ust & currentPath = stepDataPath currentPath})
where
	(OBJECT x) = o

	path = case [cons \\ cons <- d.gtd_conses | cons.gcd_name == update] of
		[cons]	= getConsPath cons
		_		= []

gUpdate{|OBJECT of d|} fx o ust=:{mode=UDMask,currentPath,mask}
	# (_,ust)	= fx x {USt|ust & currentPath = shiftDataPath currentPath, mask = [currentPath:mask]}
	= (o,{USt|ust & currentPath = stepDataPath currentPath})
where
	(OBJECT x) = o

gUpdate{|OBJECT|} fx o ust = (o, ust)

gUpdate{|FIELD|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx (abort "FIELD create with undef") ust
	= (FIELD nx, ust)

gUpdate{|FIELD|} fx f ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	= (FIELD nx, ust)
where
	(FIELD x) = f
	
gUpdate{|FIELD|} fx f ust=:{mode=UDMask}
	# (nx,ust) = fx x ust
	= (FIELD nx, ust)
where
	(FIELD x) = f

gUpdate{|FIELD|} fx f ust = (f, ust)

gUpdate{|Int|} _ ust=:{USt|mode=UDCreate} = (0,ust)
gUpdate{|Int|} i ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (toInt update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (i, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Int|} i ust=:{USt|mode=UDMask,currentPath,mask}
	= (i, {USt|ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]}) 
gUpdate{|Int|} i ust = (i,ust)

gUpdate{|Real|} _ ust=:{USt|mode=UDCreate} = (0.0, ust)
gUpdate{|Real|} r ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (toReal update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (r, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Real|} r ust=:{USt|mode=UDMask,currentPath,mask}
	= (r, {USt|ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]}) 
gUpdate{|Real|} r ust = (r, ust)

gUpdate{|Char|} _ ust=:{USt|mode=UDCreate} = (' ', ust)
gUpdate{|Char|} c ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (if (size update > 0) update.[0] c, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (c, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Char|} c ust=:{USt|mode=UDMask,currentPath,mask}
	= (c, {USt|ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]}) 
gUpdate{|Char|} c ust = (c, ust)

gUpdate{|Bool|} _ ust=:{USt|mode=UDCreate} = (False, ust)
gUpdate{|Bool|} b ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (update == "true", toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (b, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Bool|} b ust=:{USt|mode=UDMask,currentPath,mask}
	= (b, {USt|ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]}) 
gUpdate{|Bool|} b ust = (b, ust)

gUpdate{|String|} _ ust=:{USt|mode=UDCreate} = ("", ust)
gUpdate{|String|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|String|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]}) 
gUpdate{|String|} s ust = (s, ust)


//Specialize instance for Dynamic
gUpdate{|Dynamic|} _ ust=:{USt|mode=UDCreate}	= (dynamic 42, ust)
gUpdate{|Dynamic|} d ust						= (d, ust)

gUpdate{|[]|} fx _ ust=:{USt|mode=UDCreate} = ([], ust)

gUpdate{|[]|} fx l ust=:{USt|mode=UDSearch,searchPath,currentPath,update,mask}
	# (lx,ust=:{mask}) = applyListUpdates fx l {USt|ust & currentPath = shiftDataPath currentPath}
	| currentPath == searchPath
	= case update % (0,2) of	
		"ord"
			# indexes = split "," (update % ((indexOf "_" update)+1,(textSize update)))
			# upd = changeListOrder lx [(toInt i) \\ i <- indexes]
			# nm = changeMaskOrder currentPath mask [(toInt i) \\ i <- indexes] 0 []
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, mask = nm})
		"add"
			# (nv,ust) = fx (abort "LIST create with undef") {USt | ust & mode=UDCreate}
			= (lx++[nv], {USt | ust & currentPath = stepDataPath currentPath, mode = UDSearch});
		"rem"
			# indexes = split "," (update % ((indexOf "_" update)+1,(textSize update)))
			# nm  = removeMaskItems currentPath mask [(toInt i) \\ i <- indexes]
			# upd = removeListItems lx [(toInt i) \\ i <- indexes]
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, mask = nm})
		_ 	= (lx, {USt | ust & currentPath = stepDataPath currentPath})
	| otherwise
		= (lx, {USt | ust & currentPath = stepDataPath currentPath})
where
	changeListOrder []   _   = []
	changeListOrder list ord = changeListOrder` list [] ord
	
	changeListOrder` list acc [] 	 = reverse acc
	changeListOrder` list acc [x:xs] = changeListOrder` list [(list !! x):acc] xs
	
	applyListUpdates fx []     ust = ([],ust)
	applyListUpdates fx [l:ls] ust
	# (lx,ust)  = fx l ust
	# (lxs,ust) = applyListUpdates fx ls ust
	= ([lx:lxs],ust)	
		
	changeMaskOrder path []	  ord	 idx acc = acc
	changeMaskOrder path mask []     idx acc = acc++mask
	changeMaskOrder path mask [i:ix] idx acc
		# mEl 	= [e \\ e <- mask | tlEq e [i:path]]
		# mask 	= remFromMask mEl mask	
		# acc 	= changeElements mEl [idx:path] acc
		= changeMaskOrder path mask ix (idx+1) acc
	
	tlEq e i = tlEq` (reverse e) (reverse i)
	tlEq` _  	 []		= True
	tlEq` [] 	 _ 		= False
	tlEq` [e:ex] [i:ix] = (i == e) && (tlEq` ex ix)	
			
	changeElements [] ntl acc = acc
	changeElements [e:ex] ntl acc
		# ne = reverse (changeElement (reverse e) (reverse ntl))
		= changeElements ex ntl [ne:acc]
	
	changeElement e		 []		= e
	changeElement [e:ex] [t:tx] = [t]++changeElement ex tx
	
	remFromMask remEl mask = [i \\ i <- mask | not(isMember i remEl)]
	
	removeListItems [] _ 		= []
	removeListItems l  []		= l
	removeListItems l  [i:is]	= removeListItems (removeAt i l) is
	
	removeMaskItems p [] _		= []
	removeMaskItems p m []		= m
	removeMaskItems p m [i:is]
		# gEl = [e \\ e <- m | tlGr e [i:p]]
		# lEl = [e \\ e <- m | not (tlEq e [i:p] || tlGr e [i:p])]
		# idx = length p
		= lEl ++ [reverse(shiftUpAt (reverse e) idx) \\ e <- gEl]
	
	tlGr e i = tlGr` (reverse e) (reverse i)
	tlGr` []	 _		= False
	tlGr` [e:ex] [i]	= (e>i)
	tlGr` [e:ex] [i:ix] = (i==e) && (tlGr` ex ix)

	shiftUpAt [] _ 	   = abort "index out of bounds"
	shiftUpAt [i:ix] 0 = [(i-1):ix]
	shiftUpAt [i:ix] n = [i:shiftUpAt ix (n-1)] 
	
gUpdate{|[]|} fx l ust=:{USt|mode=UDMask,currentPath,mask}
	# ust = gMarkList fx l {USt|ust & mask = [currentPath:mask],currentPath = shiftDataPath currentPath}
	= (l,{USt | ust & currentPath = stepDataPath currentPath})
where
	gMarkList fx [] ust
		= ust
	gMarkList fx [x:xs] ust=:{USt|currentPath,mask}
		# (_,ust)	= fx x ust
		= gMarkList fx xs ust
		
gUpdate{|[]|} fx l ust = (l,ust)

gUpdate{|Maybe|} fx _ ust=:{USt|mode=UDCreate} = (Nothing,ust)
gUpdate{|Maybe|} fx m ust=:{USt|mode=UDSearch,currentPath,searchPath,update}
	| currentPath == searchPath && update == ""	
		= (Nothing, toggleMask {USt|ust & mode = UDDone}) //Reset
	| otherwise
		= case m of
			Nothing
				# (x,ust) = fx (abort "Maybe create with undef") {ust & mode = UDCreate} //Create an empty value to update
				# (x,ust=:{mode,currentPath}) = fx x {ust & mode = UDSearch,currentPath = currentPath, searchPath = searchPath,update = update}
				= case mode of
					UDDone	= (Just x,ust) //Only switch keep newly created value if a field was updated
					_		= (Nothing, ust)
			Just x
				# (x,ust) = fx x ust
				= (Just x,ust)

//Specialized instance Maybe that chooses the non-recursive constructor 

gUpdate{|Maybe|} fx m ust=:{USt|mode=UDMask,currentPath,mask}
	= case m of
		Nothing	= (m, {USt|ust & currentPath = stepDataPath currentPath})
		Just x
			# (_,ust) = fx x ust
			= (m, {USt|ust & currentPath = stepDataPath currentPath})

gUpdate{|Maybe|} fx l ust = (l,ust)

derive gUpdate Either, (,), (,,), (,,,), Void

//Utility functions
dp2s :: DataPath -> String
dp2s path = join "-" (map toString (reverse path))

dp2id :: String DataPath -> String
dp2id prefix path = prefix +++ "-" +++ dp2s path 

s2dp :: String -> DataPath
s2dp path = reverse (map toInt (split "-" path))

isdps :: String -> Bool
isdps path = and [c == '-' || isDigit c \\ c <-: path]

stepDataPath :: DataPath -> DataPath
stepDataPath []		= []
stepDataPath [x:xs]	= [inc x:xs]

shiftDataPath :: DataPath -> DataPath
shiftDataPath path	= [0:path]

dataPathLevel :: DataPath -> Int
dataPathLevel l	= length l

instance == DataPath
where
	(==) a b = (length a == length b) && and (map (\(x,y) -> x == y) (zip (a,b)))

//Force a field to be masked
setMask :: *USt -> *USt
setMask ust=:{currentPath,mask}
# mask = if (isMember currentPath mask) mask [currentPath:mask]
= {ust & mask = mask}

//Masking and unmasking of fields
toggleMask :: *USt -> *USt
toggleMask ust=:{searchPath,currentPath,update,mask}
	| searchPath == currentPath
		| update == ""
			//Remove the current path from the mask
			= {ust & mask = filter (\x -> x <> currentPath) mask}
		| otherwise
			//Add the current path to the mask (if it is not already in it)
			# mask = if (isMember currentPath mask) mask [currentPath:mask]
			//Remove the underlying fields from the mask
			# mask = [m \\ m <- mask | not (childOf currentPath m)]
			= {ust & mask = mask}
	| otherwise
		= ust
where
	//Check if a datapath is a child of another
	childOf parent child 
		| clen > plen
			= take plen (reverse child) == reverse parent
		| otherwise
			= False
	where
		plen = length parent
		clen = length child
		
isMasked :: DataPath DataMask -> Bool
isMasked dp dm = isMember dp dm