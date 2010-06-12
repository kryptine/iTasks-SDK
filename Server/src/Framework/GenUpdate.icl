implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, StdEnum
import Void, Either
import Text
import JSON
import Types

from StdFunc import id

derive bimap	(,)

:: DataPath = DataPath [Int] (Maybe SubEditorIndex)

defaultValue :: !*World -> (!a,!*World) | gUpdate{|*|} a
defaultValue world  
	# (a,ust=:{world}) = gUpdate{|*|} (abort "gUpdate accessed value during create") {USt|mode = UDCreate, searchPath = initialDataPath, currentPath = initialDataPath, consPath = [], update = "", mask = [], listMask = [], world = world}
	= (a,world)
	
defaultMask :: a !*World -> (DataMask,*World) | gUpdate{|*|} a
defaultMask a world
	# (_,ust=:{world,mask}) = gUpdate{|*|} a {USt| mode = UDMask, searchPath = initialDataPath, currentPath = shiftDataPath initialDataPath, consPath = [], update = "", mask = [], listMask = [], world = world}
	= (mask,world)
	
updateValue	:: DataPath String a !*World -> (a,!*World)	| gUpdate{|*|} a  
updateValue path update a world
	# (a,_,_,world) = updateValueAndMask path update a [] [] world
	= (a,world)

updateValueAndListMask :: DataPath String a !*World ->(a,ListMask,!*World) | gUpdate{|*|} a
updateValueAndListMask path update a world
	# (a,_,lm,world) = updateValueAndMask path update a [] [] world
	= (a,lm,world) 

updateValueAndMask :: DataPath String a DataMask ListMask !*World -> (a,DataMask,ListMask,!*World)	| gUpdate{|*|} a
updateValueAndMask path update a mask listMask world	
	# (a,ust=:{world,mask,listMask}) = gUpdate{|*|} a {USt| mode = UDSearch, searchPath = path, currentPath = shiftDataPath initialDataPath, consPath = [], update = update, mask = mask, listMask = listMask, world = world}
	= (a,mask,listMask,world)

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
	# (_,ust)	= fx x {USt|ust & currentPath = shiftDataPath currentPath, mask = appendToMask currentPath mask}
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
	= (i, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask}) 
gUpdate{|Int|} i ust = (i,ust)

gUpdate{|Real|} _ ust=:{USt|mode=UDCreate} = (0.0, ust)
gUpdate{|Real|} r ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (toReal update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (r, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Real|} r ust=:{USt|mode=UDMask,currentPath,mask}
	= (r, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask}) 
gUpdate{|Real|} r ust = (r, ust)

gUpdate{|Char|} _ ust=:{USt|mode=UDCreate} = (' ', ust)
gUpdate{|Char|} c ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (if (size update > 0) update.[0] c, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (c, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Char|} c ust=:{USt|mode=UDMask,currentPath,mask}
	= (c, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask}) 
gUpdate{|Char|} c ust = (c, ust)

gUpdate{|Bool|} _ ust=:{USt|mode=UDCreate} = (False, ust)
gUpdate{|Bool|} b ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (update == "true", toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (b, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Bool|} b ust=:{USt|mode=UDMask,currentPath,mask}
	= (b, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask}) 
gUpdate{|Bool|} b ust = (b, ust)

gUpdate{|String|} _ ust=:{USt|mode=UDCreate} = ("", ust)
gUpdate{|String|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|String|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask}) 
gUpdate{|String|} s ust = (s, ust)

import StdDebug
derive gPrint DataPath

//Specialize instance for Dynamic
gUpdate{|Dynamic|} _ ust=:{USt|mode=UDCreate}	= (dynamic 42, ust)
gUpdate{|Dynamic|} d ust						= (d, ust)

gUpdate{|[]|} fx _ ust=:{USt|mode=UDCreate} = ([], ust)

gUpdate{|[]|} fx l ust=:{USt|mode=UDSearch,searchPath,currentPath,update,mask}
	# (l,ust)	= case (isNewElement (dataPathList currentPath) (dataPathList searchPath) (length l)) of
					True
						# (nv,ust=:{USt | listMask}) = fx (abort "LIST - new element accessed during create") {USt | ust & mode=UDCreate}
						= applyListUpdates fx (l++[nv]) {USt | ust & currentPath = shiftDataPath currentPath, mode = UDSearch} //apply updates for new element, because here currentPath =/= searchPath
					False 
						= (l,ust)
	# (lx,ust=:{mask,listMask}) 
				= (applyListUpdates fx l {USt|ust & currentPath = shiftDataPath currentPath})
	# lx 		= removeUnmaskedTail lx (dataPathList currentPath) mask
	| currentPath == searchPath
	# split = split "_" update
	# index = toInt (last split)
	= case hd split of	
		"mup"
			| index == 0 = (lx, {USt | ust & currentPath = stepDataPath currentPath, mode=UDDone})
			# upd   = swapList lx index
			# nm    = swapMask mask currentPath index
			//# lmask = appendToListMask currentPath [index-1,index] listMask
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, mask = nm, mode=UDDone}) 
		"mdn"
			| index >= (length lx)-1 = (lx, {USt | ust & currentPath = stepDataPath currentPath, mode=UDDone})
			# upd	= swapList lx ((toInt index)+1) //down idx == up (idx+1)
			# nm	= swapMask mask currentPath (index+1)
			//# lmask = appendToListMask currentPath [index,index+1] listMask
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, mask=nm, mode=UDDone})
		"rem"
			# upd   = removeAt index lx
			# nm	= (maskRemove mask currentPath index (length lx == (index+1)))
			//# lmask = appendToListMask currentPath [index..length upd-1] listMask
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, mask=nm, mode=UDDone})	
		"add"
			# (nv,ust) = fx (abort "LIST create with undef") {USt | ust & mode=UDCreate}
			# upd	   = insertAt (index+1) nv lx
			# nm	   = moveMaskDown mask currentPath (index+1)
			//# lmask    = appendToListMask currentPath [index+1..length upd-1] listMask
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, mask=nm, mode=UDDone})
		_ 	= (lx, {USt | ust & currentPath = stepDataPath currentPath, mode=UDDone})
	| otherwise
		= (lx, {USt | ust & currentPath = stepDataPath currentPath})
where
	isNewElement cp sp l = sp <== [l:cp] //search path is equal or below [datapath:(length list)]

	removeUnmaskedTail [] cp mask = []
	removeUnmaskedTail l  cp mask
		| lastElementMasked (length l) cp mask = l
		| otherwise = removeUnmaskedTail (init l) cp mask
	where
		lastElementMasked l cp mask = not (isEmpty [m \\ m <- mask | m <== [(l-1):cp]])

	applyListUpdates fx []     ust = ([],ust)
	applyListUpdates fx [l:ls] ust
		# (lx,ust)  = fx l ust
		# (lxs,ust) = applyListUpdates fx ls ust
		= ([lx:lxs],ust)
	
	swapList []	  _		= []
	swapList list index
	| index == 0 			= list //prevent move first element up
	| index >= length list 	= list //prevent move last element down
	| otherwise				
		# f = list !! (index-1)
		# l = list !! (index)
		= updateAt (index-1) l (updateAt index f list)
	
	swapMask mask (DataPath path _) index = [ (swapMask` m path index) \\ m <- mask ]
	where
		swapMask` m path index
		| m <== [index:path] 	 = changeMask [index:path] (-) m
		| m <== [(index-1):path] = changeMask [(index-1):path] (+) m
		| otherwise = m
	
	changeMask prefix fx datapath
	# (unchanged,changed) = splitAt((length datapath-length prefix)) datapath
	# id = hd changed
	# tl = tl prefix
	= unchanged ++ [(fx id 1)] ++ tl
	
	maskRemove mask (DataPath path _) index True		= [m \\ m <- mask | not(m <== [index:path])] //last element, nothing to move up
	maskRemove mask dp=:(DataPath path _) index False	= moveMaskUp [m \\ m <- mask | not(m <== [index:path])] dp (index+1)
	
	moveMaskUp mask (DataPath path _) index = map (\m = if(tlGrEq m [index:path]) (changeMask [index:path] (-) m) (id m)) mask
	
	moveMaskDown mask (DataPath path _) index = map (\m = if(tlGrEq m [index:path]) (changeMask [index:path] (+) m) (id m)) mask
			
	tlGrEq mask path = tlGrEq` (reverse mask) (reverse path)
	tlGrEq` []	   _	  = False
	tlGrEq` [e:ex] [i]	  = (e>=i)
	tlGrEq` [e:ex] [i:ix] = (i==e) && (tlGrEq` ex ix)
	
gUpdate{|[]|} fx l ust=:{USt|mode=UDMask,currentPath,mask}
	# ust = gMarkList fx l {USt|ust & mask = appendToMask currentPath mask,currentPath = shiftDataPath currentPath}
	= (l,{USt | ust & currentPath = stepDataPath currentPath})
where
	gMarkList fx []     ust = ust
	gMarkList fx [x:xs] ust = gMarkList fx xs (snd (fx x ust))
		
gUpdate{|[]|} fx l ust = (l,ust)

//Specialized instance Maybe that chooses the non-recursive constructor 
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

gUpdate{|Maybe|} fx m ust=:{USt|mode=UDMask,currentPath,mask}
	= case m of
		Nothing	= (m, {USt|ust & currentPath = stepDataPath currentPath})
		Just x
			# (_,ust) = fx x ust
			= (m, {USt|ust & currentPath = stepDataPath currentPath})

gUpdate{|Maybe|} fx l ust = (l,ust)

// Document
gUpdate {|Document|} _ ust =: {USt | mode=UDCreate} = (emptyDoc,ust)

gUpdate {|Document|} s ust =: {USt | mode=UDMask,currentPath,mask}
	= (s, {USt | ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})
	
gUpdate {|Document|} s ust =: {USt | mode=UDSearch, searchPath, currentPath, update, mask}
	| currentPath == searchPath
		# upd = fromJSON (fromString update)
		# ust = toggleMask ust
		| isJust upd = (fromJust upd, {USt | ust & currentPath = stepDataPath currentPath, mode=UDDone})
		| otherwise  = (emptyDoc, {USt | ust & currentPath = stepDataPath currentPath}) //abort "[Upd Document] Cannot parse JSON"
		//| otherwise  = ({ Document | fileName = "", size = 0, mimeType = "", taskId = "", index = 0},{USt | ust & currentPath = stepDataPath currentPath})
	| otherwise 
		= (s, {USt | ust & currentPath = stepDataPath currentPath})

gUpdate {|Document|} s ust = (s,ust)

derive gUpdate Either, (,), (,,), (,,,), Void, HtmlDisplay, Editable, Hidden, VisualizationHint

//Utility functions
dp2s :: DataPath -> String
dp2s (DataPath path idx) = (join "-" (map toString (reverse path))) +++ subEditorPostfix
where
	subEditorPostfix = case idx of
		Nothing		= ""
		Just idx	= "_" +++ (toString idx)

dp2id :: String DataPath -> String
dp2id prefix path = prefix +++ "-" +++ dp2s path 

s2dp :: String -> DataPath
s2dp str
	# length				= textSize str
	# postfixIdx			= lastIndexOf "_" str
	# (str,sidx)			= if (postfixIdx == -1)
								(str,Nothing)
								(subString 0 postfixIdx str,Just (toInt (subString (postfixIdx + 1) (length - postfixIdx - 1) str)))
	= DataPath (reverse (map toInt (split "-" str))) sidx

isdps :: String -> Bool
isdps path = and [c == '-' || isDigit c || c == '_' \\ c <-: path]

initialDataPath :: DataPath
initialDataPath = DataPath [] Nothing

stepDataPath :: DataPath -> DataPath
stepDataPath dp=:(DataPath [] _)	= dp
stepDataPath (DataPath [x:xs] sidx)	= DataPath [inc x:xs] sidx

shiftDataPath :: DataPath -> DataPath
shiftDataPath (DataPath path sidx) = DataPath [0:path] sidx

dataPathLevel :: DataPath -> Int
dataPathLevel (DataPath l _) = length l

dataPathHasSubEditorIdx	:: DataPath Int -> Bool
dataPathHasSubEditorIdx (DataPath _ (Just idx0)) idx1	= idx0 == idx1
dataPathHasSubEditorIdx _ _								= False

dataPathSetSubEditorIdx	:: DataPath Int -> DataPath
dataPathSetSubEditorIdx	(DataPath dp _) idx = DataPath dp (Just idx)

instance == DataPath
where
	(==) (DataPath a _) (DataPath b _) = a == b

dataPathList :: DataPath -> [Int]
dataPathList (DataPath list _) = list

// detect whether two paths are equal or if path A is a sub-path of B, assuming reverse-notation. 
// e.g. [1,0] <== [0] 
(<==) infixr 1 :: [Int] [Int] -> Bool
(<==) pathA pathB = tlEq (reverse pathA) (reverse pathB)
where
	tlEq _  	 []		= True
	tlEq [] 	 _ 		= False
	tlEq [a:as] [b:bs] 	= (a == b) && (tlEq as bs)

//Force a field to be masked
setMask :: *USt -> *USt
setMask ust=:{currentPath,mask}
# mask = if (isMasked currentPath mask) mask (appendToMask currentPath mask)
= {ust & mask = mask}

//Masking and unmasking of fields
toggleMask :: *USt -> *USt
toggleMask ust=:{searchPath,currentPath,update,mask}
	| searchPath == currentPath
		| update == ""
			//Remove the current path from the mask
			= {ust & mask = filter (\x -> (dataPathList currentPath) <> x) mask}
		| otherwise
			//Add the current path to the mask (if it is not already in it)
			# mask = if (isMasked currentPath mask) mask (appendToMask currentPath mask)
			//Remove the underlying fields from the mask
			# mask = [m \\ m <- mask | not (childOf currentPath m)]
			= {ust & mask = mask}
	| otherwise
		= ust
where
	//Check if a datapath is a child of another
	childOf (DataPath parent _) child 
		| clen > plen
			= take plen (reverse child) == reverse parent
		| otherwise
			= False
	where
		plen = length parent
		clen = length child

initialDataMask :: DataMask
initialDataMask = []

initialListMask		:: ListMask
initialListMask = []

updateListMask :: ListMask DataPath -> (ListMask,[Int])
updateListMask lmask path
	# nmask = [(dp,idx) \\ (dp,idx) <- lmask | (dataPathList path) == dp]
	= (nmask,if(length nmask > 0) (snd (last nmask)) [])
		
isMasked :: DataPath DataMask -> Bool
isMasked (DataPath dp _) dm = isMember dp dm

appendToMask :: DataPath DataMask -> DataMask
appendToMask (DataPath l _) mask = [l:mask]

appendToListMask :: DataPath [Int] ListMask -> ListMask
appendToListMask (DataPath path _) ints lmask = [(path,ints):lmask]