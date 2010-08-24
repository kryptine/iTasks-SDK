implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, StdEnum
import Void, Either
import Text
import JSON
import Types
import DocumentDB

from StdFunc import id

derive JSONEncode UpdateMask
derive JSONDecode UpdateMask

derive bimap	(,), Maybe

:: DataPath = DataPath [Int] (Maybe SubEditorIndex)

defaultValue :: !*IWorld -> (!a,!*IWorld) | gUpdate{|*|} a
defaultValue iworld  
	# (a,ust=:{USt|iworld}) = gUpdate{|*|} (abort "gUpdate accessed value during create") {USt|mode = UDCreate, searchPath = initialDataPath, currentPath = initialDataPath, consPath = [], update = "", oldMask = initialUpdateMask, newMask = initialUpdateMask, iworld = iworld}
	= (a,iworld)

defaultMask :: a !*IWorld -> (!UpdateMask,!*IWorld) | gUpdate{|*|} a	
defaultMask a iworld
	# (_,ust=:{newMask,iworld}) = gUpdate{|*|} a {USt| mode = UDMask, searchPath = initialDataPath, currentPath = shiftDataPath initialDataPath, consPath = [], update = "", oldMask = initialUpdateMask, newMask = initialUpdateMask, iworld = iworld}
	= (newMask,iworld)

updateValue	:: DataPath String a !*IWorld -> (a,!*IWorld) | gUpdate{|*|} a 	
updateValue path update a iworld
	# (a,_,iworld) = updateValueAndMask path update a initialUpdateMask iworld
	= (a,iworld)

updateValueAndMask :: DataPath String a UpdateMask !*IWorld -> (a,UpdateMask,!*IWorld) | gUpdate{|*|} a
updateValueAndMask path update a oldMask iworld	
	# (a,ust=:{newMask,iworld}) = gUpdate{|*|} a {USt| mode = UDSearch, searchPath = path, currentPath = shiftDataPath initialDataPath, consPath = [], update = update, oldMask = oldMask, newMask = initialUpdateMask, iworld = iworld}
	= (a,newMask,iworld)

appIWorldUSt :: !.(*IWorld -> *IWorld)!*USt -> *USt
appIWorldUSt f ust=:{USt|iworld}
	= {USt|ust & iworld = f iworld}
	
accIWorldUSt :: !.(*IWorld -> *(.a,*IWorld))!*USt -> (.a,!*USt)
accIWorldUSt f ust=:{USt|iworld}
	# (a,iworld) = f iworld
	= (a,{USt|ust & iworld = iworld})

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

gUpdate{|OBJECT|} fx _ ust=:{mode=UDCreate,newMask}
	//Empty object mask
	# objectMask = Untouched False []
	# (nx,ust=:{newMask=objectMask}) = fx (abort "OBJECT create with undef") {USt | ust & newMask = objectMask}
	= (OBJECT nx, {USt | ust & newMask = appendToMask newMask objectMask})

gUpdate{|OBJECT of d|} fx o ust=:{mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# objectMask = case cm of
		(Untouched d _) = Untouched d []
		(Touched d _)   = Touched d []
		(Blanked d _)   = Blanked d []
	| currentPath == searchPath
		# (nx,ust) = fx (abort "OBJECT create with undef") {USt|ust & mode = UDCreate, consPath = path}
		= (OBJECT nx, {USt|ust & oldMask = om, currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), mode = UDSearch}) 
	| otherwise
		# (nx,ust=:{newMask=childMask}) = fx x {USt|ust & currentPath = shiftDataPath currentPath, oldMask = cm, newMask = objectMask}
		= (OBJECT nx, {USt|ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask childMask})
where
	(OBJECT x) = o

	path = case [cons \\ cons <- d.gtd_conses | cons.gcd_name == update] of
		[cons]	= getConsPath cons
		_		= []

gUpdate{|OBJECT of d|} fx o ust=:{mode=UDMask,currentPath,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# (_,ust=:{newMask = childMask}) = fx x {USt|ust & currentPath = shiftDataPath currentPath, oldMask = cm, newMask = (Touched False [])}
	= (o,{USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask childMask})
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
//**********************************************************************************************************
gUpdate{|Int|} _ ust=:{USt|mode=UDCreate,newMask} //newMask == mask of the parent component
	= (0,{USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Int|} i ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask	
	| currentPath == searchPath
		= (toInt update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (i, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Int|} i ust=:{USt|mode=UDMask,currentPath,newMask}
	= (i, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
gUpdate{|Int|} i ust = (i,ust)
//***
gUpdate{|Real|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (0.0, {USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Real|} r ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= (toReal update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (r, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Real|} r ust=:{USt|mode=UDMask,currentPath,newMask}
	= (r, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
gUpdate{|Real|} r ust = (r, ust)
//***
gUpdate{|Char|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (' ', {USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Char|} c ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= (if (size update > 0) update.[0] c, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (c, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Char|} c ust=:{USt|mode=UDMask,currentPath,newMask}
	= (c, {USt|ust & currentPath = stepDataPath currentPath, newMask = Touched True []}) 
gUpdate{|Char|} c ust = (c, ust)
//***
gUpdate{|Bool|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (False, {USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Bool|} b ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= (update == "true", {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (b, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Bool|} b ust=:{USt|mode=UDMask,currentPath,newMask}
	= (b, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
gUpdate{|Bool|} b ust = (b, ust)
//***
gUpdate{|String|} _ ust=:{USt|mode=UDCreate,newMask} 
	= ("", {USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|String|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= (update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|String|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
gUpdate{|String|} s ust = (s, ust)

//Specialize instance for Dynamic
gUpdate{|Dynamic|} _ ust=:{USt|mode=UDCreate}	= (dynamic 42, ust)
gUpdate{|Dynamic|} d ust						= (d, ust)

gUpdate{|[]|} fx _ ust=:{USt|mode=UDCreate,newMask} 
	= ([], {USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|[]|} fx l ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# (l,listMask,cm,ust) = case isNewElement (dataPathList currentPath) (dataPathList searchPath) (length l) of
							True
								# (child,ust=:{newMask=childMask}) = fx (abort "LIST - new element accessed during create") {USt | ust & mode=UDCreate}
								=(l++[child], UMList[length l][], appendToMask cm childMask, {USt | ust & newMask = childMask})
							False
								= (l,UMList [] [],cm,ust)
	# (lx,ust=:{newMask=listMask}) = applyListUpdates fx l {USt|ust & mode = UDSearch, currentPath = shiftDataPath currentPath, oldMask = cm, newMask = listMask}
	| currentPath == searchPath
	# split = split "_" update
	# index = toInt (last split)
	= case hd split of	
		"mup"
			| index == 0 = (lx, {USt | ust & currentPath = stepDataPath currentPath})
			# upd   = swapList lx index
			# nm    = swapMask index listMask
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask nm, oldMask = om}) 
		"mdn"
			| index >= (length lx)-1 = (lx, {USt | ust & currentPath = stepDataPath currentPath})
			# upd	= swapList lx (index+1) //down idx == up (idx+1)
			# nm	= swapMask (index+1) listMask
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask nm, oldMask = om})
		"rem"
			# upd   = removeAt index lx
			# nm	= maskRemove index listMask
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask nm, oldMask = om})	
		"add"
			# (nv,ust=:{newMask=childMask}) 
					   = fx (abort "LIST create with undef") {USt | ust & mode=UDCreate}
			# upd	   = insertAt (index+1) nv lx
			# nm	   = maskInsert (index+1) childMask listMask
			= (upd, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask nm, oldMask = om})
		_ 	
			= (lx, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask listMask, oldMask = om})
	| otherwise 
		= (lx, {USt | ust & currentPath  = stepDataPath currentPath, newMask = appendToMask newMask listMask, oldMask = om})
where
	isNewElement cp sp l = sp <== [l:cp] //search path is equal or below [datapath:(length list)]

	applyListUpdates fx []     ust 
		= ([],ust)
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
	
	swapMask index (UMList _ c)
	# f = c !! (index-1)
	# l = c !! (index)
	= UMList [index-1,index] (updateAt (index-1) l (updateAt index f c))
	
	maskRemove index (UMList _ c)
	= UMList [index..length c-1] (removeAt index c)
	
	maskInsert index cm (UMList _ c)
	= UMList [index+1..length c-1] (insertAt index cm c)
	
gUpdate{|[]|} fx l ust=:{USt|mode=UDMask,currentPath,newMask}
# ust=:{newMask=childMask} = gMarkList fx l {USt | ust & currentPath = shiftDataPath currentPath, newMask = Touched True []}
= (l,{USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask childMask})
where
	gMarkList fx [] ust = ust
	gMarkList fx [x:xs] ust
		# (_,ust) = fx x ust
		= gMarkList fx xs ust
		
gUpdate{|[]|} fx l ust = (l,ust)

//Specialized instance Maybe that chooses the non-recursive constructor 
gUpdate{|Maybe|} fx _ ust=:{USt|mode=UDCreate,newMask} 
	= (Nothing,{USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Maybe|} fx m ust=:{USt|mode=UDSearch,currentPath,searchPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath && update == ""	
		= (Nothing, {USt | ust & newMask = appendToMask newMask (Blanked True []), oldMask = om}) //Reset
	| otherwise
		= case m of
			Nothing
				| (dataPathList searchPath) <== (dataPathList currentPath)
					# (x,ust=:{newMask=nmCreate}) 	= fx (abort "Maybe create with undef") {ust & mode = UDCreate}
					# (x,ust=:{newMask=nmSearch}) 	= fx x {ust & mode = UDSearch, currentPath = currentPath, searchPath = searchPath, update = update, oldMask = oldMask, newMask = newMask}
					# children						= getMaskChildren nmSearch
					| or (map isDirtyUM children)	= (Just x, ust)
					| otherwise						= (Nothing, {USt | ust & newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
				| otherwise
					= (Nothing, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
			Just x
				# (x,ust) = fx x ust //all mask transformations are made here..
				= (Just x,ust)
gUpdate{|Maybe|} fx m ust=:{USt|mode=UDMask,currentPath,newMask}
	= case m of
		Nothing	= (m, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Blanked True [])}) //Untouched or Blanked??
		Just x
			# (_,ust) = fx x ust //all mask transformations are made here..
			= (m,ust)
gUpdate{|Maybe|} fx l ust = (l,ust)

// Document

gUpdate {|Document|} _ ust =: {USt | mode=UDCreate,newMask} 
	= ({Document|documentId = "", name="", mime="", size = 0},{USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate {|Document|} s ust =: {USt | mode=UDMask,currentPath,newMask}
	= (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])})
gUpdate {|Document|} s ust =: {USt | mode=UDSearch, searchPath, currentPath, update, oldMask, newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		| update == "" // Reset
			= ({Document|documentId = "", name="", mime="", size = 0},{USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Blanked True []), oldMask = om})
		| otherwise // Look up meta-data in the store and update the document
			# (mbDocument,ust) = getDocument update ust
			= case mbDocument of
				Just document 	= (document,{USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True []), oldMask = om})
				Nothing			= (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True []), oldMask = om})
	| otherwise 
		= (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
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

//Masking and unmasking of fields
toggleMask :: !String -> UpdateMask
toggleMask update
	| update == "" = Blanked True []
	| otherwise	= Touched True []

cleanUpdMask :: !UpdateMask -> UpdateMask
cleanUpdMask m = markUpdMask m False

dirtyUpdMask :: !UpdateMask -> UpdateMask
dirtyUpdMask m = markUpdMask m True

markUpdMask :: !UpdateMask !Bool -> UpdateMask
markUpdMask (Untouched _ u) d = Untouched d u
markUpdMask (Touched _ u)	d = Touched d u
markUpdMask (Blanked _ u)	d = Blanked d u
markUpdMask (UMList i u) 	d = UMList i u

instance GenMask UpdateMask
where
	popMask :: !UpdateMask -> (!UpdateMask, !UpdateMask)
	popMask (Untouched d [c:cm])	= (c, Untouched d cm)
	popMask (Touched d [c:cm])		= (c, Touched d cm)
	popMask (Blanked d [c:cm])		= (c, Blanked d cm)
	popMask (UMList i [c:cm])		= (c, UMList i cm)
	popMask m				 		= (Untouched False [], m)

	appendToMask :: !UpdateMask !UpdateMask -> UpdateMask
	appendToMask (Untouched d m) cm = (Untouched (isDirty d cm) (m++[cm]))
	appendToMask (Touched d m)   cm = (Touched (isDirty d cm) (m++[cm]))
	appendToMask (Blanked d m)   cm = (Blanked (isDirty d cm) (m++[cm]))
	appendToMask (UMList i m)	 cm = (UMList i (m++[cm]))
		
	getMaskChildren :: !UpdateMask -> [UpdateMask]
	getMaskChildren (Untouched _ c) = c
	getMaskChildren (Touched _ c) = c
	getMaskChildren (Blanked _ c) = c
	getMaskChildren (UMList _ c) = c

isDirty d cm 
| isDirtyUM cm = True
			   = d
			   
isDirtyUM :: !UpdateMask -> Bool
isDirtyUM (Untouched d _) = d
isDirtyUM (Touched d _) = d
isDirtyUM (Blanked d _) = d
isDirtyUM (UMList i _) = not (isEmpty i)

initialUpdateMask :: UpdateMask
initialUpdateMask = Untouched False []
