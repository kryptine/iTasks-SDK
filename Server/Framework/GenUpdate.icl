implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, Maybe, StdGeneric, StdEnum
import Types, Text, Util, Shared, DocumentDB
from StdFunc import id, o

derive bimap (,), UpdateMode

:: DataPath = DataPath [Int]

defaultValue :: !*IWorld -> (!a,!*IWorld) | gUpdate{|*|} a
defaultValue iworld  
	# (a,ust=:{iworld}) = gUpdate{|*|} UDCreate {searchPath = emptyDataPath, currentPath = emptyDataPath, consPath = [], update = "", oldMask = [], newMask = [], iworld = iworld}
	= (a,iworld)

defaultMask :: !a -> UpdateMask | gDefaultMask{|*|} a	
defaultMask a = hd (gDefaultMask{|*|} a)

updateValue	:: DataPath String a !*IWorld -> (a,!*IWorld) | gUpdate{|*|} a 	
updateValue path update a iworld
	# (a,_,iworld) = updateValueAndMask path update a Untouched iworld
	= (a,iworld)

updateValueAndMask :: DataPath String a UpdateMask !*IWorld -> (a,UpdateMask,!*IWorld) | gUpdate{|*|} a
updateValueAndMask path update a oldMask iworld	
	# (a,ust=:{newMask,iworld}) = gUpdate{|*|} (UDSearch a) {searchPath = path, currentPath = startDataPath, consPath = [], update = update, oldMask = [oldMask], newMask = [], iworld = iworld}
	= (a,hd newMask,iworld)

appIWorldUSt :: !.(*IWorld -> *IWorld)!*USt -> *USt
appIWorldUSt f ust=:{iworld}
	= {ust & iworld = f iworld}
	
accIWorldUSt :: !.(*IWorld -> *(.a,*IWorld))!*USt -> (.a,!*USt)
accIWorldUSt f ust=:{iworld}
	# (a,iworld) = f iworld
	= (a,{ust & iworld = iworld})

//Generic updater
generic gUpdate a :: !(UpdateMode a) !*USt -> (!a,!*USt)

gUpdate{|UNIT|} _ ust = (UNIT,ust)

gUpdate{|OBJECT|} fx UDCreate ust=:{newMask}
	//Empty object mask
	# (nx,ust=:{newMask=objectMask}) = fx UDCreate {ust & newMask = []}
	= (OBJECT nx, {ust & newMask = newMask ++ objectMask})
gUpdate{|OBJECT of d|} fx (UDSearch (OBJECT x)) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		//Update is a constructor switch
		# (nx,ust) = fx UDCreate {ust & consPath = path}
		= (OBJECT nx, {ust & oldMask = om, currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update)}) 
	| searchPath <== currentPath
		//Update is targeted somewhere in a substructure of this value
		# (nx,ust=:{newMask=childMask}) = fx (UDSearch x) {ust & currentPath = shiftDataPath currentPath, oldMask = childMasks cm, newMask = []}
		= (OBJECT nx, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask (Touched childMask)})
	| otherwise
		//Not on the path, so just put back the current mask (cm)	
		= (OBJECT x, {ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}) 
where
	path = case [cons \\ cons <- d.gtd_conses | cons.gcd_name == update] of
		[cons]	= getConsPath cons
		_		= []
		
gUpdate{|CONS|}		fx UDCreate				ust = appFst CONS	(fx UDCreate ust)
gUpdate{|CONS|}		fx (UDSearch (CONS c))	ust = appFst CONS	(fx (UDSearch c) ust)
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
			
gUpdate{|Maybe|} _ UDCreate ust=:{newMask}
	//Specialized instance Maybe that chooses the non-recursive constructor 
	= (Nothing,{ust & newMask = appendToMask newMask Untouched})
gUpdate{|Maybe|} fx (UDSearch m) ust=:{currentPath,searchPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath && update == ""	
		= (Nothing, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask Blanked, oldMask = om}) //Reset
	| otherwise
		= case m of
			Nothing
				| searchPath <== currentPath
					// Create a default value
					# (x,ust=:{newMask=nmCreate}) 	= fx UDCreate ust
					// Search in the default value
					# (x,ust=:{newMask=nmSearch}) 	= fx (UDSearch x) {ust & currentPath = currentPath, searchPath = searchPath, update = update, oldMask = oldMask, newMask = newMask}
					= (Just x, ust)
				| otherwise
					= (Nothing, {ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
			Just x
				# (x,ust) = fx (UDSearch x) ust //all mask transformations are made here..
				= (Just x,ust)

gUpdate{|[]|} _ UDCreate ust = basicCreate [] ust
gUpdate{|[]|} fx (UDSearch l) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# (l,listMask,ust)
		= case isNew currentPath searchPath (length l) of
			True
				# (nv,ust) = fx UDCreate {ust & oldMask = [], newMask = []}
				= (l++[nv], Touched [], ust)
			False
				= (l,cm,ust)
	# (lx,ust=:{newMask=cMasks})
		= updateElements fx l {ust & currentPath = shiftDataPath currentPath, oldMask = childMasks cm, newMask = []}
	# ust = {ust & newMask = cMasks}
	| currentPath == searchPath
		//Process the reordering commands 
		# split = split "_" update
		# index = toInt (last split)
		# (lx,cMasks,ust) = case hd split of	
			"mup" = (swap lx index,swap cMasks index,ust) 
			"mdn" = (swap lx (index+1),swap cMasks (index+1),ust)
			"rem" = (removeAt index lx,removeAt index cMasks,ust)	
			"add"
				# (nv,ust=:{newMask=childMask}) = fx UDCreate {ust & oldMask = [], newMask = []}
				= (insertAt (index+1) nv lx,insertAt (index+1) (hd childMask) cMasks,{ust & newMask = childMask})
			_ 	
				= (lx,cMasks,ust)
		= (lx,{ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched cMasks), oldMask = om})
	| otherwise
		# listMask = case listMask of
			Touched _	= Touched cMasks
			mask		= mask
		= (lx, {ust & currentPath  = stepDataPath currentPath, newMask = appendToMask newMask listMask, oldMask = om})
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

gUpdate{|Display|}	fx mode										ust = wrapperUpdate fx mode fromDisplay Display ust
gUpdate{|Editable|}	fx mode										ust = wrapperUpdate fx mode fromEditable Editable ust
gUpdate{|Hidden|}	fx mode										ust = wrapperUpdate fx mode fromHidden Hidden ust
gUpdate {|VisualizationHint|} fx UDCreate						ust = wrapperUpdate fx UDCreate undef VHEditable ust 
gUpdate {|VisualizationHint|} fx m=:(UDSearch (VHEditable s))	ust = wrapperUpdate fx m fromVisualizationHint VHEditable ust
gUpdate {|VisualizationHint|} fx m=:(UDSearch (VHDisplay s))	ust = wrapperUpdate fx m fromVisualizationHint VHDisplay ust
gUpdate {|VisualizationHint|} fx m=:(UDSearch (VHHidden s))		ust = wrapperUpdate fx m fromVisualizationHint VHHidden ust

wrapperUpdate fx mode get cons ust=:{currentPath} = case mode of
	UDCreate
		= appFst cons (fx UDCreate ust)
	UDSearch w
		# (w,ust) = fx (UDSearch (get w)) ust
		= (cons w,{ust & currentPath = stepDataPath currentPath})

gUpdate{|Int|}				mode ust = basicUpdateSimple mode toInt 0 ust
gUpdate{|Real|}				mode ust = basicUpdateSimple mode toReal 0.0 ust
gUpdate{|Char|}				mode ust = basicUpdateSimple mode (\str -> if (size str > 0) str.[0] ' ') ' ' ust
gUpdate{|Bool|}				mode ust = basicUpdateSimple mode ((==) "true") False ust
gUpdate{|String|}			mode ust = basicUpdateSimple mode id "" ust
gUpdate{|Note|}				mode ust = basicUpdateSimple mode Note (Note "") ust
gUpdate{|Password|}			mode ust = basicUpdateSimple mode Password (Password "") ust
gUpdate{|User|}				mode ust = basicUpdateSimple mode (\str -> if (userName (NamedUser str) == "root") RootUser (NamedUser str)) AnyUser ust
gUpdate{|FormButton|}		mode ust = basicUpdate mode (\str b -> {b & state = if (str == "true") Pressed NotPressed}) {FormButton | label = "Form Button", icon="", state = NotPressed} ust
gUpdate{|Tree|} _			mode ust = basicUpdate mode (\str (Tree nodes _) -> Tree nodes (toInt str)) (Tree [] -1) ust
gUpdate{|Choice|} _			mode ust = basicUpdate mode (\str c=:(Choice opts _) -> case fromJSON (fromString str) of Just [i] = Choice opts i; _ = Choice opts -1) (Choice [] -1) ust
gUpdate{|MultipleChoice|} _	mode ust = basicUpdate mode (\str c=:(MultipleChoice opts _) -> case fromJSON (fromString str) of Just s = MultipleChoice opts s; _ = c) (MultipleChoice [] []) ust
gUpdate{|Currency|}			mode ust = basicUpdate mode parseUpdate (EUR 0) ust
where
	parseUpdate update orig = case split "." update of
		[whole]		= replaceVal orig (100 * toInt whole)
		[whole,dec] = replaceVal orig (100 * toInt whole + (if (size dec == 1) (10 * toInt dec) (toInt (dec % (0,1)))))
		_			= orig
	replaceVal (EUR _) x = (EUR x)
	replaceVal (GBP _) x = (GBP x)
	replaceVal (USD _) x = (USD x)
	replaceVal (JPY _) x = (JPY x)

gUpdate{|Date|} UDCreate ust=:{iworld}
	# (date,iworld) = currentDate iworld
	= basicCreate date {ust & iworld = iworld}
gUpdate{|Date|} (UDSearch d) ust = basicSearch d (\str _ -> fromString str) ust
gUpdate{|Time|} UDCreate ust=:{iworld}
	# (time,iworld) = currentTime iworld
	= basicCreate time {ust & iworld = iworld}
gUpdate{|Time|} (UDSearch t) ust = basicSearch t (\str _ -> fromString str) ust

gUpdate{|Dynamic|}			mode ust = basicUpdate mode unchanged (dynamic 42) ust
gUpdate{|(->)|} _ _			mode ust = basicUpdate mode unchanged (abort "default function") ust
gUpdate{|Shared|} _ _		mode ust = basicUpdate mode unchanged (abort "default shared") ust
unchanged _ v = v

gUpdate {|Document|} UDCreate ust = basicCreate {Document|documentId = "", name="", mime="", size = 0} ust
gUpdate {|Document|} (UDSearch s) ust=:{searchPath, currentPath, update, oldMask, newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		| update == "" // Reset
			= ({Document|documentId = "", name="", mime="", size = 0},{ust & newMask = appendToMask newMask Blanked})
		| otherwise // Look up meta-data in the store and update the document
			# (mbDocument,ust)	= getDocument update ust
			# ust				= {ust & newMask = appendToMask newMask (Touched [])}
			= (fromMaybe s mbDocument,ust)
	| otherwise 
		= (s, {ust & newMask = appendToMask newMask cm})

derive gUpdate Either, (,), (,,), (,,,), Void, DateTime, UserDetails, Timestamp, Map, EmailAddress, Action, ProcessRef, TreeNode, Table

basicUpdateSimple :: !(UpdateMode a) (String -> a) a !*USt -> *(!a,!*USt)
basicUpdateSimple mode toV def ust = case mode of
	UDCreate	= basicCreate def ust
	UDSearch v	= basicSearch v (\str _ -> toV str) ust
	
basicUpdate :: !(UpdateMode a) (String a -> a) a !*USt -> *(!a,!*USt)
basicUpdate mode toV def ust = case mode of
	UDCreate	= basicCreate def ust
	UDSearch v	= basicSearch v toV ust
	
basicCreate :: !a !*USt -> *(!a,!*USt)
basicCreate def ust=:{newMask} = (def,{ust & newMask = appendToMask newMask Untouched})

basicSearch :: a (String a -> a) !*USt -> *(!a,!*USt)
basicSearch v toV ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm, om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= (toV update v, {ust & newMask = appendToMask newMask (toggleMask update)})
	| otherwise
		= (v, {ust & newMask = appendToMask newMask cm})

generic gDefaultMask a :: !a -> [UpdateMask]

gDefaultMask{|UNIT|} _						= []
gDefaultMask{|OBJECT|}	fx (OBJECT x)		= [Touched (fx x)]
gDefaultMask{|CONS|}	fx (CONS x)			= fx x
gDefaultMask{|FIELD|}	fx (FIELD x)		= fx x
gDefaultMask{|PAIR|}	fx fy (PAIR x y)	= fx x ++ fy y
gDefaultMask{|EITHER|}	fx fy e = case e of
	(LEFT x)								= fx x
	(RIGHT y)								= fy y

gDefaultMask{|Maybe|} fx mbVal = maybe [Untouched] fx mbVal

gDefaultMask{|[]|} _ [] = [Untouched]
gDefaultMask{|[]|} fx l = [Touched (map (hd o fx) l)]

gDefaultMask {|Display|}			fx (Display d)		= fx d
gDefaultMask {|Editable|}			fx (Editable e)		= fx e
gDefaultMask {|Hidden|}				fx (Hidden h)		= fx h
gDefaultMask {|VisualizationHint|}	fx (VHEditable e)	= fx e
gDefaultMask {|VisualizationHint|}	fx (VHDisplay d)	= fx d
gDefaultMask {|VisualizationHint|}	fx (VHHidden h)		= fx h

gDefaultMask{|Int|}					_ = [Touched []]
gDefaultMask{|Real|}				_ = [Touched []]
gDefaultMask{|Char|}				_ = [Touched []]
gDefaultMask{|Bool|}				_ = [Touched []]
gDefaultMask{|String|}				_ = [Touched []]
gDefaultMask{|Dynamic|}				_ = [Touched []]
gDefaultMask{|(->)|} _ _			_ = [Touched []]
gDefaultMask{|Document|}			_ = [Touched []]			
gDefaultMask{|FormButton|}			_ = [Touched []]
gDefaultMask{|Note|}				_ = [Touched []]
gDefaultMask{|Password|}			_ = [Touched []]
gDefaultMask{|Currency|}			_ = [Touched []]
gDefaultMask{|Date|}				_ = [Touched []]
gDefaultMask{|Time|}				_ = [Touched []]
gDefaultMask{|User|}				_ = [Touched []]
gDefaultMask{|MultipleChoice|}_		_ = [Touched []]
gDefaultMask{|Shared|} _ _			_ = [Touched []]
gDefaultMask{|Choice|} _ (Choice opts sel)
	// if no valid selection is made, start with untouched mask
	| sel >= 0 && sel < length opts	= [Touched []]
	| otherwise						= [Untouched]
gDefaultMask{|Tree|} _ tree=:(Tree _ sel)
	// if no valid selection is made, start with untouched mask
	| sel >= 0	= [Touched []]
	| otherwise	= [Untouched]

derive gDefaultMask Either, (,), (,,), (,,,), Void, DateTime, UserDetails, Timestamp, Map, EmailAddress, Action, ProcessRef, TreeNode, Table

//Utility functions
dp2s :: !DataPath -> String
dp2s (DataPath path) = join "-" (map toString (reverse path))

dp2id :: !String !DataPath -> String
dp2id prefix path = prefix +++ "-" +++ dp2s path 

s2dp :: !String -> DataPath
s2dp str = DataPath (reverse (map toInt (split "-" str)))

isdps :: !String -> Bool
isdps path = and [c == '-' || isDigit c || c == '_' \\ c <-: path]

startDataPath :: DataPath
startDataPath = DataPath [0]

emptyDataPath :: DataPath
emptyDataPath = DataPath []

stepDataPath :: !DataPath -> DataPath
stepDataPath dp=:(DataPath [])	= dp
stepDataPath (DataPath [x:xs])	= DataPath [inc x:xs]

shiftDataPath :: !DataPath -> DataPath
shiftDataPath (DataPath path) = DataPath [0:path]

childDataPath :: !DataPath !Int -> DataPath
childDataPath (DataPath path) i = DataPath [i:path]

dataPathLevel :: !DataPath -> Int
dataPathLevel (DataPath l) = length l

instance == DataPath
where
	(==) (DataPath a) (DataPath b) = a == b

dataPathList :: !DataPath -> [Int]
dataPathList (DataPath list) = list

dataPathFromList :: ![Int] -> DataPath
dataPathFromList l = DataPath l

// detect whether two paths are equal or if path A is a sub-path of B, assuming reverse-notation. 
// e.g. [1,0] <== [0] 
(<==) infixr 1 :: !DataPath !DataPath -> Bool
(<==) (DataPath pathA) (DataPath pathB) = tlEq (reverse pathA) (reverse pathB)
where
	tlEq _  	 []		= True
	tlEq [] 	 _ 		= False
	tlEq [a:as] [b:bs] 	= (a == b) && (tlEq as bs)

//Masking and unmasking of fields
toggleMask :: !String -> UpdateMask
toggleMask update
	| update == ""	= Blanked
	| otherwise		= Touched []

instance GenMask UpdateMask
where
	popMask :: ![UpdateMask] -> (!UpdateMask, ![UpdateMask])
	popMask []			= (Untouched, [])
	popMask [c:cm]		= (c,cm)

	appendToMask :: ![UpdateMask] !UpdateMask -> [UpdateMask]
	appendToMask l m	= l ++ [m]

	childMasks :: !UpdateMask -> [UpdateMask]
	childMasks (Untouched)		= []
	childMasks (Touched  cm)	= cm
	childMasks (Blanked)		= []
	
	isTouched :: !UpdateMask -> Bool
	isTouched (Touched _)	= True
	isTouched _				= False

allUntouched :: ![UpdateMask] -> Bool
allUntouched children = and [isUntouched c \\ c <- children]
where
	isUntouched Untouched	= True
	isUntouched _			= False

allBlanked :: ![UpdateMask] -> Bool
allBlanked children = and [isBlanked c \\ c <- children]
where
	isBlanked Blanked   = True
	isBlanked _			= False
