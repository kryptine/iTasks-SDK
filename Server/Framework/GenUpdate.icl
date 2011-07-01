implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, Maybe, StdGeneric, StdEnum
import SystemTypes, Text, Util, DocumentDB
from StdFunc import id, const, o
from TUIDefinition import :: TUISize(..), :: TUIFixedSize, :: TUIWeight

derive bimap (,), UpdateMode

:: DataPath = DataPath [Int]

defaultValue :: a | gUpdate{|*|} a
defaultValue = fst (gUpdate{|*|} UDCreate {searchPath = emptyDataPath, currentPath = emptyDataPath, consPath = [], update = JSONNull, oldMask = [], newMask = [], iworld = Nothing})

defaultMask :: !a -> UpdateMask | gDefaultMask{|*|} a	
defaultMask a = hd (gDefaultMask{|*|} a)
derive JSONEncode DataPath
updateValueAndMask :: !DataPath !JSONNode !a !UpdateMask !*IWorld -> (!a,!UpdateMask,!*IWorld) | gUpdate{|*|} a
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
	path = case update of
		JSONString cname = case [cons \\ cons <- d.gtd_conses | cons.gcd_name == cname] of
			[cons]	= getConsPath cons
			_		= []
		_			= []
		
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
	| currentPath == searchPath && (update === JSONNull || update === JSONBool False)
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
		# split = split "_" (fromMaybe "" (fromJSON update))
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

gUpdate{|Display|}				fx mode										ust = wrapperUpdate fx mode fromDisplay Display ust
gUpdate{|Editable|}				fx mode										ust = wrapperUpdate fx mode fromEditable Editable ust
gUpdate{|Hidden|}				fx mode										ust = wrapperUpdate fx mode fromHidden Hidden ust
gUpdate{|VisualizationHint|} 	fx UDCreate									ust = wrapperUpdate fx UDCreate undef VHEditable ust 
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHEditable s))			ust = wrapperUpdate fx mode fromVisualizationHint VHEditable ust
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHDisplay s))			ust = wrapperUpdate fx mode fromVisualizationHint VHDisplay ust
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHHidden s))			ust = wrapperUpdate fx mode fromVisualizationHint VHHidden ust
gUpdate{|ControlSize|} 			fx UDCreate									ust = wrapperUpdate fx UDCreate undef (ControlSize Auto Auto Nothing) ust 
gUpdate{|ControlSize|}			fx mode=:(UDSearch (ControlSize w h m _))	ust = wrapperUpdate fx mode fromControlSize (ControlSize w h m) ust
gUpdate{|FillControlSize|}		fx mode										ust = wrapperUpdate fx mode fromFillControlSize FillControlSize ust
gUpdate{|FillWControlSize|}		fx mode										ust = wrapperUpdate fx mode fromFillWControlSize FillWControlSize ust
gUpdate{|FillHControlSize|}		fx mode										ust = wrapperUpdate fx mode fromFillHControlSize FillHControlSize ust

wrapperUpdate fx mode get cons ust=:{USt|currentPath} = case mode of
	UDCreate
		= appFst cons (fx UDCreate ust)
	UDSearch w
		# (w,ust) = fx (UDSearch (get w)) ust
		= (cons w,{USt|ust & currentPath = stepDataPath currentPath})

gUpdate{|Int|}				mode ust = basicUpdateSimple mode 0 ust
gUpdate{|Real|}				mode ust = basicUpdateSimple mode 0.0 ust
gUpdate{|Char|}				mode ust = basicUpdateSimple mode ' ' ust
gUpdate{|Bool|}				mode ust = basicUpdateSimple mode False ust
gUpdate{|String|}			mode ust = basicUpdateSimple mode "" ust
gUpdate{|Note|}				mode ust = basicUpdateSimple mode (Note "") ust
gUpdate{|Password|}			mode ust = basicUpdateSimple mode (Password "") ust
gUpdate{|User|}				mode ust = basicUpdateSimple mode AnyUser ust
gUpdate{|HtmlDisplay|}		mode ust = basicUpdate mode unchanged (HtmlDisplay "") ust
gUpdate{|FormButton|}		mode ust = basicUpdate mode (\st b							-> {b & state = st})						{FormButton | label = "Form Button", icon="", state = NotPressed}	ust
gUpdate{|Table|}			mode ust = basicUpdate mode (\json (Table headers cells _)	-> case fromJSON json of Just i = Table headers cells (Just i); _ = Table headers cells Nothing) (Table [] [] Nothing) ust
gUpdate{|Tree|} _			mode ust = basicUpdate mode (\json (Tree nodes _)			-> case fromJSON json of Just i = Tree nodes i; _ = Tree nodes -1) (Tree [] -1) ust
gUpdate{|Choice|} _			mode ust = basicUpdate mode (\l c=:(Choice opts _)			-> case l of [i] = Choice opts i; _ = c)	(Choice [] -1)														ust
gUpdate{|MultipleChoice|} _	mode ust = basicUpdate mode (\sel (MultipleChoice opts _)	-> MultipleChoice opts sel)					(MultipleChoice [] [])												ust
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

gUpdate{|Date|} UDCreate ust = basicCreate {day = 1, mon = 1, year = 1970} ust
gUpdate{|Date|} (UDSearch d) ust = basicSearch d (\json old -> fromMaybe old (fromJSON json)) ust
gUpdate{|Time|} UDCreate ust = basicCreate {hour = 0, min = 0, sec = 0} ust
gUpdate{|Time|} (UDSearch t) ust = basicSearch t (\json old -> fromMaybe old (fromJSON json)) ust

gUpdate{|Dynamic|}		mode ust = basicUpdate mode unchanged (dynamic 42) ust
gUpdate{|(->)|} _ fy	mode ust
	# (def,ust) = fy UDCreate ust
	= basicUpdate mode unchanged (const def) ust
gUpdate{|WorkflowTaskContainer|} mode ust = basicUpdate mode unchanged (WorkflowTaskContainer defTask) ust
where
	defTask :: Task Void
	defTask = abort "default task container"

gUpdate {|Document|} UDCreate ust = basicCreate {Document|documentId = "", name="", mime="", size = 0} ust
gUpdate {|Document|} (UDSearch s) ust=:{searchPath, currentPath, update, oldMask, newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= case fromJSON update of
			Nothing // Reset
				= ({Document|documentId = "", name="", mime="", size = 0},{ust & newMask = appendToMask newMask Blanked})
			Just docId // Look up meta-data in the store and update the document
				# (mbDocument,ust)	= getDocument docId ust
				# ust				= {ust & newMask = appendToMask newMask (Touched [])}
				= (fromMaybe s mbDocument,ust)
	| otherwise 
		= (s, {ust & newMask = appendToMask newMask cm})

derive gUpdate Either, (,), (,,), (,,,), Void, DateTime, UserDetails, Timestamp, Map, EmailAddress, Action, TreeNode, WorkflowDescription, ManagerProperties, RunningTaskStatus, TaskPriority, Session

basicUpdateSimple :: !(UpdateMode a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} a
basicUpdateSimple mode def ust = case mode of
	UDCreate	= basicCreate def ust
	UDSearch v	= basicSearch v (\json old -> fromMaybe old (fromJSON json)) ust
	
basicUpdate :: !(UpdateMode a) (upd a -> a) a !*USt -> *(!a,!*USt) | JSONDecode{|*|} upd
basicUpdate mode toV def ust = case mode of
	UDCreate	= basicCreate def ust
	UDSearch v	= basicSearch v toV ust
	
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

gDefaultMask {|Display|}			fx (Display d)				= fx d
gDefaultMask {|Editable|}			fx (Editable e)				= fx e
gDefaultMask {|Hidden|}				fx (Hidden h)				= fx h
gDefaultMask {|VisualizationHint|}	fx (VHEditable e)			= fx e
gDefaultMask {|VisualizationHint|}	fx (VHDisplay d)			= fx d
gDefaultMask {|VisualizationHint|}	fx (VHHidden h)				= fx h
gDefaultMask {|ControlSize|}		fx (ControlSize _ _ _ v)	= fx v
gDefaultMask {|FillControlSize|}	fx (FillControlSize v)		= fx v
gDefaultMask {|FillWControlSize|}	fx (FillWControlSize v)		= fx v
gDefaultMask {|FillHControlSize|}	fx (FillHControlSize v)		= fx v

gDefaultMask{|Int|}					_ = [Touched []]
gDefaultMask{|Real|}				_ = [Touched []]
gDefaultMask{|Char|}				_ = [Touched []]
gDefaultMask{|Bool|}				_ = [Touched []]
gDefaultMask{|String|}				_ = [Touched []]
gDefaultMask{|Dynamic|}				_ = [Touched []]
gDefaultMask{|(->)|} _ _			_ = [Touched []]
gDefaultMask{|WorkflowTaskContainer|}_ = [Touched []]
gDefaultMask{|Document|}			_ = [Touched []]			
gDefaultMask{|FormButton|}			_ = [Touched []]
gDefaultMask{|Note|}				_ = [Touched []]
gDefaultMask{|Password|}			_ = [Touched []]
gDefaultMask{|Currency|}			_ = [Touched []]
gDefaultMask{|Date|}				_ = [Touched []]
gDefaultMask{|Time|}				_ = [Touched []]
gDefaultMask{|User|}				_ = [Touched []]
gDefaultMask{|HtmlDisplay|}			_ = [Touched []]
gDefaultMask{|MultipleChoice|}_		_ = [Touched []]
gDefaultMask{|Choice|} _ (Choice opts sel)
	// if no valid selection is made, start with untouched mask
	| sel >= 0 && sel < length opts	= [Touched []]
	| otherwise						= [Untouched]
gDefaultMask{|Table|} _ = [Touched []]
gDefaultMask{|Tree|} _ tree=:(Tree _ sel)
	// if no valid selection is made, start with untouched mask
	| sel >= 0	= [Touched []]
	| otherwise	= [Untouched]

derive gDefaultMask Either, (,), (,,), (,,,), Void, DateTime, UserDetails, Timestamp, Map, EmailAddress, Action, TreeNode, WorkflowDescription, ManagerProperties, RunningTaskStatus, TaskPriority, Session

//Utility functions
dp2s :: !DataPath -> String
dp2s (DataPath path) = join "-" (map toString (reverse path))

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
toggleMask :: !JSONNode -> UpdateMask
toggleMask update = case update of
	JSONNull	= Blanked
	_			= Touched []
	
unchanged :: !Void !a -> a
unchanged _ v = v

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
