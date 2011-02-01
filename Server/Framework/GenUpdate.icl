implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, StdEnum
import Types, Text, Util, Shared, DocumentDB
from StdFunc import id

:: DataPath = DataPath [Int]

defaultValue :: !*IWorld -> (!a,!*IWorld) | gUpdate{|*|} a
defaultValue iworld  
	# (a,ust=:{USt|iworld}) = gUpdate{|*|} (abort "gUpdate accessed value during create") {USt|mode = UDCreate, searchPath = emptyDataPath, currentPath = emptyDataPath, consPath = [], update = "", oldMask = [], newMask = [], iworld = iworld}
	= (a,iworld)

defaultMask :: a !*IWorld -> (!UpdateMask,!*IWorld) | gUpdate{|*|} a	
defaultMask a iworld
	# (_,ust=:{newMask,iworld}) = gUpdate{|*|} a {USt| mode = UDMask, searchPath = emptyDataPath, currentPath = startDataPath, consPath = [], update = "", oldMask = [], newMask = [], iworld = iworld}
	= (hd newMask,iworld)

updateValue	:: DataPath String a !*IWorld -> (a,!*IWorld) | gUpdate{|*|} a 	
updateValue path update a iworld
	# (a,_,iworld) = updateValueAndMask path update a Untouched iworld
	= (a,iworld)

updateValueAndMask :: DataPath String a UpdateMask !*IWorld -> (a,UpdateMask,!*IWorld) | gUpdate{|*|} a
updateValueAndMask path update a oldMask iworld	
	# (a,ust=:{newMask,iworld}) = gUpdate{|*|} a {USt| mode = UDSearch, searchPath = path, currentPath = startDataPath, consPath = [], update = update, oldMask = [oldMask], newMask = [], iworld = iworld}
	= (a,hd newMask,iworld)

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
	
gUpdate{|PAIR|} fx fy (PAIR x y) ust
	# (nx,ust) = fx x ust
	# (ny,ust) = fy y ust
	= (PAIR nx ny, ust)

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

gUpdate{|EITHER|} fx fy e ust	
	= case e of
		(LEFT x)	
			# (nx,ust) = fx x ust
			= (LEFT nx, ust)
		(RIGHT y)
			# (ny,ust) = fy y ust
			= (RIGHT ny,ust)

gUpdate{|CONS|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx (abort "CONS create with undef") ust
	= (CONS nx, ust)
		
gUpdate{|CONS|} fx (CONS x) ust
	# (nx,ust) = (fx x ust)
	= (CONS nx, ust)

gUpdate{|OBJECT|} fx _ ust=:{mode=UDCreate,newMask}
	//Empty object mask
	# (nx,ust=:{newMask=objectMask}) = fx (abort "OBJECT create with undef") {USt | ust & newMask = []}
	= (OBJECT nx, {USt | ust & newMask = newMask ++ objectMask})

gUpdate{|OBJECT of d|} fx (OBJECT x) ust=:{mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		//Update is a constructor switch
		# (nx,ust) = fx (abort "OBJECT create with undef") {USt|ust & mode = UDCreate, consPath = path}
		= (OBJECT nx, {USt|ust & oldMask = om, currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), mode = UDSearch}) 
	| searchPath <== currentPath
		//Update is targeted somewhere in a substructure of this value
		# (nx,ust=:{newMask=childMask}) = fx x {USt|ust & currentPath = shiftDataPath currentPath, oldMask = childMasks cm, newMask = []}
		= (OBJECT nx, {USt|ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask (Touched childMask)})
	| otherwise
		//Not on the path, so just put back the current mask (cm)	
		= (OBJECT x, {USt|ust & currentPath = stepDataPath currentPath, oldMask = om, newMask = appendToMask newMask cm}) 
where
	path = case [cons \\ cons <- d.gtd_conses | cons.gcd_name == update] of
		[cons]	= getConsPath cons
		_		= []

gUpdate{|OBJECT of d|} fx o=:(OBJECT x) ust=:{mode=UDMask,currentPath,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# (_,ust=:{newMask = childMask}) = fx x {USt|ust & currentPath = shiftDataPath currentPath, oldMask = childMasks cm, newMask = []}
	= (o,{USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched childMask)})

gUpdate{|FIELD|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx (abort "FIELD create with undef") ust
	= (FIELD nx, ust)

gUpdate{|FIELD|} fx (FIELD x) ust
	# (nx,ust) = fx x ust
	= (FIELD nx, ust)
	
//**********************************************************************************************************
gUpdate{|Int|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (0,{USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Int|} i ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask	
	| currentPath == searchPath
		= (toInt update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (i, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|Int|} i ust=:{USt|mode=UDMask,currentPath,newMask}
	= (i, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 
//***
gUpdate{|Real|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (0.0, {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Real|} r ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= (toReal update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (r, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|Real|} r ust=:{USt|mode=UDMask,currentPath,newMask}
	= (r, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 
//***
gUpdate{|Char|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (' ', {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Char|} c ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= (if (size update > 0) update.[0] c, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (c, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|Char|} c ust=:{USt|mode=UDMask,currentPath,newMask}
	= (c, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 
//***
gUpdate{|Bool|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (False, {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Bool|} b ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= (update == "true", {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (b, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|Bool|} b ust=:{USt|mode=UDMask,currentPath,newMask}
	= (b, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 
//***
gUpdate{|String|} _ ust=:{USt|mode=UDCreate,newMask} 
	= ("", {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|String|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= (update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|String|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 

//Specialized instance Maybe that chooses the non-recursive constructor 
gUpdate{|Maybe|} fx _ ust=:{USt|mode=UDCreate,newMask} 
	= (Nothing,{USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Maybe|} fx m ust=:{USt|mode=UDSearch,currentPath,searchPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath && update == ""	
		= (Nothing, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask Blanked, oldMask = om}) //Reset
	| otherwise
		= case m of
			Nothing
				| searchPath <== currentPath
					// Create a default value
					# (x,ust=:{newMask=nmCreate}) 	= (fx (abort "Maybe create with undef") {ust & mode = UDCreate})
					// Search in the default value
					# (x,ust=:{newMask=nmSearch}) 	= fx x {ust & mode = UDSearch, currentPath = currentPath, searchPath = searchPath, update = update, oldMask = oldMask, newMask = newMask}
					= (Just x, ust)
				| otherwise
					= (Nothing, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
			Just x
				# (x,ust) = fx x ust //all mask transformations are made here..
				= (Just x,ust)
				
gUpdate{|Maybe|} fx m ust=:{USt|mode=UDMask,currentPath,newMask}
	= case m of
		Nothing	= (m, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask Untouched})
		Just x
			# (_,ust) = fx x ust //all mask transformations are made here..
			= (m,ust)

//Specialize instance for Dynamic
gUpdate{|Dynamic|} _ ust=:{USt|mode=UDCreate}	= (dynamic 42, ust)
gUpdate{|Dynamic|} d ust						= (d, ust)

gUpdate{|[]|} fx _ ust=:{USt|mode=UDCreate,newMask} 
	= ([], {USt | ust & newMask = appendToMask newMask (Touched [])})
gUpdate{|[]|} fx l ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	# (l,listMask,ust)
		= case isNew currentPath searchPath (length l) of
			True
				# (nv,ust) = fx (abort "LIST - new element accessed during create") {USt | ust & mode=UDCreate, oldMask = [], newMask = []}
				= (l++[nv], Touched [], ust)
			False
				= (l,Touched [], ust)
	# (lx,ust=:{newMask=listMask})
		= updateElements fx l {USt|ust & mode = UDSearch, currentPath = shiftDataPath currentPath, oldMask = childMasks cm, newMask = []}
	| currentPath == searchPath
		//Process the reordering commands 
		# split = split "_" update
		# index = toInt (last split)
		= case hd split of	
			"mup"
				| index == 0
					= (lx, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask), oldMask = om})
				# lx		= swap lx index
				# listMask	= swap listMask index
				# dirty		= [index - 1, index]
				= (lx, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask), oldMask = om}) 
			"mdn"
				| index >= (length lx) - 1
					= (lx, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask), oldMask = om})
				# lx		= swap lx (index+1) //down idx == up (idx+1)
				# listMask	= swap listMask (index+1)
				# dirty		= [index, index + 1]
				= (lx, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask), oldMask = om})
			"rem"
				# lx		= removeAt index lx
				# listMask	= removeAt index listMask 
				# dirty		= [index .. length lx-1]	//Mark everything above the removed item dirty
				= (lx, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask), oldMask = om})	
			"add"
				# (nv,ust=:{newMask=childMask})
							= fx (abort "LIST create with undef") {USt | ust & mode=UDCreate, oldMask = [], newMask = []}
				# lx		= insertAt (index+1) nv lx
				# listMask	= insertAt (index+1) (hd childMask) listMask
				# dirty		= [index+1 .. length lx - 1]//Mark evertything above the inserted item dirty
				= (lx, {USt | ust & mode = UDSearch, currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask), oldMask = om})
			_ 	
				= (lx, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask), oldMask = om})
	| otherwise 
		= (lx, {USt | ust & currentPath  = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask), oldMask = om})
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
	
gUpdate{|[]|} fx l ust=:{USt|mode=UDMask,currentPath,newMask}
	# ust=:{newMask=listMask} = gMarkList fx l {USt | ust & currentPath = shiftDataPath currentPath, newMask = []}
	= (l,{USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched listMask)})
where
	gMarkList fx [] ust = ust
	gMarkList fx [x:xs] ust
		# (_,ust) = fx x ust
		= gMarkList fx xs ust

gUpdate {|Display|} fx _ ust=:{USt | mode=UDCreate, currentPath}
	# (nx,ust) = fx (abort "Display created with undef") ust
	= (Display nx,{USt | ust & currentPath = stepDataPath currentPath})
gUpdate {|Display|} fx (Display s) ust=:{USt | currentPath} 
	# (s,ust) = fx s ust
	= (Display s,{USt | ust & currentPath = stepDataPath currentPath})

gUpdate {|Editable|} fx _ ust=:{USt | mode=UDCreate, currentPath} 
	# (nx,ust) = fx (abort "Editable created with undef") ust
	= (Editable nx,{USt | ust & currentPath = stepDataPath currentPath})
gUpdate {|Editable|} fx (Editable s) ust=:{USt | currentPath} 
	# (s,ust) = fx s ust
	= (Editable s,{USt | ust & currentPath = stepDataPath currentPath})

gUpdate {|(->)|} fx fy _ ust=:{USt | mode=UDCreate, currentPath} 
	= (abort "default function",{USt | ust & currentPath = stepDataPath currentPath})
gUpdate {|(->)|} fx fy f ust=:{USt | currentPath} 
	= (f,{USt | ust & currentPath = stepDataPath currentPath})

gUpdate {|Hidden|} fx _ ust=:{USt | mode=UDCreate, currentPath} 
	# (nx,ust) = fx (abort "Hidden created with undef") ust
	= (Hidden nx,{USt | ust & currentPath = stepDataPath currentPath})
gUpdate {|Hidden|} fx (Hidden s) ust=:{USt | currentPath} 
	# (s,ust) = fx s ust
	= (Hidden s,{USt | ust & currentPath = stepDataPath currentPath})

gUpdate {|VisualizationHint|} fx _ ust=:{USt | mode=UDCreate, currentPath} 
	# (nx,ust) = fx (abort "VisualizationHint created with undef") ust
	= (VHEditable nx,{USt | ust & currentPath = stepDataPath currentPath})
gUpdate {|VisualizationHint|} fx (VHEditable s) ust=:{USt | currentPath} 
	# (s,ust) = fx s ust
	= (VHEditable s,{USt | ust & currentPath = stepDataPath currentPath})
gUpdate {|VisualizationHint|} fx (VHDisplay s) ust=:{USt | currentPath} 
	# (s,ust) = fx s ust
	= (VHDisplay s,{USt | ust & currentPath = stepDataPath currentPath})
gUpdate {|VisualizationHint|} fx (VHHidden s) ust=:{USt | currentPath} 
	# (s,ust) = fx s ust
	= (VHHidden s,{USt | ust & currentPath = stepDataPath currentPath})

gUpdate {|Document|} _ ust =: {USt | mode=UDCreate,newMask} 
	= ({Document|documentId = "", name="", mime="", size = 0},{USt | ust & newMask = appendToMask newMask Untouched})
gUpdate {|Document|} s ust =: {USt | mode=UDMask,currentPath,newMask}
	= (s,{USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])})
gUpdate {|Document|} s ust =: {USt | mode=UDSearch, searchPath, currentPath, update, oldMask, newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		| update == "" // Reset
			= ({Document|documentId = "", name="", mime="", size = 0},{USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask Blanked, oldMask = om})
		| otherwise // Look up meta-data in the store and update the document
			# (mbDocument,ust) = getDocument update ust
			= case mbDocument of
				Just document 	= (document,{USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched []), oldMask = om})
				Nothing			= (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched []), oldMask = om})
	| otherwise 
		= (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})

gUpdate{|FormButton|} _ ust=:{USt|mode=UDCreate,newMask}
	= ({FormButton | label = "Form Button", icon="", state = NotPressed}, {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|FormButton|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= ({s & state = if(update == "true") Pressed NotPressed}, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|FormButton|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 

gUpdate{|Note|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (Note "", {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Note|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (Note update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|Note|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 

gUpdate{|Password|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (Password "", {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Password|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (Password update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|Password|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 

gUpdate{|Currency|} _ ust=:{USt|mode=UDCreate, newMask} 
	= (EUR 0,{USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Currency|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,newMask, oldMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (parseUpdate s update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
where
	parseUpdate orig update =
		 case split "." update of
			[whole]		= replaceVal orig (100 * toInt whole)
			[whole,dec] = replaceVal orig (100 * toInt whole + (if (size dec == 1) (10 * toInt dec) (toInt (dec % (0,1)))))
			_			= orig
	
	replaceVal (EUR _) x = (EUR x)
	replaceVal (GBP _) x = (GBP x)
	replaceVal (USD _) x = (USD x)
	replaceVal (JPY _) x = (JPY x)

gUpdate{|Currency|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 

gUpdate{|Date|} _ ust=:{USt|mode=UDCreate,newMask,iworld=iworld=:{IWorld|world}}
	# (date,world) = currentDate world
	= (date, {USt|ust & iworld = {IWorld|iworld & world = world}, newMask = appendToMask newMask Untouched})
gUpdate{|Date|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (fromString update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|Date|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 

gUpdate{|Time|} _ ust=:{USt|mode=UDCreate,newMask,iworld=iworld=:{IWorld|world}}
	# (time,world) = currentTime world
	= (time, {USt|ust & iworld = {IWorld|iworld & world = world}, newMask = appendToMask newMask Untouched})
gUpdate{|Time|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (fromString update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|Time|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])}) 

gUpdate{|User|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (AnyUser,{USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|User|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		| userName (NamedUser update) == "root"
			= (RootUser, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
		| otherwise
			= (NamedUser update,  {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask cm, oldMask = om})
gUpdate{|User|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])})

gUpdate{|Choice|} _ _ ust=:{USt|mode=UDCreate,newMask} 
	= (Choice [] -1, {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Choice|} _ c=:(Choice opts _) ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		# (n,mask) = case fromJSON (fromString update) of
			Just [i]	= (Choice opts i,	Touched [])
			_			= (Choice opts -1,	Blanked)
		= (n, {ust & newMask = appendToMask newMask mask}) 
	| otherwise
		= (c, {ust & newMask = appendToMask newMask cm})
gUpdate{|Choice|} _ c=:(Choice opts sel) ust=:{USt|mode=UDMask,currentPath,newMask}
	// if no valid selection is made, start with untouched mask
	# mask = if (sel >= 0 && sel < length opts) (Touched []) (Untouched)
	= (c, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask mask})
	
gUpdate{|MultipleChoice|} _ _ ust=:{USt|mode=UDCreate,newMask} 
	= (MultipleChoice [] [], {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|MultipleChoice|} _ c=:(MultipleChoice opts _) ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		# n = case fromJSON (fromString update) of
			Just s	= MultipleChoice opts s
			Nothing	= MultipleChoice opts []
		= (n, {ust & newMask = appendToMask newMask (toggleMask update)}) 
	| otherwise
		= (c, {ust & newMask = appendToMask newMask cm})
gUpdate{|MultipleChoice|} _ c ust=:{USt|mode=UDMask,currentPath,newMask}
	= (c, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched [])})

gUpdate{|Tree|} _ _ ust=:{USt|mode=UDCreate,newMask} 
	= (Tree [] -1, {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Tree|} _ tree=:(Tree nodes _) ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		# selIdx = toInt update
		# (n,mask) = if (update <> "" && selIdx >= 0)
			(Tree nodes (toInt update),	Touched [])
			(Tree nodes -1,				Blanked)
		= (n, {ust & newMask = appendToMask newMask mask}) 
	| otherwise
		= (tree, {ust & newMask = appendToMask newMask cm})
gUpdate{|Tree|} _ tree=:(Tree _ sel) ust=:{USt|mode=UDMask,currentPath,newMask}
	// if no valid selection is made, start with untouched mask
	# mask = if (sel >= 0) (Touched []) Untouched
	= (tree, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask mask})

gUpdate{|Shared|}			_ _ ust=:{mode=UDCreate}	= (Shared "" Nothing, ust)
gUpdate{|Shared|}			_ x ust						= (x,ust)
gUpdate{|SharedReadOnly|}	_ _ ust=:{mode=UDCreate}	= (SharedReadOnly "" Nothing, ust)
gUpdate{|SharedReadOnly|}	_ x ust						= (x,ust)

derive gUpdate Either, (,), (,,), (,,,), Void, DateTime, UserDetails, Timestamp, Map, EmailAddress, Action, ProcessRef
derive bimap (,)

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