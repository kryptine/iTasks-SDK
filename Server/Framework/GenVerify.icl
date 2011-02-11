implementation module GenVerify

import StdGeneric, StdBool, StdInt, StdList, StdTuple, StdFunc, Maybe, Text
import GenUpdate, StdMisc, GenVisualize

derive gVerify (,), (,,), (,,,), Void, Either, UserDetails, DateTime, Timestamp, Map, EmailAddress, Action, ProcessRef, TreeNode, Table
derive bimap (,), Maybe

generic gVerify a :: (Maybe a) VerSt -> VerSt

verifyValue :: !a !UpdateMask !*IWorld -> (!VerifyMask, !*IWorld) | gVerify{|*|} a
verifyValue val updateMask iworld
	# verSt = gVerify{|*|} (Just val) {VerSt | updateMask = [updateMask], verifyMask = [], optional = False, iworld = iworld}
	= (hd verSt.VerSt.verifyMask, verSt.VerSt.iworld)

isValidValue :: !VerifyMask -> Bool
isValidValue (VMUntouched _ optional cm)	= optional && (and (map isValidValue cm)) 
isValidValue (VMValid _ cm)					= and (map isValidValue cm)
isValidValue (VMInvalid _ _)				= False

//Generic Verify
gVerify{|UNIT|} 			  _ 					vst = vst

gVerify{|PAIR|} 		fx fy  Nothing				vst = fy Nothing (fx Nothing vst)
gVerify{|PAIR|} 		fx fy (Just (PAIR x y))		vst = fy (Just y) (fx (Just x) vst)

gVerify{|EITHER|} 		_  _  Nothing				vst = vst
gVerify{|EITHER|}		fx _  (Just (LEFT x))		vst	= fx (Just x) vst
gVerify{|EITHER|}		_  fy (Just (RIGHT y))		vst = fy (Just y) vst 

gVerify{|OBJECT of d|}  fx     Nothing				vst = fx Nothing vst
gVerify{|OBJECT of d|}  fx	  (Just (OBJECT x))		vst	= fx (Just x) vst

gVerify{|CONS of d|}	fx    cons					vst=:{VerSt|updateMask,verifyMask,optional}
	# (cmu,um)	= popMask updateMask
	//Records
	| not (isEmpty d.gcd_fields)
		| optional
			//Only compute child verify mask, if record is already touched. Else you can end up in endless recursion!
			= case cmu of
				Touched _
					# vst=:{VerSt | verifyMask = childMask} = fx val {VerSt | vst & optional = False, updateMask = childMasks cmu, verifyMask = []}
					= {VerSt| vst & verifyMask = appendToMask verifyMask (VMValid Nothing childMask), optional = optional, updateMask = um}
				_
					= {VerSt| vst & verifyMask = appendToMask verifyMask (VMValid Nothing []), optional = optional, updateMask = um}
		| otherwise
			//Compute child mask
			# vst=:{VerSt | verifyMask = childMask} = fx val {VerSt | vst & optional = False, updateMask = childMasks cmu, verifyMask = []}
			= {VerSt| vst & verifyMask = appendToMask verifyMask (VMValid Nothing childMask), optional = optional, updateMask = um}	
	// ADT's with just one constructor
	| d.gcd_type_def.gtd_num_conses == 1
		# vst=:{VerSt | verifyMask = childMask} = fx val {VerSt | vst & updateMask = childMasks cmu, verifyMask = []}
		= case cmu of
			Untouched
				= {VerSt| vst & verifyMask = appendToMask verifyMask (VMUntouched Nothing optional childMask), optional = optional, updateMask = um}
			_
				= {VerSt| vst & verifyMask = appendToMask verifyMask (VMValid Nothing childMask), optional = optional, updateMask = um}	
	// ADT's with multiple constructors
	| otherwise
		//ADT's
		# vst=:{VerSt | verifyMask = childMask} = fx val {VerSt | vst & optional = False, updateMask = childMasks cmu, verifyMask = []}
		# consMask = case optional of
			True
				= VMValid (Just "Select an option") childMask
			False
				= case cmu of
					Untouched	= VMUntouched (Just "Select an option") False childMask
					Blanked		= VMInvalid IsBlankError childMask
					Touched _	= VMValid (Just "Select an option") childMask
		= {VerSt | vst & updateMask = um, optional = optional, verifyMask = appendToMask verifyMask consMask}	
where
	val = case cons of
		Nothing			= Nothing
		Just (CONS x)	= Just x
	
gVerify{|FIELD of d|}   fx	  Nothing				vst = fx Nothing vst
gVerify{|FIELD of d|}   fx    (Just (FIELD x))		vst = fx (Just x) vst

gVerify{|Maybe|} fx (Just (Just x)) vst=:{VerSt | optional}
	# vst = fx (Just x) {VerSt | vst & optional = True}
	= {VerSt | vst & optional = optional}
gVerify{|Maybe|} fx (Just Nothing) vst=:{VerSt | optional} 
	# vst = fx Nothing {VerSt | vst & optional = True}
	= {VerSt | vst & optional = optional}
gVerify{|Maybe|} fx Nothing vst=:{VerSt | updateMask,verifyMask,optional}
	# vst = fx Nothing {VerSt | vst & optional = True}
	= {VerSt | vst & optional = optional}

gVerify{|[]|} fx Nothing   vst=:{VerSt | optional}
	# msg = if optional "You may add list items" "Create at least one list item"
	= simpleVerify msg vst
gVerify{|[]|} fx (Just []) vst=:{VerSt | updateMask,verifyMask,optional}
	# (cm,um)	= popMask updateMask
	# vst=:{VerSt | verifyMask=childMask} = verifyItems fx [] {VerSt | vst & verifyMask = [], updateMask = childMasks cm, optional = False}
	| optional
		= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just "You may add list elements") childMask)}	
	# listMask  = case cm of
					Untouched	= VMUntouched Nothing optional childMask
					_			= VMInvalid IsBlankError childMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask listMask}
gVerify{|[]|} fx (Just x)  vst=:{VerSt | updateMask,verifyMask,optional}
	# (cm,um)	= popMask updateMask
	# vst=:{VerSt | verifyMask=childMask} = verifyItems fx x {VerSt | vst & verifyMask = [], updateMask = childMasks cm, optional = False}
	= case cm of
		(Untouched)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMUntouched Nothing optional childMask), optional = optional}
		_
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing childMask), optional = optional}

verifyItems fx [] vst=:{VerSt | optional}
	# vst = fx Nothing {VerSt | vst & optional = True}
	= {VerSt | vst & optional = optional}
verifyItems fx [x:xs] vst
	# vst = fx (Just x) vst
	= verifyItems fx xs vst

gVerify{|Hidden|} fx Nothing vst = vst
gVerify{|Hidden|} fx (Just (Hidden x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing [])}

gVerify{|Editable|} fx Nothing vst = fx Nothing vst
gVerify{|Editable|} fx (Just (Editable x)) vst = fx (Just x) vst

gVerify{|Display|} fx Nothing vst = vst
gVerify{|Display|} fx (Just (Display x)) vst = fx (Just x) vst

gVerify{|VisualizationHint|} fx Nothing vst = fx Nothing vst
gVerify{|VisualizationHint|} fx (Just (VHHidden x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing [])}
gVerify{|VisualizationHint|} fx (Just (VHDisplay x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing [])}
gVerify{|VisualizationHint|} fx (Just (VHEditable x)) vst = fx (Just x) vst

gVerify{|Int|}    			_ vst = simpleVerify "Enter a number" vst
gVerify{|Real|}   			_ vst = simpleVerify "Enter a decimal number" vst
gVerify{|Char|}   			_ vst = simpleVerify "Enter a character" vst
gVerify{|String|}			_ vst = simpleVerify "Enter a short text" vst
gVerify{|Bool|}   			_ vst = alwaysValid vst
gVerify{|Document|}			_ vst = simpleVerify "Upload a document" vst
gVerify{|Password|}			_ vst = simpleVerify "Enter a password" vst
gVerify{|Date|}				_ vst = simpleVerify "Enter a date" vst
gVerify{|Time|}				_ vst = simpleVerify "Enter a time of day" vst
gVerify{|Note|}				_ vst = simpleVerify "Enter a long text" vst
gVerify{|FormButton|}		_ vst = alwaysValid vst
gVerify{|Currency|}			_ vst = simpleVerify "Enter a currency value" vst
gVerify{|User|}				_ vst = simpleVerify "Select a username" vst 
gVerify{|Choice|}_			_ vst = simpleVerify "Choose one item" vst
gVerify{|MultipleChoice|} _	_ vst = simpleVerify "Choose a number of items" vst
gVerify{|Tree|} _			_ vst = simpleVerify "Choose a leaf of the tree" vst

gVerify{|Dynamic|}			_ vst = alwaysValid vst
gVerify{|(->)|} _ _			_ vst = alwaysValid vst
gVerify{|Shared|} _			_ vst = alwaysValid vst
gVerify{|SharedReadOnly|} _	_ vst = alwaysValid vst

//********************************************************************************************************
anyError :: ![VerifyMask] -> Bool
anyError children = or [isError c \\ c <- children]
where
	isError (VMInvalid _ _) = True
	isError _				= False

anyUntouched :: ![VerifyMask] -> Bool
anyUntouched children = or [isUntouched c \\ c <- children]
where
	isUntouched (VMUntouched _ _ _)	= True
	isUntouched _					= False

allUntouched :: ![VerifyMask] -> Bool
allUntouched children = and [isUntouched c \\ c <- children]
where
	isUntouched (VMUntouched _ _ _)	= True
	isUntouched _					= False

allValid :: ![VerifyMask] -> Bool
allValid children = and [isValid c \\ c <- children]
where
	isValid (VMValid _ _)	= True
	isValid _				= False

instance toString ErrorMessage
where
	toString (ErrorMessage s) = s
	toString IsBlankError = "This value is required"
	
instance GenMask VerifyMask
where
	popMask :: ![VerifyMask] -> (!VerifyMask, ![VerifyMask])
	popMask []					   		= (VMValid Nothing [], [])
	popMask [c:cm]						= (c,cm)
	
	appendToMask :: ![VerifyMask] !VerifyMask -> [VerifyMask]
	appendToMask l c = l++[c]
	
	childMasks :: !VerifyMask -> [VerifyMask]
	childMasks (VMValid _ cm)		= cm
	childMasks (VMInvalid _ cm)		= cm
	childMasks (VMUntouched _ _ cm)	= cm
	
alwaysValid :: !*VerSt -> *VerSt
alwaysValid vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing [])}
	
simpleVerify :: !String !*VerSt -> *VerSt
simpleVerify hint vst
	= customWorldVerify (Just hint) (curry (app2 (const (WPRValid (Just hint)),id))) (Just (abort "no value needed for simple verify")) vst

wrapperVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) !*VerSt -> *VerSt
wrapperVerify mbHint pred parseErr mbVal vst=:{VerSt | updateMask, verifyMask, optional} 
	# (cm,um) = popMask updateMask
	# vmask = case mbVal of
		Just val
			| optional = case cm of
				Touched _	= validateValue val
				_			= VMValid Nothing [VMValid mbHint []]           
			| otherwise = case cm of
				Untouched	= VMUntouched Nothing False [VMUntouched mbHint False []]
				Blanked		= VMInvalid IsBlankError [VMInvalid IsBlankError []]
				Touched _	= validateValue val     
		Nothing
			| optional		= VMValid Nothing [VMValid mbHint []]
			| otherwise		= VMUntouched Nothing False [VMUntouched mbHint False []]
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask vmask}
where
    validateValue val
	    | pred val  = VMValid Nothing [VMValid mbHint []]
	    | otherwise = VMInvalid (ErrorMessage "") [VMInvalid (ErrorMessage (parseErr val)) []]

customVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) !*VerSt -> *VerSt
customVerify mbHint pred mkErrMsg mbVal vst = customWorldVerify mbHint pred` mbVal vst
where
	pred` val iworld
		| pred val	= (WPRValid mbHint,iworld)
		| otherwise	= (WPRInvalid (mkErrMsg val),iworld)

customWorldVerify :: !(Maybe String) !(a *IWorld -> (!WorldPredicateResult,!*IWorld)) !(Maybe a) !*VerSt -> *VerSt
customWorldVerify mbHint pred mbVal vst=:{VerSt | updateMask, verifyMask, optional} 
	# (cm,um) = popMask updateMask
	# (vmask,vst) = case mbVal of
		Just val
			| optional
				= case cm of
					Touched _	= validateValue val vst
					_			= (VMValid mbHint [],vst)
			| otherwise
				= case cm of
					Untouched	= (VMUntouched mbHint False [],vst)
					Blanked		= (VMInvalid IsBlankError [],vst)
					Touched _	= validateValue val vst
		Nothing
			| optional			= (VMValid mbHint [],vst)
			| otherwise			= (VMUntouched mbHint False [],vst)
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask vmask}
where
	validateValue val vst=:{VerSt|iworld}
		# (res,iworld) = pred val iworld
		# mask = case res of
			WPRValid mbHint	= VMValid mbHint []
			WPRInvalid err	= VMInvalid (ErrorMessage err) []
		= (mask,{VerSt|vst & iworld = iworld})

setInvalid :: ![(!DataPath,!ErrorMessage)] !VerifyMask -> VerifyMask
setInvalid errors mask = seq (map setInvalid` errors) mask
where
	setInvalid` (p,msg) mask = hd (setInvalid`` (reverse (dataPathList p)) [mask])
	where
		setInvalid`` [0]	[mask:masks] = [VMInvalid msg (childMasks mask):masks]
		setInvalid`` [0:p]	[mask:masks] = [setChildren mask (setInvalid`` p (childMasks mask)):masks]
		setInvalid`` [n:p]	[mask:masks] = [mask:setInvalid`` [dec n:p] masks]
		
	setChildren (VMUntouched	mbHint optional _)	children = VMUntouched	mbHint optional children
	setChildren (VMValid	 	mbHint _)			children = VMValid	 	mbHint children
	setChildren (VMInvalid		errorM _)			children = VMInvalid	errorM children