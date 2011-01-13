implementation module GenVerify

import StdMaybe, StdGeneric, StdBool, StdInt, Text, StdList, StdTuple, StdFunc
import GenUpdate, StdMisc

derive bimap (,), Maybe

derive gVerify (,), (,,), (,,,), Void, Either, UserDetails, DateTime
derive JSONEncode VerifyMask, ErrorMessage

generic gVerify a :: (Maybe a) VerSt -> VerSt

verifyValue :: !a !UpdateMask *IWorld -> (VerifyMask, *IWorld) | gVerify{|*|} a
verifyValue val updateMask iworld
	# verSt = gVerify{|*|} (Just val) {VerSt | updateMask = [updateMask], verifyMask = [], optional = False, iworld = iworld}
	= (hd verSt.VerSt.verifyMask, verSt.VerSt.iworld)

isValidValue :: !VerifyMask -> Bool
isValidValue (VMUntouched _ _ optional cm)	= optional && (and (map isValidValue cm)) 
isValidValue (VMValid _ _ cm)				= and (map isValidValue cm)
isValidValue (VMInvalid _ _ _)				= False

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
				(Touched _ _)
					# vst=:{VerSt | verifyMask = childMask} = fx val {VerSt | vst & optional = False, updateMask = childMasks cmu, verifyMask = []}
					= {VerSt| vst & verifyMask = appendToMask verifyMask (VMValid Nothing Nothing childMask), optional = optional, updateMask = um}
				_
					= {VerSt| vst & verifyMask = appendToMask verifyMask (VMValid Nothing Nothing []), optional = optional, updateMask = um}
		| otherwise
			//Compute child mask
			# vst=:{VerSt | verifyMask = childMask} = fx val {VerSt | vst & optional = False, updateMask = childMasks cmu, verifyMask = []}
			= {VerSt| vst & verifyMask = appendToMask verifyMask (VMValid Nothing Nothing childMask), optional = optional, updateMask = um}	
	// ADT's with just one constructor
	| d.gcd_type_def.gtd_num_conses == 1
		# vst=:{VerSt | verifyMask = childMask} = fx val {VerSt | vst & updateMask = childMasks cmu, verifyMask = []}
		= case cmu of
			(Untouched)
				= {VerSt| vst & verifyMask = appendToMask verifyMask (VMUntouched Nothing Nothing optional childMask), optional = optional, updateMask = um}
			_
				= {VerSt| vst & verifyMask = appendToMask verifyMask (VMValid Nothing Nothing childMask), optional = optional, updateMask = um}	
	// ADT's with multiple constructors
	| otherwise
		//ADT's
		# vst=:{VerSt | verifyMask = childMask} = fx val {VerSt | vst & optional = False, updateMask = childMasks cmu, verifyMask = []}
		# consMask = case optional of
			True
				= VMValid (Just "Select an option") Nothing childMask
			False
				= case cmu of
					(Untouched)		= VMUntouched (Just "Select an option") Nothing False childMask
					(Blanked _)		= VMInvalid IsBlankError Nothing childMask
					(Touched _ _)	= VMValid (Just "Select an option") Nothing childMask
		= {VerSt | vst & updateMask = um, optional = optional, verifyMask = appendToMask verifyMask consMask}	
where
	val = case cons of
		Nothing			= Nothing
		Just (CONS x)	= Just x
	
gVerify{|FIELD of d|}   fx	  Nothing				vst = fx Nothing vst
gVerify{|FIELD of d|}   fx    (Just (FIELD x))		vst = fx (Just x) vst
	
gVerify{|Int|}    _ vst = basicVerify "Enter a number" vst
gVerify{|Real|}   _ vst = basicVerify "Enter a decimal number" vst
gVerify{|Char|}   _ vst = basicVerify "Enter a character" vst
gVerify{|String|} _ vst = basicVerify "Enter a short text" vst
gVerify{|Bool|}   _ vst=:{VerSt | verifyMask,updateMask} 
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}

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
	= basicVerify msg vst
gVerify{|[]|} fx (Just []) vst=:{VerSt | updateMask,verifyMask,optional}
	# (cm,um)	= popMask updateMask
	# vst=:{VerSt | verifyMask=childMask} = verifyItems fx [] {VerSt | vst & verifyMask = [], updateMask = childMasks cm, optional = False}
	| optional
		= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just "You may add list elements") Nothing childMask)}	
	# listMask  = case cm of 
					(Untouched) 
						= (VMUntouched Nothing Nothing optional childMask)
					(TouchedList _ _)
						= (VMValid Nothing Nothing childMask)
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask listMask}
gVerify{|[]|} fx (Just x)  vst=:{VerSt | updateMask,verifyMask,optional}
	# (cm,um)	= popMask updateMask
	# vst=:{VerSt | verifyMask=childMask} = verifyItems fx x {VerSt | vst & verifyMask = [], updateMask = childMasks cm, optional = False}
	= case cm of
		(Untouched)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMUntouched Nothing Nothing optional childMask), optional = optional}
		_
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing childMask), optional = optional}

verifyItems fx [] vst=:{VerSt | optional}
	# vst = fx Nothing {VerSt | vst & optional = True}
	= {VerSt | vst & optional = optional}
verifyItems fx [x:xs] vst
	# vst = fx (Just x) vst
	= verifyItems fx xs vst


gVerify{|Dynamic|} _ vst = vst

gVerify{|(->)|} _ _ _ vst = vst

gVerify{|Hidden|} fx Nothing vst = vst
gVerify{|Hidden|} fx (Just (Hidden x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}

gVerify{|Editable|} fx Nothing vst = fx Nothing vst
gVerify{|Editable|} fx (Just (Editable x)) vst = fx (Just x) vst

gVerify{|Display|} fx Nothing vst = vst
gVerify{|Display|} fx (Just (Display x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}

gVerify{|VisualizationHint|} fx Nothing vst = fx Nothing vst
gVerify{|VisualizationHint|} fx (Just (VHHidden x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}
gVerify{|VisualizationHint|} fx (Just (VHDisplay x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}
gVerify{|VisualizationHint|} fx (Just (VHEditable x)) vst = fx (Just x) vst

gVerify{|Document|} _ vst = basicVerify "Upload a document" vst

gVerify{|Password|} _ vst = basicVerify "Enter a password" vst

gVerify{|Date|} _ vst = basicVerify "Enter a date" vst

gVerify{|Time|} _ vst = basicVerify "Enter a time of day" vst

gVerify{|Note|} _ vst = basicVerify "Enter a long text" vst

gVerify{|FormButton|} _ vst = vst

gVerify{|Currency|} _ vst = basicVerify "Enter a currency value" vst

gVerify{|User|} _ vst=:{VerSt | updateMask, verifyMask, optional} = basicVerify "Select a username" vst 

gVerify{|Task|} _ _ vst = vst

gVerify{|Choice|}			_ mbVal vst = verifyConstructor (Just "Choose one item")			(const True) undef mbVal vst
gVerify{|MultipleChoice|}	_ mbVal vst = verifyConstructor (Just "Choose a number of items")	(const True) undef mbVal vst

//********************************************************************************************************
basicVerify :: String !*VerSt -> *VerSt
basicVerify msg vst=:{VerSt | updateMask,verifyMask,optional}
	# (cm,um)   = popMask updateMask
	| optional  = case cm of
		(Untouched)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMUntouched (Just msg) Nothing True [])}
		_
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just msg) Nothing [])}	
	| otherwise = case cm of
		(Untouched)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMUntouched (Just msg) Nothing False [])}
		(Touched _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just msg) Nothing [])}
		(Blanked _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMInvalid IsBlankError Nothing [])}
		(TouchedList _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just msg) Nothing [])}

anyError :: ![VerifyMask] -> Bool
anyError children = or [isError c \\ c <- children]
where
	isError (VMInvalid _ _ _) = True
	isError _				  = False

anyUntouched :: ![VerifyMask] -> Bool
anyUntouched children = or [isUntouched c \\ c <- children]
where
	isUntouched (VMUntouched _ _ _ _)	= True
	isUntouched _						= False

allUntouched :: ![VerifyMask] -> Bool
allUntouched children = and [isUntouched c \\ c <- children]
where
	isUntouched (VMUntouched _ _ _ _)	= True
	isUntouched _						= False

allValid :: ![VerifyMask] -> Bool
allValid children = and [isValid c \\ c <- children]
where
	isValid (VMValid _ _ _) = True
	isValid _				= False

instance toString ErrorMessage
where
	toString (ErrorMessage s) = s
	toString IsBlankError = "This value is required."
	
instance GenMask VerifyMask
where
	popMask :: ![VerifyMask] -> (!VerifyMask, ![VerifyMask])
	popMask []					   		= (VMValid Nothing Nothing [], [])
	popMask [c:cm]						= (c,cm)
	
	appendToMask :: ![VerifyMask] !VerifyMask -> [VerifyMask]
	appendToMask l c = l++[c]
	
	childMasks :: !VerifyMask -> [VerifyMask]
	childMasks (VMValid _ _ cm)			= cm
	childMasks (VMInvalid _ _ cm)		= cm
	childMasks (VMUntouched _ _ _ cm)	= cm

//********************************************************************************************************

verifyConstructor :: (Maybe String) (a -> Bool) (a -> String) (Maybe a) !*VerSt -> *VerSt
verifyConstructor mbHint pred parseErr mbVal vst=:{VerSt | updateMask, verifyMask, optional} 
	# (cm,um) = popMask updateMask
	# vmask = case mbVal of
		(Just val)
			| optional
				= case cm of
					(Touched _ _) 
						= validateValue val
					_
						= VMValid Nothing Nothing [VMValid mbHint Nothing []]			
			| otherwise
				= case cm of
					(Untouched)
						= VMUntouched Nothing Nothing False [VMUntouched mbHint Nothing False []]
					(Blanked _)
						= VMInvalid IsBlankError Nothing [VMInvalid IsBlankError Nothing []]
					(Touched _ _)
						= validateValue val		
		Nothing
			| optional 	= VMValid Nothing Nothing [VMValid mbHint Nothing []]
			| otherwise = VMUntouched Nothing Nothing False [VMUntouched mbHint Nothing False []]
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask vmask}
where
	validateValue val
	| pred val 	= VMValid Nothing Nothing [VMValid mbHint Nothing []]
	| otherwise	= VMInvalid (ErrorMessage "") Nothing [VMInvalid (ErrorMessage (parseErr val)) Nothing []]

worldVerify :: (Maybe String) (a *IWorld -> (Maybe String, *IWorld)) (Maybe a) !*VerSt -> *VerSt
worldVerify mbHint fErr mbVal vst=:{VerSt | updateMask, verifyMask, optional, iworld} 
	# (cm,um) = popMask updateMask
	# (vmask,iworld) = case mbVal of
		(Just val)
			| optional
				= case cm of
					(Touched _ _) 
						= validateValue val iworld
					_
						= (VMValid Nothing Nothing [VMValid mbHint Nothing []], iworld)
			| otherwise
				= case cm of
					(Untouched)
						= (VMUntouched Nothing Nothing False [VMUntouched mbHint Nothing False []], iworld)
					(Blanked _)
						= (VMInvalid IsBlankError Nothing [VMInvalid IsBlankError Nothing []], iworld)
					(Touched _ _)
						= validateValue val iworld
		Nothing
			| optional 	= (VMValid Nothing Nothing [VMValid mbHint Nothing []], iworld)
			| otherwise = (VMUntouched Nothing Nothing False [VMUntouched mbHint Nothing False []], iworld)
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask vmask, iworld = iworld}
where
	validateValue val iworld
		# (mErr, iworld) = fErr val iworld
		# vm = case mErr of
		         Nothing  = VMValid mbHint Nothing []
		         Just err = VMInvalid (ErrorMessage err) Nothing []
		= (vm, iworld)