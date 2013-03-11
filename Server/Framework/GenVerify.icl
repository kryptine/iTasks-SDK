implementation module GenVerify

import StdGeneric, StdBool, StdInt, StdList, StdTuple, StdFunc, Maybe, Functor, Util, Text, Generic
import GenUpdate, StdMisc

verifyValue :: !a -> Bool | gVerify{|*|} a
verifyValue val = isValidMask (verifyMaskedValue val Touched)

verifyMaskedValue :: !a !InteractionMask -> VerifyMask | gVerify{|*|} a
verifyMaskedValue val mask
	= case fst (gVerify{|*|} (Just val) [mask] {VerifyOptions|optional = False, disabled = False}) of
		[mask]	= mask
		_		= VMInvalid (FormatError "Could not verify value") []

isValidMask :: !VerifyMask -> Bool
isValidMask (VMUntouched _ optional cm)	= optional && (and (map isValidMask cm)) 
isValidMask (VMValid _ cm)				= and (map isValidMask cm)
isValidMask (VMValidWithState _ cm _)	= and (map isValidMask cm)
isValidMask _							= False

//Generic Verify
generic gVerify a :: !(Maybe a) ![InteractionMask] !VerifyOptions -> ([VerifyMask],[InteractionMask])

gVerify{|UNIT|} _ um _ = ([],um)

gVerify{|PAIR|} fx fy pair um options
	# (vmx,um)		= fx (fmap fromPAIRX pair) um options
	# (vmy,um)		= fy (fmap fromPAIRY pair) um options
	= (vmx++vmy,um)

gVerify{|EITHER|} fx _ (Just (LEFT x))	um options	= fx (Just x) um options
gVerify{|EITHER|} _ fy (Just (RIGHT y)) um options	= fy (Just y) um options
gVerify{|EITHER|} fx fy Nothing um options			= ([],um)

gVerify{|RECORD|} fx record [] options = ([],[])
gVerify{|RECORD of {grd_arity}|} fx record [recordMask:um] options
	# (vm,_)	= fx (fmap fromRECORD record) (childMasksN recordMask grd_arity) options
	| isTouched recordMask
		= ([VMValid Nothing vm], um)
		= ([VMUntouched Nothing options.VerifyOptions.optional vm], um)

gVerify{|FIELD|} fx field um options = fx (fmap fromFIELD field) um options

gVerify{|OBJECT|} fx object [] options = ([],[])

gVerify{|OBJECT of {gtd_num_conses}|} fx object [consMask:um] options=:{VerifyOptions|optional}
	# hint = if (gtd_num_conses == 1) Nothing (Just "Select an option")
	= case consMask of
		Blanked | not optional	= ([VMInvalid BlankError []],um)
		Untouched
			# (vm,_)	= fx (fmap fromOBJECT object) [consMask] options
			= ([VMUntouched hint optional vm],um)
		_
			# (vm,_)	= fx (fmap fromOBJECT object) [consMask] options
			= ([VMValid hint vm],um)

gVerify{|CONS|} fx cons [] options = ([],[])
gVerify{|CONS of {gcd_arity}|} fx cons [consMask:um] options
	= fx (fmap fromCONS cons) (childMasksN consMask gcd_arity ++ um) options

gVerify{|Int|} _ um options = simpleVerify "You may enter an integer number" um options
gVerify{|Real|} _ um options = simpleVerify "You may enter a decimal number" um options
gVerify{|Char|} _ um options = simpleVerify "You may enter a single character" um options
gVerify{|String|} _ um options = simpleVerify "You may enter a single line of text" um options
gVerify{|Bool|} _ um options = alwaysValid um

gVerify{|Maybe|} fx mb um options
	= fx (fromMaybe Nothing mb) um {VerifyOptions|options & optional = True}

gVerify{|[]|} fx mbList [mask:um] options=:{VerifyOptions|optional,disabled}
	# list	= fromMaybe [] mbList
	# lm	= verifyListItems list (childMasksN mask (length list))
	| not (isTouched mask) && not disabled
		= ([VMUntouched Nothing optional lm], um)
	| otherwise
		= ([VMValid Nothing lm], um)
where
	verifyListItems [] um = []
	verifyListItems [x:xs] um
		# (vmx,um) = fx (Just x) um options
		= (vmx ++ verifyListItems xs um)
	
gVerify{|(->)|} _ _ _ um _	= alwaysValid um
gVerify{|Dynamic|}	_ um _	= alwaysValid um

gVerify{|HtmlTag|} _ um _	= alwaysValid um
gVerify{|JSONNode|}_ um _	= alwaysValid um

derive gVerify (,), (,,), (,,,), Void, Either, Timestamp, Map

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
	
instance GenMask VerifyMask
where
	popMask :: ![VerifyMask] -> (!VerifyMask, ![VerifyMask])
	popMask []					   		= (VMUntouched Nothing True [],[])
	popMask [c:cm]						= (c,cm)
	
	appendToMask :: ![VerifyMask] !VerifyMask -> [VerifyMask]
	appendToMask l c = l++[c]
	
	childMasks :: !VerifyMask -> [VerifyMask]
	childMasks (VMValid _ cm)				= cm
	childMasks (VMValidWithState _ cm _)	= cm
	childMasks (VMInvalid _ cm)				= cm
	childMasks (VMInvalidWithState _ cm _)	= cm
	childMasks (VMUntouched _ _ cm)			= cm
	
	childMasksN :: !VerifyMask !Int -> [VerifyMask]
	childMasksN m _ = childMasks m
	
	isTouched :: !VerifyMask -> Bool
	isTouched (VMUntouched _ _ _)			= False
	isTouched (VMInvalid BlankError _)		= False
	isTouched _								= True

alwaysValid :: ![InteractionMask] -> (![VerifyMask],![InteractionMask])
alwaysValid [] = ([],[])
alwaysValid [TouchedWithState s:um]	= ([VMValidWithState Nothing [] s],um)
alwaysValid [_:um]					= ([VMValid Nothing []],um)

simpleVerify :: !String ![InteractionMask] !VerifyOptions -> (![VerifyMask],![InteractionMask])
simpleVerify hint um options = customVerify (Just hint) (const True) (const undef) Nothing um options

customVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) ![InteractionMask] !VerifyOptions -> (![VerifyMask],![InteractionMask])
customVerify mbHint pred mkErrMsg mbVal [mask:um] options=:{VerifyOptions|optional,disabled} 
	# vm = case mask of
		Untouched				= VMUntouched mbHint optional []
		PartiallyTouched _		= maybe (VMValid mbHint []) (validateValue) mbVal
		Touched					= maybe (VMValid mbHint []) (validateValue) mbVal
		TouchedWithState s		= maybe (VMValidWithState mbHint [] s) (validateValueWithState s) mbVal		
		Blanked					= if optional (VMValid mbHint []) (VMInvalid BlankError [])
	= ([vm],um)
where
	validateValue val
		| pred val	= VMValid mbHint []
		| otherwise	= VMInvalid (FormatError (mkErrMsg val)) []
		
	validateValueWithState state val
		| pred val	= VMValidWithState mbHint [] state
		| otherwise	= VMInvalidWithState (FormatError (mkErrMsg val)) [] state  
	

setInvalid :: ![(!DataPath,!ErrorMessage)] !VerifyMask -> VerifyMask
setInvalid errors mask = seq (map setInvalid` errors) mask
where
	setInvalid` (p,msg) mask = hd (setInvalid`` p [mask])
	where
		setInvalid`` [0]	[mask:masks] = [VMInvalid msg (childMasks mask):masks]
		setInvalid`` [0:p]	[mask:masks] = [setChildren mask (setInvalid`` p (childMasks mask)):masks]
		setInvalid`` [n:p]	[mask:masks] = [mask:setInvalid`` [dec n:p] masks]
		
	setChildren (VMUntouched	mbHint optional _)	children = VMUntouched	mbHint optional children
	setChildren (VMValid	 	mbHint _)			children = VMValid	 	mbHint children
	setChildren (VMInvalid		errorM _)			children = VMInvalid	errorM children
