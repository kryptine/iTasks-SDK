implementation module GenVerify

import StdGeneric, StdBool, StdInt, StdList, StdTuple, StdFunc, Maybe, Functor, Util, Text, Generic
import GenUpdate, StdMisc

derive gVerify (,), (,,), (,,,), Void, Either, DateTime, Timestamp, Map, EmailAddress, Action, TreeNode, UserConstraint, ManagementMeta, TaskPriority, Tree
verifyForm :: !a !UpdateMask -> VerifyMask | gVerify{|*|} a
verifyForm val updateMask
	# verSt = gVerify{|*|} (Just val) {updateMask = [updateMask], verifyMask = [], optional = False, staticDisplay = False}
	= hd verSt.verifyMask

verifyValue :: !a -> Bool | gVerify{|*|}, gDefaultMask{|*|} a
verifyValue val = isValidValue (verifyForm val (defaultMask val))

isValidValue :: !VerifyMask -> Bool
isValidValue (VMUntouched _ optional cm)	= optional && (and (map isValidValue cm)) 
isValidValue (VMValid _ cm)					= and (map isValidValue cm)
isValidValue (VMInvalid _ _)				= False

//Generic Verify
generic gVerify a :: !(Maybe a) !*VerSt -> *VerSt

gVerify{|UNIT|} 			  _ 					vst = vst
gVerify{|PAIR|}			fx fy p						vst = fy (fmap fromPAIRY p) (fx (fmap fromPAIRX p) vst)
gVerify{|CONS|}			fx c						vst = fx (fmap fromCONS c) vst
gVerify{|FIELD|}		fx f						vst = fx (fmap fromFIELD f) vst

gVerify{|EITHER|} 		_  _  Nothing				vst = vst
gVerify{|EITHER|}		fx _  (Just (LEFT x))		vst	= fx (Just x) vst
gVerify{|EITHER|}		_  fy (Just (RIGHT y))		vst = fy (Just y) vst

gVerify{|OBJECT of d|}	fx    obj					vst=:{updateMask,verifyMask,optional}
	# val		= fmap fromOBJECT obj
	# (cmu,um)	= popMask updateMask
	# vst		= {vst & updateMask = childMasks cmu, verifyMask = []}
	# (consMask,vst) = case (isRecordType d,d.gtd_num_conses) of
		(False,1) // ADT's with just one constructor
			# vst=:{verifyMask = childMask} = fx val vst
			# vst							= {vst & verifyMask = childMask}
			| isTouched cmu					= (VMValid Nothing childMask,vst)
			| otherwise						= (VMUntouched Nothing optional childMask,vst)
		(False,_) // ADT's with multiple constructors
			# vst=:{verifyMask = childMask} = fx val {vst & optional = False}
			# vst							= {vst & verifyMask = childMask}
			 = case cmu of
			 	Blanked | not optional		= (VMInvalid IsBlankError childMask,vst)
				Untouched					= (VMUntouched (Just "Select an option") optional childMask,vst)
				_							= (VMValid (Just "Select an option") childMask,vst)
		(True,_) // Records
			//Only compute child verify mask if record has value. Else you can end up in endless recursion!
			# (childMask,vst) = case isJust obj of
				True
					# vst=:{verifyMask = childMask} = fx val {vst & optional = False}
					= (childMask,{vst & verifyMask = childMask})
				False
					= ([],vst)
			| isTouched cmu					= (VMValid Nothing childMask,vst)
			| otherwise						= (VMUntouched Nothing optional childMask,vst)		
	= {vst & updateMask = um, optional = optional, verifyMask = appendToMask verifyMask consMask}

gVerify{|[]|} fx mbL vst=:{optional,verifyMask,updateMask,staticDisplay}
	# (cm,um)			= popMask updateMask
	# l					= fromMaybe [] mbL
	# (listMask,vst)	= verifyList l cm vst	
	= {vst & updateMask = um, optional = optional, verifyMask = appendToMask verifyMask listMask}
where
	verifyList l cm vst
		# vst=:{verifyMask=childMask} = verifyItems fx l {vst & verifyMask = [], updateMask = childMasks cm, optional = False}
		# vst = {vst & verifyMask = childMask}
		| staticDisplay
				= (VMValid Nothing childMask,vst)
		| not (isTouched cm)
				= (VMUntouched Nothing optional childMask,vst)
		| isEmpty l
			| optional
				= (VMValid hintOpt childMask,vst)
			| otherwise
				= (VMInvalid (ErrorMessage "You must add at least one item") childMask,vst)
		| otherwise
				= (VMValid Nothing childMask,vst)
				
	verifyItems fx [] vst=:{optional}
		# vst = fx Nothing {vst & optional = True}
		= {vst & optional = optional}
	verifyItems fx [x:xs] vst
		# vst = fx (Just x) vst
		= verifyItems fx xs vst
	
	hintOpt	= Just "You may add list items"

gVerify{|Maybe|} fx mb vst=:{optional,verifyMask}
	# vst=:{verifyMask} = fx (fromMaybe Nothing mb) {vst & optional = True}
	= {vst & optional = optional}

gVerify{|Hidden|}				_ _ vst		= verifyHidden vst
gVerify{|Editable|}				fx e vst	= verifyEditable fx (fmap fromEditable e) vst
gVerify{|Display|}				fx d vst	= verifyDisplay fx (fmap fromDisplay d) vst
gVerify{|VisualizationHint|}	fx v vst	= case v of
	Just (VHEditable e)	= verifyEditable fx (Just e) vst
	Just (VHDisplay d)	= verifyDisplay fx (Just d) vst
	Just (VHHidden _)	= verifyHidden vst
	_					= vst
	
verifyEditable	fx e vst=:{staticDisplay}		= (\vst -> {vst & staticDisplay = staticDisplay}) (fx e {vst & staticDisplay = False})
verifyDisplay	fx d vst=:{staticDisplay}		= (\vst -> {vst & staticDisplay = staticDisplay}) (fx d {vst & staticDisplay = True})
verifyHidden	vst=:{verifyMask,updateMask}	= {vst & verifyMask = appendToMask verifyMask (VMValid Nothing []), updateMask = snd (popMask updateMask)}

gVerify{|ControlSize|}		fx v vst = fx (fmap fromControlSize v) vst
gVerify{|FillControlSize|}	fx v vst = fx (fmap fromFillControlSize v) vst
gVerify{|FillWControlSize|}	fx v vst = fx (fmap fromFillWControlSize v) vst
gVerify{|FillHControlSize|}	fx v vst = fx (fmap fromFillHControlSize v) vst

gVerify{|Int|}    				_ vst = simpleVerify "Enter a number" vst
gVerify{|Real|}   				_ vst = simpleVerify "Enter a decimal number" vst
gVerify{|Char|}   				_ vst = simpleVerify "Enter a character" vst
gVerify{|String|}				_ vst = simpleVerify "Enter a short text" vst
gVerify{|Bool|}   				_ vst = alwaysValid vst
gVerify{|Document|}				_ vst = simpleVerify "Upload a document" vst
gVerify{|Username|}				_ vst = simpleVerify "Enter a username" vst
gVerify{|Password|}				_ vst = simpleVerify "Enter a password" vst
gVerify{|Date|}					_ vst = simpleVerify "Enter a date" vst
gVerify{|Time|}					_ vst = simpleVerify "Enter a time of day" vst
gVerify{|Note|}					_ vst = simpleVerify "Enter a long text" vst
gVerify{|FormButton|}			_ vst = alwaysValid vst
gVerify{|EUR|}					_ vst = simpleVerify "Enter an amount in EUR" vst
gVerify{|USD|}					_ vst = simpleVerify "Enter an amount in USD" vst
gVerify{|User|}					_ vst = simpleVerify "Select a username" vst 
gVerify{|RadioChoice|} _ _		_ vst = simpleVerify "Choose one item" vst
gVerify{|ComboChoice|} _ _		_ vst = simpleVerify "Choose one item" vst
gVerify{|GridChoice|} _ _		_ vst = alwaysValid vst//simpleVerify "Choose one row" vst
gVerify{|CheckMultiChoice|} _ _	_ vst = simpleVerify "Choose a number of items" vst
gVerify{|Table|}				_ vst = alwaysValid vst
gVerify{|TreeChoice|} _ _		_ vst = simpleVerify "Choose an element of the tree" vst
gVerify{|HtmlInclude|}			_ vst = alwaysValid vst
gVerify{|HtmlTag|}				_ vst = alwaysValid vst

gVerify{|Dynamic|}			_ vst = alwaysValid vst
gVerify{|(->)|} _ _			_ vst = alwaysValid vst
gVerify{|JSONNode|}			_ vst = alwaysValid vst

gVerify{|DynamicChoice|} fx fy	(Just (DCCombo v)) vst = gVerify{|*->*->*|} fx fy (Just v) vst
gVerify{|DynamicChoice|} fx fy	(Just (DCRadio v)) vst = gVerify{|*->*->*|} fx fy (Just v) vst
gVerify{|DynamicChoice|} fx fy	(Just (DCTree v)) vst = gVerify{|*->*->*|} fx fy (Just v) vst
gVerify{|DynamicChoice|} fx fy	(Just (DCGrid v)) vst = gVerify{|*->*->*|} fx fy (Just v) vst
gVerify{|DynamicChoice|} fx fy	Nothing vst = alwaysValid vst

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
	
	isTouched :: !VerifyMask -> Bool
	isTouched (VMUntouched _ _ _)			= False
	isTouched (VMInvalid IsBlankError _)	= False
	isTouched _								= True
	
alwaysValid :: !*VerSt -> *VerSt
alwaysValid vst=:{verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing [])}
	
simpleVerify :: !String !*VerSt -> *VerSt
simpleVerify hint vst
	= customVerify (Just hint) (const True) (const "") (Just (abort "no value needed for simple verify")) vst

wrapperVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) !*VerSt -> *VerSt
wrapperVerify mbHint pred parseErr mbVal vst=:{updateMask, verifyMask, optional, staticDisplay} 
	# (cm,um) = popMask updateMask
	# vmask = case mbVal of
		Just val
			| staticDisplay
							= VMValid Nothing [VMValid Nothing []]
			| optional = case cm of
				Touched _	= validateValue val
				_			= VMValid Nothing [VMValid mbHint []]           
			| otherwise = case cm of
				Untouched	= VMUntouched Nothing False [VMUntouched mbHint False []]
				Blanked		= VMInvalid IsBlankError [VMInvalid IsBlankError []]
				Touched _	= validateValue val     
		Nothing
							= VMValid Nothing [VMValid mbHint []]
	= {vst & updateMask = um, verifyMask = appendToMask verifyMask vmask}
where
    validateValue val
	    | pred val  = VMValid Nothing [VMValid mbHint []]
	    | otherwise = VMInvalid (ErrorMessage "") [VMInvalid (ErrorMessage (parseErr val)) []]

customVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) !*VerSt -> *VerSt
customVerify mbHint pred mkErrMsg mbVal vst=:{updateMask, verifyMask, optional, staticDisplay} 
	# (cm,um) = popMask updateMask
	# vmask = case mbVal of
		Just val
			| staticDisplay
								= VMValid Nothing []
			| optional
				= case cm of
					Touched _	= validateValue val
					_			= VMValid mbHint []
			| otherwise
				= case cm of
					Untouched	= VMUntouched mbHint False []
					Blanked		= VMInvalid IsBlankError []
					Touched _	= validateValue val
		Nothing
								= VMValid mbHint []
	= {vst & updateMask = um, verifyMask = appendToMask verifyMask vmask}
where
	validateValue val
		| pred val	= VMValid mbHint []
		| otherwise	= VMInvalid (ErrorMessage (mkErrMsg val)) []

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
