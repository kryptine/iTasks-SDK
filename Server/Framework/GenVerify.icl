implementation module GenVerify

import StdGeneric, StdBool, StdInt, StdList, StdTuple, StdFunc, Maybe, Functor, Util, Text
import GenUpdate, StdMisc

derive gVerify (,), (,,), (,,,), Void, Either, UserDetails, DateTime, Timestamp, Map, EmailAddress, Action, ProcessRef, TreeNode, Table
derive bimap (,), Maybe

generic gVerify a :: (Maybe a) VerSt -> VerSt

verifyValue :: !a !UpdateMask !*IWorld -> (!VerifyMask, !*IWorld) | gVerify{|*|} a
verifyValue val updateMask iworld
	# verSt = gVerify{|*|} (Just val) {updateMask = [updateMask], verifyMask = [], optional = False, iworld = iworld}
	= (hd verSt.verifyMask, verSt.VerSt.iworld)

isValidValue :: !VerifyMask -> Bool
isValidValue (VMUntouched _ optional cm)	= optional && (and (map isValidValue cm)) 
isValidValue (VMValid _ cm)					= and (map isValidValue cm)
isValidValue (VMInvalid _ _)				= False

//Generic Verify
gVerify{|UNIT|} 			  _ 					vst = vst
gVerify{|PAIR|}			fx fy p						vst = fy (fmap fromPAIRY p) (fx (fmap fromPAIRX p) vst)
gVerify{|OBJECT|}		fx o						vst = fx (fmap fromOBJECT o) vst
gVerify{|FIELD|}		fx f						vst = fx (fmap fromFIELD f) vst

gVerify{|EITHER|} 		_  _  Nothing				vst = vst
gVerify{|EITHER|}		fx _  (Just (LEFT x))		vst	= fx (Just x) vst
gVerify{|EITHER|}		_  fy (Just (RIGHT y))		vst = fy (Just y) vst

gVerify{|CONS of d|}	fx    cons					vst=:{updateMask,verifyMask,optional}
	# val		= fmap fromCONS cons
	# (cmu,um)	= popMask updateMask
	# vst		= {vst & updateMask = childMasks cmu, verifyMask = []}
	# (consMask,vst) = case (d.gcd_fields,d.gcd_type_def.gtd_num_conses) of
		([],1) // ADT's with just one constructor
			# vst=:{verifyMask = childMask} 	= fx val vst
			# vst								= {vst & verifyMask = childMask}
			| optional || isTouched cmu			= (VMValid Nothing childMask,vst)
			| otherwise							= (VMUntouched Nothing optional childMask,vst)
		([],_) // ADT's with multiple constructors
			# vst=:{verifyMask = childMask} 	= fx val {vst & optional = False}
			# vst								= {vst & verifyMask = childMask}
			| optional || isTouched cmu			= (VMValid (Just "Select an option") childMask,vst)
			| otherwise = case cmu of
				Untouched						= (VMUntouched (Just "Select an option") False childMask,vst)
				Blanked							= (VMInvalid IsBlankError childMask,vst)
		_ // Records
			| not (isTouched cmu)				= (VMValid Nothing [],vst)
			| otherwise
												//Only compute child verify mask, if record is already touched. Else you can end up in endless recursion!
												# vst=:{verifyMask = childMask} = fx val {vst & optional = False}						
												= (VMValid Nothing childMask,{vst & verifyMask = childMask})
	= {vst & updateMask = um, optional = optional, verifyMask = appendToMask verifyMask consMask}

gVerify{|[]|} fx mbL vst=:{optional,verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	# (listMask,vst) = case mbL of
		Nothing
			= (if optional (VMValid hintOpt []) (VMInvalid err []),vst)
		Just l
			# vst=:{verifyMask=childMask} = verifyItems fx l {vst & verifyMask = [], updateMask = childMasks cm, optional = False}
			# vst = {vst & verifyMask = childMask}
			| not (isTouched cm)
					= (VMUntouched Nothing optional childMask,vst)
			| isEmpty l
				| optional
					= (VMValid hintOpt childMask,vst)
				| otherwise
					= (VMInvalid IsBlankError [],vst)
			| otherwise
					= (VMValid Nothing childMask,vst)
	= {vst & updateMask = um, optional = optional, verifyMask = appendToMask verifyMask listMask}
where
	err		= ErrorMessage "Create at least one list item"
	hintOpt	= Just "You may add list items"

verifyItems fx [] vst=:{optional}
	# vst = fx Nothing {vst & optional = True}
	= {vst & optional = optional}
verifyItems fx [x:xs] vst
	# vst = fx (Just x) vst
	= verifyItems fx xs vst

gVerify{|Maybe|} fx m vst=:{optional}
	# vst = fx (maybe Nothing id m) {vst & optional = True}
	= {vst & optional = optional}

gVerify{|Hidden|}				fx h vst = fx (fmap fromHidden h) vst
gVerify{|Editable|}				fx e vst = fx (fmap fromEditable e) vst
gVerify{|Display|}				fx d vst = fx (fmap fromDisplay d) vst
gVerify{|VisualizationHint|}	fx v vst = fx (fmap fromVisualizationHint v) vst

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
	= customWorldVerify (Just hint) (curry (appFst (const (WPRValid (Just hint))))) (Just (abort "no value needed for simple verify")) vst

wrapperVerify :: !(Maybe String) !(a -> Bool) !(a -> String) !(Maybe a) !*VerSt -> *VerSt
wrapperVerify mbHint pred parseErr mbVal vst=:{updateMask, verifyMask, optional} 
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
	= {vst & updateMask = um, verifyMask = appendToMask verifyMask vmask}
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
customWorldVerify mbHint pred mbVal vst=:{updateMask, verifyMask, optional} 
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
	= {vst & updateMask = um, verifyMask = appendToMask verifyMask vmask}
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
