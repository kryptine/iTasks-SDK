implementation module GenVerify

import StdMaybe, StdGeneric, StdBool, StdInt, Text, StdList
import GenUpdate, StdMisc

derive bimap (,), Maybe

derive gVerify (,), (,,), (,,,), Void, Either
derive JSONEncode VerifyMask, ErrorMessage

generic gVerify a :: (Maybe a) *VerSt -> *VerSt

verifyValue :: !a !UpdateMask -> VerifyMask | gVerify{|*|} a
verifyValue val updateMask
	# verSt = gVerify{|*|} (Just val) {VerSt | updateMask = updateMask, verifyMask = VMValid Nothing Nothing [], optional = False}
	= verSt.VerSt.verifyMask
	
//Generic Verify
gVerify{|UNIT|} 			  _ 					vst = vst

gVerify{|PAIR|} 		_  _  Nothing				vst = vst
gVerify{|PAIR|} 		fx fy (Just (PAIR x y))		vst = fy (Just y) (fx (Just x) vst)

gVerify{|EITHER|} 		_  _  Nothing				vst = vst
gVerify{|EITHER|}		fx _  (Just (LEFT x))		vst	= fx (Just x) vst
gVerify{|EITHER|}		_  fy (Just (RIGHT y))		vst = fy (Just y) vst 

import StdDebug

gVerify{|CONS of d|}    _     Nothing				vst = vst
gVerify{|CONS of d|}	fx    (Just (CONS x))		vst=:{VerSt | verifyMask,updateMask,optional}
	# (cm,um) = popMask updateMask
	# vst=:{VerSt | verifyMask=childMask} = fx (Just x) {VerSt | vst & optional = False, updateMask = cm, verifyMask = (VMValid Nothing Nothing [])}
	# children = getMaskChildren childMask
	| not (isEmpty d.gcd_fields) //record
		| allUntouched children
			= {VerSt | vst & verifyMask = appendToMask verifyMask (VMUntouched Nothing Nothing children), optional = optional, updateMask = um}
		| allValid children
			= {VerSt | vst & verifyMask = appendToMask verifyMask (VMValid Nothing Nothing children), optional = optional, updateMask = um}
		| otherwise
			= {VerSt | vst & verifyMask = appendToMask verifyMask (VMInvalid (ErrorMessage "One or more items contain errors or are still required") Nothing children), optional = optional, updateMask = um}
	| otherwise //adt
		# consMask = case optional of
			True
				= case cm of
					(Untouched _ _) = VMValid (Just "Select an option") Nothing children
					(Blanked _ _)	= VMValid (Just "Select an option") Nothing children
					(Touched _ _)
						| allValid children 	= VMValid (Just "Select an option") Nothing children
						| allUntouched children	= VMValid (Just "Select an option") Nothing children
						| otherwise				= VMInvalid (ErrorMessage "One or more items contain errors or are still required") Nothing children
			False
				= case cm of
					(Untouched _ _)	= VMUntouched (Just "Select an option") Nothing children
					(Blanked _ _)	= VMInvalid IsBlankError Nothing children
					(Touched _ _)
						| allValid children 	= VMValid (Just "Select an option") Nothing children
						| allUntouched children	= VMUntouched (Just "Select an option") Nothing children
						| otherwise				= VMInvalid (ErrorMessage "One or more items contain errors or are still required") Nothing children
		= {VerSt | vst & updateMask = um, optional = optional, verifyMask = appendToMask verifyMask consMask}	

gVerify{|FIELD of d|}   _	  Nothing				vst = vst
gVerify{|FIELD of d|}   fx    (Just (FIELD x))		vst = fx (Just x) vst

gVerify{|OBJECT of d|}  _     Nothing				vst = vst
gVerify{|OBJECT of d|}  fx	  (Just (OBJECT x))		vst
	= fx (Just x) vst
	
gVerify{|Int|}    _ vst = basicVerify "Enter a number" vst
gVerify{|Real|}   _ vst = basicVerify "Enter a decimal number" vst
gVerify{|Char|}   _ vst = basicVerify "Enter a character" vst
gVerify{|String|} _ vst = basicVerify "Enter a short text" vst
gVerify{|Bool|}   _ vst=:{VerSt | verifyMask,updateMask} 
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}
	
gVerify{|[]|} fx Nothing   vst=:{VerSt | optional}
	# msg = if optional "You may add list items" "Create at least one list item"
	 = basicVerify msg vst
gVerify{|[]|} fx (Just []) vst=:{VerSt | updateMask,verifyMask,optional}
	# (cm,um)	= popMask updateMask
	| optional
		={VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just "You may add list elements") Nothing [])}	
	# listMask  = case cm of 
					(Untouched _ _) 
						= (VMUntouched (Just "Create at least one list item") Nothing [])
					_ 
						= (VMInvalid (ErrorMessage "Create at least one list item") Nothing []) //Empty lists are invalid
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask listMask}
gVerify{|[]|} fx (Just x)  vst=:{VerSt | updateMask,verifyMask,optional}
	# listMask 	= (VMValid Nothing Nothing [])
	# (cm,um)	= popMask updateMask
	# vst=:{VerSt | verifyMask=childMask} = verifyItems fx x {VerSt | vst & verifyMask = listMask, updateMask = cm, optional = False}
	# children  = getMaskChildren childMask
	| allUntouched children
		= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMUntouched Nothing Nothing children), optional = optional}
	| allValid children
		= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing children), optional = optional}
	| otherwise
		= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMInvalid (ErrorMessage "One or more elements contain errors.") Nothing children), optional = optional}
where
	verifyItems fx [] vst = vst
	verifyItems fx [x:xs] vst
	# vst = fx (Just x) vst
	= verifyItems fx xs vst

gVerify{|Maybe|} fx (Just (Just x)) vst=:{VerSt | optional}
	# vst = fx (Just x) {VerSt | vst & optional = True}
	= {VerSt | vst & optional = optional}
gVerify{|Maybe|} fx (Just Nothing) vst=:{VerSt | optional} 
	# vst = fx Nothing {VerSt | vst & optional = True}
	= {VerSt | vst & optional = optional}
gVerify{|Maybe|} _ Nothing vst=:{VerSt | updateMask,verifyMask,optional}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}

gVerify{|Dynamic|} _ vst = vst

gVerify{|Document|} _ vst = basicVerify "Upload a document" vst

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

gVerify{|VisualizationHint|} fx Nothing vst = vst
gVerify{|VisualizationHint|} fx (Just (VHHidden x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}
gVerify{|VisualizationHint|} fx (Just (VHDisplay x)) vst=:{VerSt | verifyMask,updateMask}
	# (cm,um) = popMask updateMask
	= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid Nothing Nothing [])}
gVerify{|VisualizationHint|} fx (Just (VHEditable x)) vst = fx (Just x) vst

basicVerify :: String !*VerSt -> *VerSt
basicVerify msg vst=:{VerSt | updateMask,verifyMask,optional}
	# (cm,um)   = popMask updateMask
	| optional  = {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just msg) Nothing [])}	
	| otherwise = case cm of
		(Touched _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just msg) Nothing [])}
		(Untouched _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMUntouched (Just msg) Nothing [])}
		(Blanked _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMInvalid IsBlankError Nothing [])}
		(UMList _ _)
			= {VerSt | vst & updateMask = um, verifyMask = appendToMask verifyMask (VMValid (Just msg) Nothing [])}


allUntouched :: ![VerifyMask] -> Bool
allUntouched children = and [isUntouched c \\ c <- children]
where
	isUntouched (VMUntouched _ _ _) = True
	isUntouched _					= False

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
	popMask :: !VerifyMask -> (!VerifyMask, !VerifyMask)
	popMask (VMValid h f [c:cm])   		= (c,(VMValid h f cm))
	popMask (VMUntouched h f [c:cm])   	= (c,(VMUntouched h f cm))
	popMask (VMInvalid e f [c:cm]) 		= (c,(VMInvalid e f cm))
	popMask m					   		= (VMValid Nothing Nothing [], m)
	
	appendToMask :: !VerifyMask !VerifyMask -> VerifyMask
	appendToMask (VMValid h f cm) c   		= (VMValid h f (cm++[c]))
	appendToMask (VMUntouched h f cm) c  	= (VMUntouched h f (cm++[c]))
	appendToMask (VMInvalid e f cm) c 		= (VMInvalid e f (cm++[c]))
	
	getMaskChildren :: !VerifyMask -> [VerifyMask]
	getMaskChildren (VMValid _ _ cm)   		= cm
	getMaskChildren (VMInvalid _ _ cm) 		= cm
	getMaskChildren (VMUntouched _ _ cm) 	= cm