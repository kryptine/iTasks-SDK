implementation module CommonDomain

import iTasks
import StdOverloaded, StdClass, StdInt, StdMisc, StdArray
import GenVisualize, GenUpdate, GenLexOrd
import Text

derive gVisualize		EmailAddress
derive gUpdate			EmailAddress
derive gMerge			EmailAddress, Currency, FormButton, ButtonState
derive gVerify			EmailAddress

derive gLexOrd			Currency

derive JSONEncode		EmailAddress, Currency, FormButton, ButtonState
derive JSONDecode		EmailAddress, Currency, FormButton, ButtonState

derive bimap	Maybe, (,)

gVisualize{|FormButton|} old new vst=:{vizType,label=fLabel,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,updateMask,verifyMask,updates}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	= case vizType of
		VEditorDefinition
			# (valid, err, hnt) = verifyElementStr valid cmu cmv
			= ([TUIFragment (TUIFormButtonControl {TUIButtonControl | label = label old, iconCls = icon old, name = dp2s currentPath, id = id, value = toString pressedOld, fieldLabel = labelAttr useLabels fLabel, optional = optional, staticDisplay = renderAsStatic, errorMsg = err, hintMsg = hnt})]
				, {VSt | vst & currentPath = stepDataPath currentPath, valid = valid, updateMask = um, verifyMask = vm})
		VEditorUpdate
			# upd = if (pressedOld == pressedNew) (restoreField currentPath updates id (toString pressedOld)) [TUIUpdate (TUISetValue id (toString pressedNew))]
			#(valid,msg) = verifyElementUpd valid id cmu cmv
			= (upd ++ msg
				, {VSt | vst & currentPath = stepDataPath currentPath, valid = valid, updateMask = um, verifyMask = vm})
		_
			= ([TextFragment (label old)]
				, {VSt | vst & currentPath = stepDataPath currentPath})
where
	id			= dp2id idPrefix currentPath
	
	label b		= case b of (VValue b) = b.FormButton.label; _ = ""
	icon b		= case b of (VValue b) = b.icon; _ = ""	
	
	pressedOld	= case old of (VValue ob) = pressed ob; _ = False
	pressedNew	= case new of (VValue nb) = pressed nb; _ = True
	
	pressed b	= case b.FormButton.state of
		Pressed		= True
		NotPressed	= False	

gVisualize{|Currency|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,verifyMask,updateMask,updates}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	# oldV = value old cmu
	# newV = value new cmu
	= case vizType of
		VEditorDefinition	
			# (valid,err,hnt) = verifyElementStr valid cmu cmv
			= ([TUIFragment (TUICurrencyControl {TUICurrencyControl|id = id, name = dp2s currentPath
												, value = oldV, fieldLabel = labelAttr useLabels label
												, currencyLabel = curLabel old, optional = optional
												, staticDisplay = renderAsStatic
												, errorMsg = err, hintMsg = hnt})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= valid, updateMask = um, verifyMask = vm})
		VEditorUpdate
			# upd = if (oldV == newV) (restoreField currentPath updates id oldV) [TUIUpdate (TUISetValue id newV)]
			# (valid,msg) = verifyElementUpd valid id cmu cmv
			= (upd++msg
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = valid, updateMask = um, verifyMask = vm})
		_					
			= ([TextFragment (toString old)], {VSt|vst & currentPath = stepDataPath currentPath})
where
	curLabel (VValue (EUR _))	= "&euro;"
	curLabel (VValue (GBP _))	= "&pound;"
	curLabel (VValue (USD _))	= "$"
	curLabel (VValue (JPY _)) = "&yen;"
	curLabel _					= "&euro;" //Use the default currency
	
	value VBlank um			= ""
	value (VValue v) um = case um of (Touched _ _) = (decFormat (toInt v)); _ = ""
	
	id = dp2id idPrefix currentPath

//******************************************************************************************************************************************
		
gUpdate{|FormButton|} _ ust=:{USt|mode=UDCreate,newMask}
	= ({FormButton | label = "Form Button", icon="", state = NotPressed}, {USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|FormButton|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm, om) = popMask oldMask
	| currentPath == searchPath
		= ({s & state = if(update == "true") Pressed NotPressed}, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|FormButton|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
		
gUpdate{|Currency|} _ ust=:{USt|mode=UDCreate, newMask} 
	= (EUR 0,{USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Currency|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,newMask, oldMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (parseUpdate s update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
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
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
gUpdate{|Currency|} s ust = (s,ust)

//******************************************************************************************************************************************

gVerify{|FormButton|} _ vst = vst
gVerify{|Currency|} _ vst = basicVerify "Enter a currency value" vst


//******************************************************************************************************************************************
				
instance toString Currency
where
	toString (EUR x) = "EUR " +++ decFormat x
	toString (GBP x) = "GBP " +++ decFormat x
	toString (USD x) = "USD " +++ decFormat x
	toString (JPY x) = "JPY " +++ decFormat x

instance toInt Currency
where
	toInt (EUR val) = val
	toInt (GBP val) = val
	toInt (USD val) = val
	toInt (JPY val) = val
		
instance < Currency
where
	(<) x y = case x =?= y of
		LT	= True
		_	= False

instance zero Currency
where
	zero = EUR 0

instance + Currency
where
	(+) (EUR x) (EUR y) = EUR (x + y)
	(+) (GBP x) (GBP y) = GBP (x + y)
	(+) (USD x) (USD y) = USD (x + y)
	(+) (JPY x) (JPY y) = JPY (x + y)
	(+) _ _ = abort "Trying to add money of different currencies!"

instance - Currency
where
	(-) (EUR x) (EUR y) = EUR (x - y)
	(-) (GBP x) (GBP y) = GBP (x - y)
	(-) (USD x) (USD y) = USD (x - y)
	(-) (JPY x) (JPY y) = JPY (x - y)
	(-) _ _ = abort "Trying to subtract money of different currencies!"
	
