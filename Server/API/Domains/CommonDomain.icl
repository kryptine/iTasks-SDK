implementation module CommonDomain

import iTasks
import StdOverloaded, StdClass, StdInt, StdMisc, StdArray
import GenVisualize, GenUpdate, GenLexOrd
import Text, Time

derive gVisualize		EmailAddress, DateTime
derive gUpdate			EmailAddress, DateTime
derive gMerge			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState
derive gVerify			EmailAddress, DateTime

derive gLexOrd			Currency

derive JSONEncode		EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState
derive JSONDecode		EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState

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

gVisualize{|Password|} old new vst=:{VSt | vizType,currentPath}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUIPasswordControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_					
			= ([TextFragment ("********")],{VSt | vst & currentPath = stepDataPath currentPath})
		
gVisualize{|Date|} old new vst=:{VSt | vizType,currentPath}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUIDateControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_					
			= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath})
	
gVisualize{|Time|} old new vst=:{VSt | vizType,currentPath,updateMask,idPrefix}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUITimeControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_					
			= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath})
	
gVisualize{|Note|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,verifyMask,updateMask,updates}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUINoteControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_					
			= ([HtmlFragment (flatten [[Text line,BrTag []] \\ line <- split "\n" (toString old)])]
					, {VSt|vst & currentPath = stepDataPath currentPath})

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
		
gUpdate{|Password|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (Password "", {USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Password|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (Password update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Password|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
	
gUpdate{|Date|} _ ust=:{USt|mode=UDCreate,newMask,iworld=iworld=:{IWorld|world}}
	# (date,world) = currentDate world
	= (date, {USt|ust & iworld = {IWorld|iworld & world = world}, newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Date|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (fromString update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Date|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
gUpdate{|Date|} s ust = (s, ust)

gUpdate{|Time|} _ ust=:{USt|mode=UDCreate,newMask,iworld=iworld=:{IWorld|world}}
	# (time,world) = currentTime world
	= (time, {USt|ust & iworld = {IWorld|iworld & world = world}, newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Time|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (fromString update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Time|} s ust=:{USt|mode=UDMask,currentPath,newMask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])}) 
gUpdate{|Time|} s ust = (s, ust)

gUpdate{|Note|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (Note "", {USt | ust & newMask = appendToMask newMask (Untouched False [])})
gUpdate{|Note|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om) = popMask oldMask
	| currentPath == searchPath
		= (Note update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (toggleMask update), oldMask = om}) 
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
gUpdate{|Note|} s ust=:{USt|mode=UDMask,currentPath,newMask}
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
gVerify{|Password|} _ vst = basicVerify "Enter a password" vst
gVerify{|Date|} _ vst = basicVerify "Enter a date" vst
gVerify{|Time|} _ vst = basicVerify "Enter a time of day" vst
gVerify{|Currency|} _ vst = basicVerify "Enter a currency value" vst
gVerify{|Note|} _ vst = basicVerify "Enter a text" vst

//******************************************************************************************************************************************

currentTime :: !*World -> (!Time,!*World)
currentTime world
	# (tm,world) = localTime world
	= ({Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec},world)

currentDate :: !*World -> (!Date,!*World)
currentDate world
	# (tm,world) = localTime world
	= ({Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year},world)

currentDateTime :: !*World -> (!DateTime,!*World)
currentDateTime world
	# (tm,world)	= localTime world
	# date			= {Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year}
	# time			= {Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec}
	= (DateTime date time,world)

instance html Note
where
	html (Note msg) = Text msg
	
instance == Note
where
	(==) (Note x) (Note y) = x == y

instance toString Time
where
	toString {Time|hour,min,sec}	= (pad 2 hour) +++ ":" +++ (pad 2 min) +++ ":" +++ (pad 2 sec)

instance fromString Time
where
	fromString s					= {Time|hour = toInt (s %(0,1)), min = toInt (s %(3,4)), sec = toInt (s %(6,7)) }

instance toString Date
where
	toString {Date|year,mon,day}	= (pad 2 day) +++ "-" +++ (pad 2 mon) +++ "-" +++ (pad 4 year)

instance fromString Date
where
	fromString s					= {Date|day = toInt (s %(0,1)), mon = toInt (s %(3,4)), year = toInt (s %(6,9))}

instance toString DateTime
where
	toString (DateTime d t) = toString d +++ " " +++ toString t

instance toString Note
where
	toString (Note s)				= s
				
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

instance < Time
where
	(<) x y
		| x.Time.hour < y.Time.hour															= True
		| x.Time.hour == y.Time.hour && x.Time.min < y.Time.min								= True
		| x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec < y.Time.sec	= True
		| otherwise																			= False
		
instance < Date
where
	(<) x y 
		| x.Date.year < y.Date.year															= True
		| x.Date.year == y.Date.year && x.Date.mon < y.Date.mon								= True
		| x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day < y.Date.day	= True
		| otherwise																			= False

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

instance + Time
where
	(+) x y = {Time|hour = x.Time.hour + y.Time.hour, min = x.Time.min + y.Time.min, sec = x.Time.sec + y.Time.sec}

instance + Date
where
	(+) x y = {Date|year = x.Date.year + y.Date.year, mon = x.Date.mon + y.Date.mon, day = x.Date.day + y.Date.day}

instance - Time
where
	(-) x y = {Time|hour = x.Time.hour - y.Time.hour, min = x.Time.min - y.Time.min, sec = x.Time.sec - y.Time.sec}

instance - Date
where
	(-) x y = {Date|year = x.Date.year - y.Date.year, mon = x.Date.mon - y.Date.mon, day = x.Date.day - y.Date.day}

instance - Currency
where
	(-) (EUR x) (EUR y) = EUR (x - y)
	(-) (GBP x) (GBP y) = GBP (x - y)
	(-) (USD x) (USD y) = USD (x - y)
	(-) (JPY x) (JPY y) = JPY (x - y)
	(-) _ _ = abort "Trying to subtract money of different currencies!"
	
//Utility functions
pad :: Int Int -> String
pad len num = (createArray (max 0 (len - size nums)) '0' ) +++ nums
where 
	nums = toString num

decFormat :: Int -> String
decFormat x = toString (x / 100) +++ "." +++ pad 2 (x rem 100)