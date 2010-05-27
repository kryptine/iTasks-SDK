implementation module CommonDomain

import iTasks
import StdOverloaded, StdClass, StdInt, StdMisc, StdArray
import GenPrint, GenParse, GenVisualize, GenUpdate, GenLexOrd
import Text, Time

derive gPrint			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState
derive gParse			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState
derive gVisualize		EmailAddress, DateTime
derive gUpdate			EmailAddress, Note, DateTime
derive gMerge			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState
derive gMakeSharedCopy	EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState
derive gMakeLocalCopy	EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState
derive gLexOrd			Currency
derive gError			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState
derive gHint			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormButton, ButtonState

derive bimap	Maybe, (,)

//VValue a DataMask
gVisualize{|FormButton|} old new vst=:{vizType,label=fLabel,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition
			# errMsg = getErrorMessage currentPath oldM errorMask
			# hntMsg = getHintMessage currentPath oldM hintMask
			= ([TUIFragment (TUIFormButtonControl {TUIButtonControl | label = label old, iconCls = icon old, name = dp2s currentPath, id = id, value = toString pressedOld, fieldLabel = labelAttr useLabels fLabel, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 1
				, {VSt | vst & currentPath = stepDataPath currentPath, valid = isValid currentPath oldM errorMask valid})
		VEditorUpdate
			# upd = if (pressedOld <> pressedNew) [TUIUpdate (TUISetValue id (toString pressedNew))] []
			# err = getErrorUpdate id currentPath newM errorMask
			# hnt = getHintUpdate id currentPath newM hintMask
			= ([err,hnt:upd]
				, 1
				, {VSt | vst & currentPath = stepDataPath currentPath, valid = isValid currentPath newM errorMask valid})
		_
			= ([TextFragment (label old)]
				, 1
				, {VSt | vst & currentPath = stepDataPath currentPath})
where
	id			= dp2id idPrefix currentPath
	
	label b		= case b of (VValue b _) = b.FormButton.label; _ = ""
	icon b		= case b of (VValue b _) = b.icon; _ = ""	
	
	pressedOld	= case old of (VValue ob _) = pressed ob; _ = False
	pressedNew	= case new of (VValue nb _) = pressed nb; _ = True
	pressed b	= case b.FormButton.state of
		Pressed		= True
		NotPressed	= False	
		
	isValid dp dm em valid
		| getErrorCount dp dm em > 0 	= False
		| otherwise 				= valid
		
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []

gVisualize{|Password|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
			# errMsg = getErrorMessage currentPath oldM errorMask
			# hntMsg = getHintMessage currentPath oldM hintMask
			= ([TUIFragment (TUIPasswordControl {TUIBasicControl | name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 1
				, {VSt | vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			# upd = if (oldV <> newV) [TUIUpdate (TUISetValue id newV)] []
			# err = getErrorUpdate id currentPath newM errorMask
			# hnt = getHintUpdate id currentPath newM hintMask
			= ([err,hnt:upd]
				, 1
				, {VSt | vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_					
			= ([TextFragment (foldr (+++) "" (repeatn (size oldV) "*"))],1,{VSt | vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
where
	id		= dp2id idPrefix currentPath
	oldV	= value2s currentPath old
	newV	= value2s currentPath new
	oldM	= case old of (VValue _ omask) = omask; _ = []
	newM	= case new of (VValue _ nmask) = nmask; _ = []
		
gVisualize{|Date|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
			# errMsg = getErrorMessage currentPath oldM errorMask
			# hntMsg = getHintMessage currentPath oldM hintMask
			= ([TUIFragment (TUIDateControl {TUIBasicControl | name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			# upd = if (oldV <> newV) [TUIUpdate (TUISetValue id newV)] []
			# err = getErrorUpdate id currentPath newM errorMask
			# hnt = getHintUpdate id currentPath newM hintMask
			= ([err,hnt:upd]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask new optional valid})
		_					
			= ([TextFragment (toString old)],1,{VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	oldV	= value2s currentPath old
	newV	= value2s currentPath new
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []
	
gVisualize{|Time|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
			# errMsg = getErrorMessage currentPath oldM errorMask
			# hntMsg = getHintMessage currentPath oldM hintMask
			= ([TUIFragment (TUITimeControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			# upd = if (oldV <> newV) [TUIUpdate (TUISetValue id newV)] []
			# err = getErrorUpdate id currentPath newM errorMask
			# hnt = getHintUpdate id currentPath newM hintMask
			= ([err,hnt:upd]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask new optional valid})
		_					
			= ([TextFragment (toString old)],1,{VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	oldV	= value2s currentPath old
	newV	= value2s currentPath new
	
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []
	
gVisualize{|Note|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
			# errMsg = getErrorMessage currentPath oldM errorMask
			# hntMsg = getHintMessage currentPath oldM hintMask
			= ([TUIFragment (TUINoteControl {TUIBasicControl|name = dp2s contentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 3
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath errorMask old optional valid})
		VEditorUpdate
			# upd = if (oldV <> newV) [TUIUpdate (TUISetValue id newV)] []
			# err = getErrorUpdate id currentPath newM errorMask
			# hnt = getHintUpdate id currentPath newM hintMask
			= ([err,hnt:upd]
					, 3
					, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath errorMask new optional valid})
		_					
			= ([HtmlFragment (flatten [[Text line,BrTag []] \\ line <- split "\n" (toString old)])]
				, 3
					, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath errorMask new optional valid})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	contentPath	= shiftDataPath currentPath
	id			= dp2id idPrefix contentPath
	oldV		= value2s contentPath old
	newV		= value2s contentPath new

	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []

gVisualize{|Currency|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
			# errMsg = getErrorMessage currentPath oldM errorMask
			# hntMsg = getHintMessage currentPath oldM hintMask
			= ([TUIFragment (TUICurrencyControl {TUICurrencyControl|id = id, name = dp2s currentPath
												, value = oldV, fieldLabel = labelAttr useLabels label
												, currencyLabel = curLabel old, optional = optional
												, staticDisplay = renderAsStatic
												, errorMsg = errMsg, hintMsg = hntMsg})]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			# upd = if (oldV <> newV) [TUIUpdate (TUISetValue id newV)] []
			# err = getErrorUpdate id currentPath newM errorMask
			# hnt = getHintUpdate id currentPath newM hintMask
			= ([err,hnt:upd]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_					
			= ([TextFragment (toString old)], 1, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	curLabel (VValue (EUR _) _)	= "&euro;"
	curLabel (VValue (GBP _) _)	= "&pound;"
	curLabel (VValue (USD _) _)	= "$"
	curLabel (VValue (JPY _) _) = "&yen;"
	curLabel _					= "&euro;" //Use the default currency
	
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []
	oldV	= value currentPath old
	newV	= value currentPath new
	value dp VBlank			= ""
	value dp (VValue v dm)	= if (isMasked dp dm) (decFormat (toInt v)) ""
	
	id = dp2id idPrefix currentPath

		
gUpdate{|FormButton|} _ ust=:{USt|mode=UDCreate}
	= ({FormButton | label = "Form Button", icon="", state = NotPressed}, ust)
gUpdate{|FormButton|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= ({s & state = if(update == "true") Pressed NotPressed}, toggleMask {USt|ust & mode = UDDone}) 
	| otherwise
		= (s, {USt | ust & currentPath = stepDataPath currentPath})
gUpdate{|FormButton|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})	
		
gUpdate{|Password|} _ ust=:{USt|mode=UDCreate} 
	= (Password "", ust)
gUpdate{|Password|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (Password update, toggleMask {USt | ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Password|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})	
	
gUpdate{|Date|} _ ust=:{USt|mode=UDCreate,world}
	# (date,world) = currentDate world
	= (date, {USt|ust & world = world})
gUpdate{|Date|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (fromString update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Date|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})

gUpdate{|Date|} s ust = (s, ust)

gUpdate{|Time|} _ ust=:{USt|mode=UDCreate,world}
	# (time,world) = currentTime world
	= (time, {USt|ust & world = world})
gUpdate{|Time|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (fromString update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Time|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})
gUpdate{|Time|} s ust = (s, ust)

gUpdate{|Currency|} _ ust=:{USt|mode=UDCreate} = (EUR 0, ust)
gUpdate{|Currency|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (parseUpdate s update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt| ust & currentPath = stepDataPath currentPath})
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

gUpdate{|Currency|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})	
gUpdate{|Currency|} s ust = (s,ust)

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
	html (Note msg) = [Text msg]

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