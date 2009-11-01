implementation module CommonDomain

import iTasks
import StdOverloaded, StdClass, StdInt, StdMisc, StdArray
import GenPrint, GenParse, GenVisualize, GenUpdate, GenLexOrd
import Text, Time

derive gPrint		EmailAddress, Password, Note, Date, Time, Currency
derive gParse		EmailAddress, Password, Note, Date, Time, Currency
derive gVisualize	EmailAddress, Password
derive gUpdate		EmailAddress, Password, Note
derive gLexOrd		Currency

gVisualize{|Date|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSDateField {ExtJSDateField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old, format = "d-m-Y", fieldLabel = label2s optional label, hideLabel = not useLabels})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath old optional valid})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath new optional valid})

gVisualize{|Time|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTimeField {ExtJSTimeField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old, format = "H:i:s", fieldLabel = label2s optional label, hideLabel = not useLabels})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath old optional valid})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath new optional valid})

gVisualize{|Note|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextArea {ExtJSTextArea|name = dp2s contentPath, id = dp2id idPrefix contentPath, value = value2s contentPath old, fieldLabel = label2s optional label, hideLabel = not useLabels, width = 400, height = 150 })]
							, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath old optional valid})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath new optional valid})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	contentPath				= shiftDataPath currentPath				

gVisualize{|Currency|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition
			= ([ExtJSFragment combinedPanel], {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath old optional valid})
		_
			= ([TextFragment (toString old)],{VSt|vst & valid= stillValid currentPath new optional valid})
where
	combinedPanel			= ExtJSPanel {ExtJSPanel| layout = "hbox", autoHeight = True, fieldLabel = label2s optional label, items = [currencyLabel,numberField], buttons = [], border = False, bodyCssClass = ""}
	numberField				= ExtJSNumberField {ExtJSNumberField|name = dp2s currentPath, id = dp2id idPrefix currentPath
								, value = value currentPath old, fieldLabel = Nothing, hideLabel = True, allowDecimals = True, numDecimals = 2}
	currencyLabel			= ExtJSCustom (JSON ("{xtype : \"displayfield\", value : \"" +++ curLabel old +++ "\", style : \"padding: 3px 5px 2px 2px;\"}"))
	curLabel (VValue (EUR _) _)	= "&euro;"
	curLabel (VValue (GBP _) _)	= "&pound;"
	curLabel (VValue (USD _) _)	= "$"
	curLabel (VValue (JPY _) _)	= "&yen;"
	curLabel _					= ""

	value dp VBlank			= ""
	value dp (VValue v dm)	= if (isMasked dp dm) (decFormat (toInt v)) "" 
	 
gUpdate{|Date|} _ ust=:{USt|mode=UDCreate,world}
	# (date,world) = currentDate world
	= (date, {USt|ust & world = world})
gUpdate{|Date|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (fromString update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Date|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]})

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
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]})
gUpdate{|Time|} s ust = (s, ust)

gUpdate{|Currency|} _ ust=:{USt|mode=UDCreate} = (EUR 0, ust)
gUpdate{|Currency|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (parseUpdate s update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt| ust & currentPath = stepDataPath currentPath})
where
	parseUpdate orig update
		= case split "." update of
			[whole]		= replaceVal orig (100 * toInt whole)
			[whole,dec] = replaceVal orig (100 * toInt whole + (if (size dec == 1) (10 * toInt dec) (toInt (dec % (0,1)))))
			_			= orig
	
	replaceVal (EUR _) x = (EUR x)
	replaceVal (GBP _) x = (GBP x)
	replaceVal (USD _) x = (USD x)
	replaceVal (JPY _) x = (JPY x)

gUpdate{|Currency|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = [currentPath:mask]})	
gUpdate{|Currency|} s ust = (s,ust)

currentTime :: !*World -> (!Time,!*World)
currentTime world
	# (tm,world) = localTime world
	= ({Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec},world)

currentDate :: !*World -> (!Date,!*World)
currentDate world
	# (tm,world) = localTime world
	= ({Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year},world)
	
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