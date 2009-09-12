implementation module CommonDomain

import iTasks
import StdOverloaded, StdClass, StdInt, StdMisc, StdArray
import GenPrint, GenParse, GenVisualize, GenUpdate, GenLexOrd
import Text

derive gPrint		EmailAddress, Password, Note, Date, Time, Currency
derive gParse		EmailAddress, Password, Note, Date, Time, Currency
derive gVisualize	EmailAddress, Password
derive gUpdate		EmailAddress, Password, Note
derive gLexOrd		Currency

gVisualize{|Date|} old new vst=:{vizType,label,idPrefix,currentPath,optional,blank}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSDateField {ExtJSDateField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s blank old, format = "d-m-Y", fieldLabel = label2s optional label, hideLabel = isNothing label})], {VSt|vst & currentPath = stepDataPath currentPath})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath})

gVisualize{|Time|} old new vst=:{vizType,label,idPrefix,currentPath,optional,blank}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTimeField {ExtJSTimeField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s blank old, format = "H:i:s", fieldLabel = label2s optional label, hideLabel = isNothing label})], {VSt|vst & currentPath = stepDataPath currentPath})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath})

gVisualize{|Note|} old new vst=:{vizType,label,idPrefix,currentPath,optional,blank}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextArea {ExtJSTextArea|name = dp2s contentPath, id = dp2id idPrefix contentPath, value = value2s blank old, fieldLabel = label2s optional label, hideLabel = isNothing label, width = 400, height = 150 })], {VSt|vst & currentPath = stepDataPath currentPath})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	contentPath				= shiftDataPath currentPath				

gVisualize{|Currency|} old new vst=:{vizType,label,idPrefix,currentPath,optional,blank}
	= case vizType of
		VEditorDefinition
			= ([ExtJSFragment combinedPanel], {VSt|vst & currentPath = stepDataPath currentPath})
		_
			= ([TextFragment (toString old)],vst)
where
	combinedPanel			= ExtJSPanel {ExtJSPanel| layout = "hbox", fieldLabel = label2s optional label, items = [currencyLabel,numberField], buttons = [], border = False, bodyCssClass = ""}
	numberField				= ExtJSNumberField {ExtJSNumberField|name = dp2s currentPath, id = dp2id idPrefix currentPath
								, value = value2s blank (decFormat (toInt old)), fieldLabel = Nothing, hideLabel = True, allowDecimals = True, numDecimals = 2}
	currencyLabel			= ExtJSCustom (JSON ("{xtype : \"displayfield\", value : \"" +++ curLabel old +++ "\", style : \"padding: 3px 5px 2px 2px;\"}"))
	curLabel (EUR _)		= "&euro;"
	curLabel (GBP _)		= "&pound;"
	curLabel (USD _)		= "$"
	curLabel (JPY _)		= "&yen;"

gUpdate{|Date|} _ ust=:{USt|mode=UDCreate} = ({Date|year = 2000, mon = 1, day = 1}, ust)
gUpdate{|Date|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (fromString update, {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Date|} s ust = (s, ust)

gUpdate{|Time|} _ ust=:{USt|mode=UDCreate} = ({Time|hour = 0, min = 0, sec = 0}, ust)
gUpdate{|Time|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (fromString update, {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Time|} s ust = (s, ust)

gUpdate{|Currency|} _ ust=:{USt|mode=UDCreate} = (EUR 0, ust)
gUpdate{|Currency|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (parseUpdate s update, {USt|ust & mode = UDDone})
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
	
gUpdate{|Currency|} s ust = (s,ust)



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
		| x.hour < y.hour										= True
		| x.hour == y.hour && x.min < y.min						= True
		| x.hour == y.hour && x.min == y.min && x.sec < y.sec	= True
		| otherwise												= False
		
instance < Date
where
	(<) x y 
		| x.year < y.year										= True
		| x.year == y.year && x.mon < y.mon						= True
		| x.year == y.year && x.mon == y.mon && x.day < y.day	= True
		| otherwise												= False

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
	(+) x y = {hour = x.hour + y.hour, min = x.min + y.min, sec = x.sec + y.sec}

instance + Date
where
	(+) x y = {year = x.year + y.year, mon = x.mon + y.mon, day = x.day + y.day}

instance - Time
where
	(-) x y = {hour = x.hour - y.hour, min = x.min - y.min, sec = x.sec - y.sec}

instance - Date
where
	(-) x y = {year = x.year - y.year, mon = x.mon - y.mon, day = x.day - y.day}

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