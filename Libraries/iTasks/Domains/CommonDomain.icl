implementation module CommonDomain

import iTasks
import StdOverloaded, StdClass, StdInt, StdMisc, StdArray
import GenPrint, GenParse, GenLexOrd, GUICore
import GUICore

derive gPrint		EmailAddress, Password, Note, Date, Time, Currency
derive gParse		EmailAddress, Password, Note, Date, Time, Currency
derive gVisualize	EmailAddress, Password
derive gUpdate		EmailAddress, Password, Note, Currency
derive gLexOrd		Currency

gVisualize{|Date|} old new vst=:{vizType,label,idPrefix,dataPath}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSDateField {ExtJSDateField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = toString old, format = "d-m-Y", fieldLabel = label})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})

gVisualize{|Time|} old new vst=:{vizType,label,idPrefix,dataPath}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTimeField {ExtJSTimeField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = toString old, format = "H:i:s", fieldLabel = label})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})

gVisualize{|Note|} old new vst=:{vizType,label,idPrefix,dataPath}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextArea {ExtJSTextArea|name = dp2s contentPath, id = dp2id idPrefix contentPath, value = toString old, fieldLabel = label, width = 400, height = 150 })], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	contentPath				= shiftDataPath dataPath				

gVisualize{|Currency|} old new vst=:{vizType}
	= case vizType of
		//VEditorDefinition
		_
			= ([TextFragment (toString old)],vst)

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
	
//Utility functions
pad :: Int Int -> String
pad len num = (createArray (max 0 (len - size nums)) '0' ) +++ nums
where 
	nums = toString num

decFormat :: Int -> String
decFormat x = toString (x / 100) +++ "." +++ pad 2 (x rem 100)