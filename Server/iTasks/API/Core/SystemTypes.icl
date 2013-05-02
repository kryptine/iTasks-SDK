implementation module iTasks.API.Core.SystemTypes
from StdFunc import until

import StdInt, StdBool, StdClass, StdArray, StdTuple, StdMisc, StdList, StdFunc, StdOrdList
import List, JSON, HTML, Text, Map, Base64, Tuple, dynamic_string
import iTasks.Framework.GenVisualize, iTasks.Framework.GenUpdate
import iTasks.Framework.Util

from Time 		import :: Timestamp(..)
from iTasks.Framework.Task		import :: TaskValue

from iTasks.Framework.UIDefinition import :: UIDef(..), :: UIControlSequence, :: UIActionSet, :: UIControlGroup, :: UIActions, :: UIControls, :: UITitle, :: UIDirection(..), :: UIAnnotatedControls, :: UIAbstractContainer, :: UIViewport, :: UIAction, :: UIControl, stringDisplay
from iTasks.API.Core.LayoutCombinators import mergeAttributes, setMargins

//* EmailAddress
derive JSONEncode		EmailAddress
derive JSONDecode		EmailAddress
derive gDefault			EmailAddress
derive gEq				EmailAddress
derive gVisualizeText	EmailAddress
derive gVisualizeEditor	EmailAddress
derive gHeaders			EmailAddress
derive gGridRows		EmailAddress
derive gUpdate			EmailAddress
derive gVerify			EmailAddress

//* URL
gVisualizeText{|URL|}	_ val	= [toString val]

gVisualizeEditor{|URL|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap (\(URL url) -> ATag [HrefAttr url] [Text url]) val},newMap)], vst)
		| otherwise
			# val = checkMask touched val
			# ui = UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap toString val}
			= ([(ui,addVerAttributes verRes newMap)],vst)

gUpdate{|URL|} target upd val = basicUpdate (\json url -> maybe url (\s -> URL s) (fromJSON json)) target upd val

gVerify{|URL|} _ um options = simpleVerify "Enter a uniform resource locator (URL)" um options

derive JSONEncode		URL
derive JSONDecode		URL
derive gDefault			URL
derive gEq				URL
derive gHeaders			URL
derive gGridRows		URL

instance toString URL
where
	toString (URL url) = url

instance html URL
where
	html (URL url) = ATag [HrefAttr url] [Text url]

//* Note
JSONEncode{|Note|} (Note txt) = [JSONString txt]

JSONDecode{|Note|} [JSONString txt:c] = (Just (Note txt),c)
JSONDecode{|Note|} c = (Nothing,c)

gVisualizeText{|Note|}			_ val	= [toString val]

gVisualizeEditor{|Note|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
//		| disabled	= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap (\(Note v) -> Text v) val},newMap)],vst)
		| disabled	= ([(setMargins 5 5 5 5 (UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap noteToHtml val}),newMap)],vst)

		| otherwise	= ([(UIEditNote sizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(Note v) -> v) val},addVerAttributes verRes newMap)],vst)
	
	sizeOpts = {UISizeOpts|defaultSizeOpts & height = Just FlexSize, minHeight = Just WrapMin}
	
	// THIS IS A HACK!
	// The encoding of a Text constructor should escape newlines and convert them to <br> tags. Unfortunately it doesn't
	noteToHtml (Note s)	//TODO: Fix this in the toString of the Text constructor of HtmlTag type
		= case split "\n" s of
			[line]	= Text line
			lines	= SpanTag [] (intersperse (BrTag []) (map Text lines))

gUpdate{|Note|} target upd val = basicUpdateSimple target upd val

gVerify{|Note|} _ um options = simpleVerify "Enter a long text" um options

derive gDefault			Note
derive gEq				Note
derive gHeaders			Note
derive gGridRows		Note

instance toString Note
where
	toString (Note s) = s

instance html Note
where
	html (Note msg) = Text msg

instance == Note
where
	(==) (Note x) (Note y) = x == y
	
//* Source code
JSONEncode{|CleanCode|} (CleanCode txt) = [JSONString txt]

JSONDecode{|CleanCode|} [JSONString txt:c] = (Just (CleanCode txt),c)
JSONDecode{|CleanCode|} c = (Nothing,c)

gVisualizeText{|CleanCode|}		_ val		= [toString val]

gVisualizeEditor{|CleanCode|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(setMargins 5 5 5 5 (UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap codeToHtml val}),newMap)],vst)
		| otherwise	= ([(UIEditCode sizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(CleanCode v) -> JSONString v) val} {UICodeOpts|lineNumbers=True},addVerAttributes verRes newMap)],vst)
	
	sizeOpts = {UISizeOpts|defaultSizeOpts & height = Just FlexSize, minHeight = Just WrapMin}
	
	codeToHtml (CleanCode s)
		= case split "\n" s of
			[line]	= Text line
			lines	= SpanTag [] (intersperse (BrTag []) (map Text lines))

gUpdate{|CleanCode|} target upd val = basicUpdate codeUpd target upd val
where
	codeUpd (JSONString s) _	= CleanCode s
	codeUpd _ old				= old			

gVerify{|CleanCode|} _ um options = simpleVerify "Enter a piece of Clean code" um options

derive gDefault		CleanCode
derive gEq			CleanCode
derive gHeaders		CleanCode
derive gGridRows	CleanCode

instance toString CleanCode
where
	toString (CleanCode s) = s

//* Money (ISO4217 currency codes are used)

gVisualizeText{|EUR|} _ val = [toString val]

gVisualizeEditor{|EUR|}	val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap (\(EUR v) -> toString v) val},newMap)],vst)
		| otherwise	= ([(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(EUR v) -> toReal v / 100.0) val},addVerAttributes verRes newMap)],vst)

gUpdate{|EUR|} target upd val = basicUpdateSimple target upd val

gVerify{|EUR|} _ um options = simpleVerify "Enter an amount in EUR" um options

instance toString EUR
where
	toString c = "EUR " +++ decFormat (toInt c)
	
instance + EUR
where
	(+) (EUR x) (EUR y) = EUR (x + y)

instance - EUR
where
	(-) (EUR x) (EUR y) = EUR (x - y)

instance == EUR
where
	(==) (EUR x) (EUR y) = x == y

instance < EUR
where
	(<) (EUR x) (EUR y) = x < y

instance toInt EUR
where
	toInt (EUR val) = val

instance zero EUR
where
	zero = EUR 0

gVisualizeText{|USD|} _ val = [toString val]

gVisualizeEditor{|USD|}	val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(USD v) -> toReal v / 100.0) val},addVerAttributes verRes newMap)],vst)

gUpdate{|USD|} target upd val = basicUpdateSimple target upd val

gVerify{|USD|} _ um options = simpleVerify "Enter an amount in USD" um options

instance toString USD
where
	toString c = "USD " +++ decFormat (toInt c)

instance + USD
where
	(+) (USD x) (USD y) = USD (x + y)

instance - USD
where
	(-) (USD x) (USD y) = USD (x - y)

instance == USD
where
	(==) (USD x) (USD y) = x == y

instance < USD
where
	(<) (USD x) (USD y) = x < y
	
instance toInt USD
where
	toInt (USD val) = val

instance zero USD
where
	zero = USD 0

derive JSONEncode		EUR, USD
derive JSONDecode		EUR, USD
derive gDefault			EUR, USD
derive gEq				EUR, USD
derive gHeaders			EUR, USD
derive gGridRows		EUR, USD

//* (Local) date and time

JSONEncode{|Date|} d		= [JSONString (toString d)]

JSONDecode{|Date|} [JSONString s:c] 	= (Just (fromString s), c)
JSONDecode{|Date|} c					= (Nothing, c)

gVisualizeText{|Date|} _ val = [toString val]

gVisualizeEditor{|Date|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val	= checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditDate defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gDefault{|Date|} _ = {day = 1, mon = 1, year = 1970}

gUpdate{|Date|} target upd val = basicUpdate (\json old -> (fromMaybe old (fromJSON json))) target upd val

gVerify{|Date|} _ um options = simpleVerify "Enter a date" um options

derive gEq			Date
derive gHeaders		Date
derive gGridRows	Date

instance toString Date
where
	toString {Date|year,mon,day}	= (pad 4 year) +++ "-" +++ (pad 2 mon) +++ "-" +++ (pad 2 day)

instance fromString Date
where
	fromString s					= {Date|day = toInt (s %(8,9)), mon = toInt (s %(5,6)), year = toInt (s %(0,3))}

instance + Date //Second date is treated as an interval to be added
where
	(+) x y = normDays (addYears y.Date.year (normMonths (addMonths y.Date.mon (normDays (addDays y.Date.day x)))))
		// last normDays to remove 29-2 in non leap year
	where
		addDays days date		= {Date|date & day = date.day + days}
		addMonths months date	= {Date|date & mon = date.mon + months}
		addYears years date		= {Date|date & year = date.year + years}

		normDays date
			# monthLength = monthLengthOfDate date
			| date.day <= monthLength
				= date
				= normDays (normMonths {date & mon = date.mon + 1, day = date.day - monthLength})

		normMonths date
			| date.mon <= 12
				= date
				= normMonths {date & year = date.year + 1, mon = date.mon - 12}

		monthLengthOfDate date=:{mon}
			| mon==2
				| isLeapYear date.year
					= 29
					= 28
			| mon==4 || mon==6 || mon==9 || mon==11
				= 30
				= 31

		isLeapYear year
			| year rem 4<>0
				= False
				= year rem 100 <> 0 || year rem 400 == 0

instance - Date
where
	(-) x y = {Date|year = x.Date.year - y.Date.year, mon = x.Date.mon - y.Date.mon, day = x.Date.day - y.Date.day}

instance == Date
where
	(==) x y	= x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day == y.Date.day
		
instance < Date
where
	(<) x y 
		| x.Date.year < y.Date.year															= True
		| x.Date.year == y.Date.year && x.Date.mon < y.Date.mon								= True
		| x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day < y.Date.day	= True
		| otherwise																			= False

JSONEncode{|Time|} t					= [JSONString (toString t)]
JSONDecode{|Time|} [JSONString s:c]		= (Just (fromString s), c)
JSONDecode{|Time|} c					= (Nothing, c)

gVisualizeText{|Time|} _ val = [toString val]

gVisualizeEditor{|Time|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditTime defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gUpdate{|Time|} target upd val = basicUpdate (\json old -> fromMaybe old (fromJSON json)) target upd val

gVerify{|Time|} _ um options = simpleVerify "Enter a time of day" um options

derive gDefault		Time
derive gEq			Time
derive gHeaders		Time
derive gGridRows	Time

instance toString Time
where
	toString {Time|hour,min,sec}	= (pad 2 hour) +++ ":" +++ (pad 2 min) +++ ":" +++ (pad 2 sec)

instance fromString Time
where
	fromString s					= {Time|hour = toInt (s %(0,1)), min = toInt (s %(3,4)), sec = toInt (s %(6,7)) }

instance + Time // Second time is treated as an interval
where
	(+) x y = normHours (addHours y.Time.hour (normMinutes (addMinutes y.Time.min (normSeconds (addSeconds y.Time.sec x)))))
	where
		addSeconds s t	= {Time|t & sec = t.Time.sec + s}
		normSeconds t	= {Time|t & min = t.Time.min + (t.Time.sec / 60), sec = t.Time.sec rem 60}
		addMinutes m t	= {Time|t & min = t.Time.min + m}
		normMinutes t	= {Time|t & hour = t.Time.hour + (t.Time.min / 60), min = t.Time.min rem 60}
		addHours h t	= {Time|t & hour = t.Time.hour + h}
		normHours t		= {Time|t & hour = t.Time.hour rem 24}
		
instance - Time
where
	(-) x y = normHours (subHours y.Time.hour (normMinutes (subMinutes y.Time.min (normSeconds (subSeconds y.Time.sec x)))))
	where
		subSeconds s t	= {t & sec = t.sec - s}
		normSeconds t
			# ns = t.Time.sec rem 60
			| ns < 0	= {Time|t & min = t.Time.min + (t.Time.sec / 60) - 1, sec = ns + 60}
						= {Time|t & min = t.Time.min + (t.Time.sec / 60), sec = ns}
		subMinutes m t	= {Time|t & min = t.Time.min - m}
		normMinutes t	
			# nm = t.Time.min rem 60
			| nm < 0	= {Time|t & hour = t.Time.hour + (t.Time.min / 60) - 1, min = nm + 60}
						= {Time|t & hour = t.Time.hour + (t.Time.min / 60), min = nm}
		subHours h t	= {Time|t & hour = t.Time.hour - h}
		normHours t	
			# nh = t.Time.hour rem 24
			| nh < 0	= {Time|t & hour = nh + 24}
						= {Time|t & hour = nh}
		
instance == Time
where
	(==) x y = x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec == y.Time.sec
	
instance < Time
where
	(<) x y
		| x.Time.hour < y.Time.hour															= True
		| x.Time.hour == y.Time.hour && x.Time.min < y.Time.min								= True
		| x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec < y.Time.sec	= True
		| otherwise																			= False

JSONEncode{|DateTime|} dt	= [JSONString (toString dt)]

JSONDecode{|DateTime|} [JSONString s:c]	= (Just (fromString s), c)
JSONDecode{|DateTime|} c				= (Nothing, c)

derive gDefault			DateTime
derive gEq				DateTime
derive gVisualizeText	DateTime
derive gVisualizeEditor	DateTime
derive gHeaders			DateTime
derive gGridRows		DateTime
derive gUpdate			DateTime
derive gVerify			DateTime

instance toString DateTime
where
	toString (DateTime d t) = toString d +++ " " +++ toString t

instance fromString DateTime
where
	fromString s	= DateTime
						{Date|day = toInt (s %(8,9)), mon = toInt (s %(5,6)), year = toInt (s %(0,3))}
						{Time|hour = toInt (s %(11,12)), min = toInt (s %(14,15)), sec = toInt (s %(17,18)) }

instance + DateTime
where
	(+) (DateTime dx tx) (DateTime dy ty)
			| tn >= tx	= DateTime dn tn
			| otherwise	= DateTime (dn + {year = 0, mon = 0, day = 1}) tn	//We've passed midnight
	where
		dn = dx + dy
		tn = tx + ty
		
instance - DateTime
where
	(-) (DateTime dx tx) (DateTime dy ty) = DateTime (dx - dy) (tx - ty)

instance == DateTime
where
	(==) (DateTime dx tx) (DateTime dy ty)	= dx == dy && tx == ty

instance < DateTime
where
	(<) (DateTime dx tx) (DateTime dy ty)
		| dx < dy	= True
		| dx == dy	= (tx < ty)
		| otherwise	= False
		
//* Documents
gVisualizeText{|Document|} _ val
	| val.Document.size == 0			= ["No Document"]
	| otherwise							= [val.Document.name]

gVisualizeEditor {|Document|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewDocument defaultSizeOpts {UIViewOpts|value = val},newMap)],vst)
		| otherwise	= ([(UIEditDocument defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gUpdate {|Document|} [] upd (val,[dmask:mask]) = case fromJSON upd of
	Nothing		= ({Document|documentId = "", contentUrl = "", name="", mime="", size = 0},[Blanked:mask])// Reset
	Just doc	= (doc,[Touched:mask]) //Update 

gVerify{|Document|} _ um options = simpleVerify "Upload a document" um options

derive JSONEncode		Document
derive JSONDecode		Document
derive gDefault			Document
derive gEq				Document
derive gHeaders			Document
derive gGridRows		Document

instance toString Document
where
	toString doc = ""
	
instance == Document
where
	(==) doc0 doc1 = doc0.documentId == doc1.documentId

//* Authentication
JSONEncode{|Username|} (Username u) = [JSONString u]
JSONDecode{|Username|} [JSONString u:c] = (Just (Username u),c)

JSONDecode{|Username|} c = (Nothing,c)

gVisualizeEditor{|Username|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap (\(Username v) -> v) val},newMap)],vst)
		| otherwise	= ([(UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\(Username v) -> v) val},addVerAttributes verRes newMap)],vst)

gUpdate{|Username|} target upd val = basicUpdateSimple target upd val

gVerify{|Username|} _ um options = simpleVerify "Enter a username" um options

derive gDefault			Username
derive gEq				Username
derive gVisualizeText	Username
derive gHeaders			Username
derive gGridRows		Username

instance toString Username
where
	toString (Username u) = u

instance == Username
where
	(==) (Username a) (Username b)	= a == b

instance < Username
where
	(<) (Username a) (Username b) = a < b

JSONEncode{|Password|} (Password p) = [JSONString p]
JSONDecode{|Password|} [JSONString p:c] = (Just (Password p),c)

JSONDecode{|Password|} c = (Nothing,c)

gVisualizeText{|Password|} _ val = ["********"]

gVisualizeEditor{|Password|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = Just "********"},newMap)],vst)
		| otherwise	= ([(UIEditPassword defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value= fmap (\(Password v) -> v) val},addVerAttributes verRes newMap)],vst)

gUpdate{|Password|} target upd val = basicUpdateSimple target upd val

gVerify{|Password|} _ um options = simpleVerify "Enter a password" um options

derive gDefault			Password
derive gEq				Password
derive gHeaders			Password
derive gGridRows		Password

instance toString Password
where
	toString (Password p) = p
	
instance == Password
where
	(==) (Password a) (Password b) = a == b

instance < Password
where
	(<) (Password a) (Password b) = a < b

derive class iTask		Credentials

//* Common exceptions used by API tasks
instance toString FileException
where
	toString (FileException path error) = case error of
		CannotOpen	= "Cannot open file '" +++ path +++ "'"
		CannotClose	= "Cannot close file '" +++ path +++ "'"
		IOError		= "Error reading/writing file '" +++ path +++ "'"
	
instance toString ParseException
where
	toString (CannotParse err) = "Parse error: " +++ err
	
instance toString CallException
where
	toString (CallFailed (_,err)) = "Error calling external process: " +++ err
	
instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err
	
instance toString RPCException
where
	toString (RPCException err) = "Error performing RPC call: " +++ err
	
instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err
	
instance toString WorkOnException
where
	toString WorkOnNotFound				= "Error working on process: cannot find process"
	toString WorkOnEvalError			= "Error working on process: evaluation error"
	toString WorkOnDependencyCycle		= "Error working on process: cycle in dependencies detected"

derive class iTask	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException
derive class iTask	FileError

//* Geograpic data and Google Maps
gVisualizeEditor{|GoogleMap|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId}
		# editOpts	= {UIEditOpts|taskId=toString taskId,editorId=name,value=Nothing}
		# opts		= mapOpts (fromMaybe defaultValue val)
		= ([(UIEditGoogleMap defaultSizeOpts editOpts opts,addVerAttributes verRes newMap)],vst)
	
	mapOpts map =
		{ UIGoogleMapOpts
		| center = (map.perspective.GoogleMapPerspective.center.lat,map.perspective.GoogleMapPerspective.center.lng)
		, mapType = mapType map.perspective.GoogleMapPerspective.type
		, markers = [{UIGoogleMapMarker|position=(lat,lng),title=title,icon=icon,infoWindow=fmap toString infoWindow,draggable=draggable,selected=selected}
					\\ {GoogleMapMarker|position={lat,lng},title,icon,infoWindow,draggable,selected} <- map.GoogleMap.markers]
		, options =
			{ UIGoogleMapOptions
			| mapTypeControl = map.settings.GoogleMapSettings.mapTypeControl
			, panControl = map.settings.GoogleMapSettings.panControl
			, streetViewControl = map.settings.GoogleMapSettings.streetViewControl
			, zoomControl = map.settings.GoogleMapSettings.zoomControl
			, scaleControl = map.settings.GoogleMapSettings.scaleControl
			, scrollwheel = map.settings.GoogleMapSettings.scrollwheel
			, draggable = map.settings.GoogleMapSettings.draggable
			, zoom = map.perspective.GoogleMapPerspective.zoom
			}
		}
	mapType ROADMAP 	= "ROADMAP"
	mapType SATELLITE 	= "SATELLITE"
	mapType HYBRID 		= "HYBRID"
	mapType TERRAIN 	= "TERRAIN"

gVisualizeText{|GoogleMapPosition|} _  {GoogleMapPosition|lat,lng} = [toString lat + " " + toString lng]

//Helper types for GoogleMap gUpdate instance
:: MVCUpdate = 
	{ center			:: !(Real,Real)
	, zoom				:: !Int
	, type				:: !GoogleMapType
	}	
	
:: MapClickUpdate = 
	{ event				:: !ClickEvent
	, point				:: !(Real,Real)
	}

:: ClickEvent	= LEFTCLICK | RIGHTCLICK | DBLCLICK

:: MarkerClickUpdate =
	{ index				:: !Int
	, event				:: !ClickEvent
	}
:: MarkerDragUpdate = 
	{ index				:: !Int
	, point				:: !(Real,Real)
	}

derive JSONDecode MVCUpdate, MapClickUpdate, ClickEvent, MarkerClickUpdate, MarkerDragUpdate

gUpdate{|GoogleMap|} target upd val = basicUpdate parseUpdate target upd val
where
	parseUpdate json orig
		# mbMVC		= fromJSON json
		| isJust mbMVC
			# {MVCUpdate|center=(lat,lng),zoom,type} = fromJust mbMVC
			= {GoogleMap | orig & perspective = {GoogleMapPerspective|orig.perspective & center = {lat=lat,lng=lng}, zoom = zoom, type = type}}
		# mbMarkerDrag = fromJSON json
		| isJust mbMarkerDrag
			# {MarkerDragUpdate|index,point=(lat,lng)}	= fromJust mbMarkerDrag
			= {GoogleMap | orig & markers = [if (i == index) {GoogleMapMarker|m & position = {lat=lat,lng=lng}} m \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		# mbMarkerClick = fromJSON json
		| isJust mbMarkerClick
			# {MarkerClickUpdate|index,event} = fromJust mbMarkerClick
			= {GoogleMap| orig & markers = [{GoogleMapMarker|m & selected = i == index} \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		| otherwise	
			= orig

gVerify{|GoogleMap|} _ um _ = alwaysValid um
//derive gVerify GoogleMap

gDefault{|GoogleMapPerspective|} _ =
	{ GoogleMapPerspective
	| type				= ROADMAP
	, center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
	, zoom				= 10
	}
gDefault{|GoogleMapSettings|} _ =
	{ GoogleMapSettings
	| mapTypeControl	= True
	, panControl		= True
	, streetViewControl	= True
	, zoomControl		= True
	, scaleControl		= True
	, scrollwheel		= True
	, draggable			= True
	}

derive JSONEncode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive JSONDecode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gDefault			GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gEq				GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gVisualizeText	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gVisualizeEditor GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gHeaders			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gGridRows		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gUpdate			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gVerify			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon

//* A sliding scale
gVisualizeText{|Scale|}	_ {Scale|cur} = [toString cur]

gVisualizeEditor{|Scale|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# val = checkMask touched val
		# sliderOpts	= {UISliderOpts|minValue=maybe 1 (\{Scale|min} -> min) val,maxValue=maybe 5 (\{Scale|max} -> max) val}
		| disabled									
			# viewOpts = {UIViewOpts|value = fmap curVal val}  
			= ([(UIViewSlider defaultSizeOpts viewOpts sliderOpts, newMap)],vst)
		| otherwise
			# editOpts = {UIEditOpts|taskId = toString taskId, editorId = name, value = fmap curVal val}
			= ([(UIEditSlider defaultSizeOpts editOpts sliderOpts, addVerAttributes verRes newMap)],vst)

	curVal {Scale|cur} = cur

gUpdate{|Scale|} target upd val
	= basicUpdate (\json i -> maybe i (\cur -> {Scale|i & cur = cur}) (fromJSON json)) target upd val

gVerify{|Scale|} _ um _ = alwaysValid um

gDefault{|Scale|} _ = {Scale|min=1,cur=3,max=5}
gHeaders{|Scale|} _	= [""]
gGridRows{|Scale|} _ _ = Nothing

//* Progress bars
gVisualizeText{|Progress|}	_ {Progress|description} = [description]

gVisualizeEditor{|Progress|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId}
		= ([(UIViewProgress defaultSizeOpts {UIViewOpts|value=fmap value val} {UIProgressOpts|text = text val},newMap)],vst)
	where
		text (Just {Progress|description}) 	= description
		text _								= ""
		
		value {Progress|progress=ProgressRatio ratio} 
			| ratio < 0.0	= ProgressRatio 0.0
			| ratio > 1.0	= ProgressRatio 1.0
							= ProgressRatio ratio
		value {Progress|progress} = progress

gUpdate{|Progress|}	target upd val = val

gVerify{|Progress|} _ um _ = alwaysValid um

gHeaders{|Progress|} _		= [""]
gGridRows{|Progress|} _ _	= Nothing

derive gDefault			Progress

gVisualizeText{|ProgressAmount|} _ ProgressUndetermined		= ["Undetermined"]
gVisualizeText{|ProgressAmount|} _ (ProgressRatio r)		= [toString (entier (100.0 * r)) + "%"]

derive gDefault			ProgressAmount
derive gVisualizeEditor ProgressAmount
derive gHeaders			ProgressAmount
derive gGridRows		ProgressAmount
derive gUpdate			ProgressAmount
derive gVerify			ProgressAmount

//* Inclusion of external html files
gVisualizeText{|HtmlInclude|}	_ (HtmlInclude location)	= ["<External html: " + location + ">"]

gVisualizeEditor{|HtmlInclude|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst
		= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value=fmap (\(HtmlInclude path) -> IframeTag [SrcAttr path] []) val},addVerAttributes verRes newMap)],vst)

gUpdate{|HtmlInclude|} target upd val = val

gVerify{|HtmlInclude|} _ um _ = alwaysValid um

derive gDefault HtmlInclude
derive gHeaders HtmlInclude
derive gGridRows HtmlInclude

//* Form buttons
gVisualizeText{|FormButton|}	_ val				= [val.FormButton.label]

gVisualizeEditor{|FormButton|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		# text = fmap (\b -> b.FormButton.label) val
		# iconCls = fmap (\b -> b.FormButton.icon) val
		= ([(UIEditButton defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap (\_ -> JSONString "pressed") val} {UIButtonOpts|text=text,iconCls=iconCls,disabled=False},addVerAttributes verRes newMap)],vst)

gUpdate{|FormButton|} target upd val
	= basicUpdate (\st b -> {FormButton|b & state = st}) target upd val

gVerify{|FormButton|} _ um _ = alwaysValid um

gDefault{|FormButton|} _ = {FormButton | label = "Form Button", icon="", state = NotPressed}

derive gHeaders FormButton
derive gGridRows FormButton

instance toString FormButton
where
	toString button = toString (pressed button)
	where
		pressed {FormButton|state}= case state of
			Pressed		= True
			NotPressed	= False

gVisualizeText{|ButtonState|}	_ NotPressed		= ["not pressed"]
gVisualizeText{|ButtonState|}	_ Pressed			= ["pressed"]

derive gDefault ButtonState
derive gVisualizeEditor ButtonState
derive gHeaders ButtonState
derive gGridRows ButtonState
derive gUpdate ButtonState
derive gVerify ButtonState

//* Table consisting of headers, the displayed data cells & possibly a selection
gVisualizeText{|Table|}	_ _	= ["<Table>"]

gVisualizeEditor{|Table|} val vst = visualizeCustom viz vst 
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		= ([(UIGrid defaultSizeOpts
			{UIChoiceOpts|taskId=toString taskId,editorId=name,value=value val,options = options val}
			{UIGridOpts|columns = columns val},addVerAttributes verRes newMap)],vst)
	
	value (Just (Table _ _ mbSel))	= maybe [] (\s->[s]) mbSel
	value _							= []
	
	columns (Just (Table headers _ _))	= headers
	columns _							= []
	
	options (Just (Table _ cells _))	= map (map toString) cells
	options _							= []

gUpdate{|Table|} target upd val
	= basicUpdate (\json (Table headers cells _) -> case fromJSON json of Just i = Table headers cells (Just i); _ = Table headers cells Nothing) target upd val

gVerify{|Table|} _ um _ = alwaysValid um
gDefault{|Table|} _ = Table [] [] Nothing

derive gHeaders Table
derive gGridRows Table

toTable	:: ![a] -> Table | gHeaders{|*|} a & gGridRows{|*|} a & gVisualizeText{|*|} a
toTable a = Table (headers a undef) (map row a) Nothing
where
	headers:: [a] a -> [String] | gHeaders{|*|} a
	headers _ a = gHeaders{|*|} a

	row x = case (gGridRows{|*|} x []) of
		Just cells	= [Text cell \\ cell <- cells]
		Nothing		= [Text (visualizeAsText AsLabel x)]
	
//* Simple tree type (used primarily for creating trees to choose from)
derive gDefault			Tree, TreeNode
derive gVisualizeText	Tree, TreeNode
derive gVisualizeEditor	Tree, TreeNode
derive gHeaders			Tree, TreeNode
derive gGridRows		Tree, TreeNode
derive gUpdate			Tree, TreeNode
derive gVerify			Tree, TreeNode
		
instance Functor Tree
where
	fmap f (Tree nodes) = Tree (map fmap` nodes)
	where
		fmap` node = case node of
			Leaf a			= Leaf (f a)
			Node a nodes	= Node (f a) [fmap` node \\ node <- nodes]


derive JSONEncode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive JSONDecode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode
derive gEq				Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, Tree, TreeNode 

//* Choices
gDefault{|ComboChoice|} _ _ _ = ComboChoice [] Nothing
gVisualizeText{|ComboChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))

gVisualizeEditor{|ComboChoice|} fx gx _ _ _ _ _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UIDropdown defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)

	vvalue (Just (ComboChoice options (Just sel)))	= Just (hd (gx AsLabel (fst(options !! sel ))))
	vvalue _										= Nothing
	evalue (Just (ComboChoice _ mbSel))				= maybe [] (\s->[s]) mbSel
	evalue _										= []
	options (Just (ComboChoice options _))			= [concat (gx AsLabel v) \\ (v,_) <- options]
	options	_										= []

 
gUpdate{|ComboChoice|} _ _ _ _ target upd val = updateChoice (\idx (ComboChoice options _) -> ComboChoice options idx) target upd val

gVerify{|ComboChoice|} _ _ v um options = customVerify (Just "Choose one item") (\(ComboChoice _ s) -> isJust s) (const "You must choose one item") v um options

instance Choice ComboChoice
where
	selectOption newSel (ComboChoice options _)					= ComboChoice options (setListOption options newSel)
	getSelection combo											= fromJust (getMbSelection combo)
	getMbSelection (ComboChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (ComboChoice options mbSel)				= fmap fst (getListOption options mbSel)

gDefault{|ComboChoiceNoView|} _ _ = ComboChoiceNoView [] Nothing
gVisualizeText{|ComboChoiceNoView|} fo mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))

gVisualizeEditor{|ComboChoiceNoView|} _ gx _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UIDropdown defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)
	
	vvalue (Just (ComboChoiceNoView options (Just sel)))	= Just (hd (gx AsLabel (options !! sel )))
	vvalue _												= Nothing
	evalue (Just (ComboChoiceNoView _ mbSel))				= maybe [] (\s->[s]) mbSel
	evalue _												= []
	options (Just (ComboChoiceNoView options _))			= [concat (gx AsLabel v) \\ v <- options]
	options	_												= []

gUpdate{|ComboChoiceNoView|} _ _ target upd val = updateChoice (\idx (ComboChoiceNoView options _) -> ComboChoiceNoView options idx) target upd val

gVerify{|ComboChoiceNoView|} _	v um options = customVerify (Just "Choose one item") (\(ComboChoiceNoView _ s) -> isJust s) (const "You must choose one item") v um options

instance ChoiceNoView ComboChoiceNoView
where
	selectOptionNoView newSel (ComboChoiceNoView options _)		= ComboChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView combo									= fromJust (getMbSelectionNoView combo)
	getMbSelectionNoView (ComboChoiceNoView options mbSel)		= getListOption options mbSel

gDefault{|RadioChoice|} _ _ _ = RadioChoice [] Nothing
gVisualizeText{|RadioChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))

gVisualizeEditor{|RadioChoice|} _ gx _ _ _ _ _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UIRadioGroup defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)

	vvalue (Just (RadioChoice options (Just sel)))	= Just (hd (gx AsLabel (fst(options !! sel ))))
	vvalue _										= Nothing
	evalue (Just (RadioChoice _ mbSel))				= maybe [] (\i -> [i]) mbSel
	evalue _										= []
	options (Just (RadioChoice options _))			= [concat (gx AsLabel v) \\ (v,_) <- options]
	options	_										= []

gUpdate{|RadioChoice|} _ _ _ _ target upd val
	= updateChoice (\idx (RadioChoice options _) -> RadioChoice options idx) target upd val

gVerify{|RadioChoice|} _ _		_ um options = simpleVerify "Choose one item" um options

instance Choice RadioChoice
where
	selectOption newSel (RadioChoice options _)					= RadioChoice options (setListOption options newSel)
	getSelection radios											= fromJust (getMbSelection radios)
	getMbSelection (RadioChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (RadioChoice options mbSel)				= fmap fst (getListOption options mbSel)

gDefault{|RadioChoiceNoView|} _ _ = RadioChoiceNoView [] Nothing
gVisualizeText{|RadioChoiceNoView|}	fo mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))

gVisualizeEditor{|RadioChoiceNoView|} _ gx _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UIRadioGroup defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)

	vvalue (Just (RadioChoiceNoView options (Just sel)))	= Just (hd (gx AsLabel (options !! sel )))
	vvalue _												= Nothing
	evalue (Just (RadioChoiceNoView _ mbSel))				= maybe [] (\s->[s]) mbSel
	evalue _												= []
	options (Just (RadioChoiceNoView options _))			= [concat (gx AsLabel v) \\ v <- options]
	options	_												= []

gUpdate{|RadioChoiceNoView|} _ _ target upd val
	= updateChoice (\idx (RadioChoiceNoView options _) -> RadioChoiceNoView options idx) target upd val

gVerify{|RadioChoiceNoView|} _	_ um options = simpleVerify "Choose one item" um options

instance ChoiceNoView RadioChoiceNoView
where
	selectOptionNoView newSel (RadioChoiceNoView options _)		= RadioChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView radios									= fromJust (getMbSelectionNoView radios)
	getMbSelectionNoView (RadioChoiceNoView options mbSel)		= getListOption options mbSel

gDefault{|TreeChoice|} _ _ _ = TreeChoice (Tree []) Nothing

gVisualizeText{|TreeChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))

gVisualizeEditor{|TreeChoice|} _ gx _ _ _ _ _ _ val vst=:{VSt|taskId,currentPath,disabled,verifyMask=[cmv:vm]}
	# ver		= verifyElementStr cmv
	# viz		= [(UITree defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=dp2s currentPath,value=value val,options = options val cmv},addVerAttributes ver newMap)]
	= (NormalEditor viz,{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
where
	value  (Just (TreeChoice _ mbSel)) 	= maybe [] (\s->[s]) mbSel
	value _								= []
	
	options (Just (TreeChoice (Tree nodes) _)) msk = fst (mkTree nodes 0 )
		where
			expanded = case msk of
				VMValidWithState _ _ s 		= case fromJSON s of Just expanded = expanded; _ = []
				VMInvalidWithState _ _ s	= case fromJSON s of Just expanded = expanded; _ = []
				_							= []
				
			mkTree [] idx
				= ([],idx)
			mkTree [Leaf (v,_):r] idx
				# (rtree,idx`) 		= mkTree r (inc idx)
				= ([{UITreeNode|text = concat (gx AsLabel v), value = idx, leaf = True, expanded = isMember idx expanded, children = Nothing}:rtree],idx`)
			mkTree [Node (v,_) nodes:r] idx
				# (children,idx`)	= mkTree nodes (inc idx)
				# (rtree,idx`)		= mkTree r idx`
				= ([{UITreeNode|text = concat (gx AsLabel v), value = idx, leaf = False, expanded = isMember idx expanded, children = Just children}:rtree],idx`)
	options _ _ = []

gUpdate{|TreeChoice|} _ _ _ _ [] upd (TreeChoice options sel,[cmask:mask]) = case fromJSON upd of
	Just ("sel",idx,val)	= (TreeChoice options (if val (Just idx) Nothing), [touch cmask:mask])
	Just ("exp",idx,val)	= (TreeChoice options sel, [if val (expand idx cmask) (collapse idx cmask):mask])
	_						= ((TreeChoice options sel), [cmask:mask])

gUpdate{|TreeChoice|} _ _ _ _ target upd val = val


gVerify{|TreeChoice|} _ _		_ um options = simpleVerify "Choose an element of the tree" um options

instance Choice TreeChoice
where
	selectOption newSel (TreeChoice options _)					= TreeChoice options (setTreeOption options newSel)
	getSelection tree											= fromJust (getMbSelection tree)
	getMbSelection (TreeChoice options mbSel)					= fmap snd (getTreeOption options mbSel)
	getMbSelectionView (TreeChoice options mbSel)				= fmap fst (getTreeOption options mbSel)

gDefault{|TreeChoiceNoView|} _ _ = TreeChoiceNoView (Tree []) Nothing

gVisualizeText{|TreeChoiceNoView|} fo mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))

gVisualizeEditor{|TreeChoiceNoView|} _ gx _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId}
		= ([(UITree defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=value val,options = options val},newMap)],vst)

	value (Just (TreeChoiceNoView _ mbSel)) = maybe [] (\s->[s]) mbSel
	value _									= []
	options (Just (TreeChoiceNoView (Tree nodes) _)) = fst (mkTree nodes 0)
	where
		mkTree [] idx
			= ([],idx)
		mkTree [Leaf v:r] idx
			# (rtree,idx`) 		= mkTree r (inc idx)
			= ([{UITreeNode|text = concat (gx AsLabel v), value = idx, leaf = True, expanded = False, children = Nothing}:rtree],idx`)
		mkTree [Node v nodes:r] idx
			# (children,idx`)	= mkTree nodes (inc idx)
			# (rtree,idx`)		= mkTree r idx`
			= ([{UITreeNode|text = concat (gx AsLabel v), value = idx, leaf = False, expanded = False, children = Just children}:rtree],idx`)
	options _ = []

gUpdate{|TreeChoiceNoView|} _ _ target upd val = updateChoice update target upd val
where
	update ("sel",idx,val)		(TreeChoiceNoView options _) 		= TreeChoiceNoView options (if val (Just idx) Nothing)
	update ("exp",idx,val)		(TreeChoiceNoView options sel)		= TreeChoiceNoView options sel
	update _					treechoice							= treechoice

gVerify{|TreeChoiceNoView|} _	_ um options = simpleVerify "Choose an element of the tree" um options
	
instance ChoiceNoView TreeChoiceNoView
where
	selectOptionNoView newSel (TreeChoiceNoView options _)		= TreeChoiceNoView options (setTreeOptionNoView options newSel)
	getSelectionNoView tree										= fromJust (getMbSelectionNoView tree)
	getMbSelectionNoView (TreeChoiceNoView options mbSel)		= getTreeOption options mbSel

gDefault{|GridChoice|} _ _ _ = GridChoice [] Nothing

gVisualizeText{|GridChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))	

gVisualizeEditor{|GridChoice|} _ gx hx ix _ _ _ _ val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		= ([(UIGrid defaultSizeOpts
			{UIChoiceOpts|taskId=toString taskId,editorId=name,value=value val,options = options val}
			{UIGridOpts|columns = hx undef},addVerAttributes verRes newMap)],vst)
	
	value (Just (GridChoice options mbSel)) = maybe [] (\s->[s]) mbSel
	value _									= []
	options (Just (GridChoice options _))	= [fromMaybe [concat (gx AsLabel opt)] (ix opt []) \\ (opt,_) <- options]
	options _								= []

gUpdate{|GridChoice|} _ _ _ _ target upd val
	= updateChoice (\idxs (GridChoice options _) -> GridChoice options (case idxs of [idx:_] = (Just idx); _ = Nothing)) target upd val

gVerify{|GridChoice|} _ _ _ um _ = alwaysValid um

instance Choice GridChoice
where
	selectOption newSel (GridChoice options _)					= GridChoice options (setListOption options newSel)
	getSelection grid											= fromJust (getMbSelection grid)
	getMbSelection (GridChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (GridChoice options mbSel)				= fmap fst (getListOption options mbSel)

gDefault{|GridChoiceNoView|} _ _ = GridChoiceNoView [] Nothing

gVisualizeText{|GridChoiceNoView|} fo mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))	

gVisualizeEditor{|GridChoiceNoView|} _ gx hx ix val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		= ([(UIGrid defaultSizeOpts
			{UIChoiceOpts|taskId=toString taskId,editorId=name,value=value val,options =options val}
			{UIGridOpts|columns = hx undef},newMap)],vst)
	
	value (Just (GridChoiceNoView options mbSel))	= maybe [] (\s->[s]) mbSel
	value _											= []
	options (Just (GridChoiceNoView options _))		= [fromMaybe [concat (gx AsLabel opt)] (ix opt []) \\ opt <- options]
	options _										= []

gUpdate{|GridChoiceNoView|} _ _ target upd val
	= updateChoice (\idxs (GridChoiceNoView options _) -> GridChoiceNoView options (case idxs of [idx:_] = (Just idx); _ = Nothing)) target upd val

gVerify{|GridChoiceNoView|} _	_ um _ = alwaysValid um
	
instance ChoiceNoView GridChoiceNoView
where
	selectOptionNoView newSel (GridChoiceNoView options _)		= GridChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView grid										= fromJust (getMbSelectionNoView grid)
	getMbSelectionNoView (GridChoiceNoView options mbSel)		= getListOption options mbSel

gDefault{|DynamicChoice|} fx fy path = DCRadio (gDefault{|*->*->*|} fx fy path)

gVisualizeText{|DynamicChoice|}		fv fo mode (DCRadio val)	= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCCombo val)	= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCGrid val)		= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCTree val)		= gVisualizeText{|*->*->*|} fv fo mode val

gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 (Just (DCCombo val)) vst
	= gVisualizeEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 (Just val) vst
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 (Just (DCRadio val)) vst
	= gVisualizeEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 (Just val) vst
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 (Just (DCTree val)) vst
	= gVisualizeEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 (Just val) vst
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 (Just (DCGrid val)) vst
	= gVisualizeEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 (Just val) vst
gVisualizeEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 Nothing vst
	= (NormalEditor [],vst)

gUpdate{|DynamicChoice|} gUpdx gDefx gUpdy gDefy target upd	(DCCombo val,mask)	= appFst DCCombo (gUpdate{|*->*->*|} gUpdx gDefx gUpdy gDefy target upd (val,mask))
gUpdate{|DynamicChoice|} gUpdx gDefx gUpdy gDefy target upd	(DCRadio val,mask)	= appFst DCRadio (gUpdate{|*->*->*|} gUpdx gDefx gUpdy gDefy target upd (val,mask))
gUpdate{|DynamicChoice|} gUpdx gDefx gUpdy gDefy target upd	(DCTree val,mask)	= appFst DCTree (gUpdate{|*->*->*|} gUpdx gDefx gUpdy gDefy target upd (val,mask))
gUpdate{|DynamicChoice|} gUpdx gDefx gUpdy gDefy target upd	(DCGrid val,mask)	= appFst DCGrid (gUpdate{|*->*->*|} gUpdx gDefx gUpdy gDefy target upd (val,mask))

gVerify{|DynamicChoice|} fx fy	(Just (DCCombo v)) um options = gVerify{|*->*->*|} fx fy (Just v) um options
gVerify{|DynamicChoice|} fx fy	(Just (DCRadio v)) um options = gVerify{|*->*->*|} fx fy (Just v) um options
gVerify{|DynamicChoice|} fx fy	(Just (DCTree v)) um options = gVerify{|*->*->*|} fx fy (Just v) um options
gVerify{|DynamicChoice|} fx fy	(Just (DCGrid v)) um options = gVerify{|*->*->*|} fx fy (Just v) um options
gVerify{|DynamicChoice|} fx fy	Nothing um options = alwaysValid um
	
instance Choice DynamicChoice
where
	selectOption newSel (DCCombo choice)	= DCCombo (selectOption newSel choice)
	selectOption newSel (DCRadio choice)	= DCRadio (selectOption newSel choice)	
	selectOption newSel (DCTree choice)		= DCTree (selectOption newSel choice)
	selectOption newSel (DCGrid choice)		= DCGrid (selectOption newSel choice)
	
	getSelection (DCCombo choice)			= getSelection choice
	getSelection (DCRadio choice)			= getSelection choice
	getSelection (DCTree choice)			= getSelection choice
	getSelection (DCGrid choice)			= getSelection choice
	
	getMbSelection (DCCombo choice)			= getMbSelection choice
	getMbSelection (DCRadio choice)			= getMbSelection choice
	getMbSelection (DCTree choice)			= getMbSelection choice
	getMbSelection (DCGrid choice)			= getMbSelection choice
	
	getMbSelectionView (DCCombo choice)		= getMbSelectionView choice
	getMbSelectionView (DCRadio choice)		= getMbSelectionView choice
	getMbSelectionView (DCTree choice)		= getMbSelectionView choice
	getMbSelectionView (DCGrid choice)		= getMbSelectionView choice

gDefault{|DynamicChoiceNoView|} fx path = DCRadioNoView (gDefault{|*->*|} fx path)

gVisualizeText{|DynamicChoiceNoView|} fo mode (DCRadioNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCComboNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCTreeNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCGridNoView val)	= gVisualizeText{|*->*|} fo mode val

gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 (Just (DCComboNoView val)) vst
	= gVisualizeEditor{|*->*|} f1 f2 f3 f4 (Just val) vst
gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 (Just (DCRadioNoView val)) vst
	= gVisualizeEditor{|*->*|} f1 f2 f3 f4 (Just val) vst
gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 (Just (DCTreeNoView val)) vst
	= gVisualizeEditor{|*->*|} f1 f2 f3 f4 (Just val) vst
gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 (Just (DCGridNoView val)) vst
	= gVisualizeEditor{|*->*|} f1 f2 f3 f4 (Just val) vst
gVisualizeEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 Nothing vst
	= (NormalEditor [],vst)

gUpdate{|DynamicChoiceNoView|} gUpdx gDefx target upd (DCComboNoView val,mask)	= appFst DCComboNoView (gUpdate{|*->*|} gUpdx gDefx target upd (val,mask))
gUpdate{|DynamicChoiceNoView|} gUpdx gDefx target upd (DCRadioNoView val,mask)	= appFst DCRadioNoView (gUpdate{|*->*|} gUpdx gDefx target upd (val,mask))
gUpdate{|DynamicChoiceNoView|} gUpdx gDefx target upd (DCTreeNoView val,mask)	= appFst DCTreeNoView (gUpdate{|*->*|} gUpdx gDefx target upd (val,mask))
gUpdate{|DynamicChoiceNoView|} gUpdx gDefx target upd (DCGridNoView val,mask)	= appFst DCGridNoView (gUpdate{|*->*|} gUpdx gDefx target upd (val,mask))

gVerify{|DynamicChoiceNoView|} fx (Just (DCComboNoView v)) um options = gVerify{|*->*|} fx (Just v) um options
gVerify{|DynamicChoiceNoView|} fx (Just (DCRadioNoView v)) um options = gVerify{|*->*|} fx (Just v) um options
gVerify{|DynamicChoiceNoView|} fx (Just (DCTreeNoView v)) um options = gVerify{|*->*|} fx (Just v) um options
gVerify{|DynamicChoiceNoView|} fx (Just (DCGridNoView v)) um options = gVerify{|*->*|} fx (Just v) um options
gVerify{|DynamicChoiceNoView|} fx Nothing um options = alwaysValid um
	
instance ChoiceNoView DynamicChoiceNoView
where
	selectOptionNoView newSel (DCComboNoView choice)	= DCComboNoView (selectOptionNoView newSel choice)
	selectOptionNoView newSel (DCRadioNoView choice)	= DCRadioNoView (selectOptionNoView newSel choice)	
	selectOptionNoView newSel (DCTreeNoView choice)		= DCTreeNoView (selectOptionNoView newSel choice)
	selectOptionNoView newSel (DCGridNoView choice) 	= DCGridNoView (selectOptionNoView newSel choice)
	
	getSelectionNoView (DCComboNoView choice)	= getSelectionNoView choice
	getSelectionNoView (DCRadioNoView choice)	= getSelectionNoView choice
	getSelectionNoView (DCTreeNoView choice)	= getSelectionNoView choice
	getSelectionNoView (DCGridNoView choice)	= getSelectionNoView choice

	getMbSelectionNoView (DCComboNoView choice)	= getMbSelectionNoView choice
	getMbSelectionNoView (DCRadioNoView choice)	= getMbSelectionNoView choice
	getMbSelectionNoView (DCTreeNoView choice)	= getMbSelectionNoView choice
	getMbSelectionNoView (DCGridNoView choice)	= getMbSelectionNoView choice

gDefault{|CheckMultiChoice|} _ _ _ = CheckMultiChoice [] []
gVisualizeText{|CheckMultiChoice|} fv _ _ val = gVisualizeText{|* -> *|} fv  AsLabel (getSelectionViews val)

gVisualizeEditor{|CheckMultiChoice|} _ gx _ _ _ _ _ _  val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
		| otherwise
			= ([(UICheckboxGroup defaultSizeOpts {UIChoiceOpts|taskId=toString taskId,editorId=name,value=evalue val,options=options val},addVerAttributes verRes newMap)],vst)

	vvalue (Just (CheckMultiChoice options sel))	= Just (join "," ([hd (gx AsLabel (fst (options !! i ))) \\ i <- sel]))
	vvalue _										= Nothing

	evalue (Just (CheckMultiChoice _ sel))			= sel
	evalue _										= []
	
	options (Just (CheckMultiChoice options _))		= [concat (gx AsLabel v) \\ (v,_) <- options]
	options	_										= []

gUpdate{|CheckMultiChoice|} _ _ _ _ target upd val = basicUpdate (\json (CheckMultiChoice opts sel) -> case fromJSON json of Just (i,v) = CheckMultiChoice opts (updateSel i v sel); _ = CheckMultiChoice opts sel) target upd val
where
	updateSel i True sel	= removeDup [i:sel]
	updateSel i False sel 	= removeMember i sel

gVerify{|CheckMultiChoice|} _ _	_ um options = simpleVerify "Choose a number of items" um options
	
instance MultiChoice CheckMultiChoice
where
	selectOptions newSels (CheckMultiChoice options _)			= CheckMultiChoice options (setListOptions options newSels)
	getSelections (CheckMultiChoice options sels)				= fmap snd (getListOptions options sels)
	getSelectionViews (CheckMultiChoice options sels)			= fmap fst (getListOptions options sels)

// Utility functions for Choice and MultiChoice instances
touch (TouchedWithState s)	= TouchedWithState s
touch (PartiallyTouched c)	= PartiallyTouched c
touch _						= Touched

expand idx (TouchedWithState s) = case fromJSON s of
	Just list	= TouchedWithState (toJSON (removeDup [idx:list]))
	_			= TouchedWithState (toJSON [idx])
expand idx _	= TouchedWithState (toJSON [idx])
 
collapse idx (TouchedWithState s) = case fromJSON s of
	Just list	= TouchedWithState (toJSON (removeMember idx list))
	_			= TouchedWithState s
collapse idx m = m

updateChoice select target upd val = basicUpdate (\json choice -> maybe choice (\i -> select i choice) (fromJSON json)) target upd val


setListOption :: ![(v,o)] !o -> (Maybe Int) | gEq{|*|} o
setListOption options newSel
	= case setListOptions options [newSel] of
		[idx:_]	= Just idx
		_		= Nothing

setListOptions :: ![(v,o)] ![o] -> [Int] | gEq{|*|} o
setListOptions options sels
	= [idx \\ (_,option) <- options & idx <- [0..] | gIsMember option sels]
where
	gIsMember x [hd:tl]	= hd===x || gIsMember x tl
	gIsMember x []		= False
	
getListOption :: ![a] !(Maybe Int) -> Maybe a
getListOption options mbSel = case getListOptions options (maybeToList mbSel) of
	[a] = Just a
	_	= Nothing

getListOptions :: ![a] ![Int] -> [a]
getListOptions options sels = [opt \\ opt <- options & idx <- [0..] | isMember idx sels]

getTreeOption :: !(Tree a) !(Maybe Int) -> Maybe a
getTreeOption tree mbSel = getListOption (treeToList tree) mbSel

setTreeOption :: !(Tree (v,o)) !o -> (Maybe Int) | gEq{|*|} o
setTreeOption tree newSel = setListOption (treeToList tree) newSel

setTreeOptionNoView :: !(Tree o) !o -> (Maybe Int) | gEq{|*|} o
setTreeOptionNoView tree newSel
	= setListOptionNoView (treeToList tree) newSel

setListOptionNoView :: ![o] !o -> (Maybe Int) | gEq{|*|} o
setListOptionNoView options newSel
	= case setListOptionL options newSel of
		[idx:_]	= Just idx
		_		= Nothing
  where
	setListOptionL :: ![o] o -> [Int] | gEq{|*|} o
	setListOptionL options sel
		= [idx \\ option <- options & idx <- [0..] | option===sel]

treeToList :: (Tree a) -> [a]
treeToList (Tree nodes) = (foldr addNode [] nodes)
where
	addNode (Leaf a) accu		= [a:accu]
	addNode (Node a nodes) accu	= [a:foldr addNode accu nodes] 

derive JSONEncode		ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive JSONEncode		ComboChoiceNoView,RadioChoiceNoView,TreeChoiceNoView,DynamicChoiceNoView,GridChoiceNoView
derive JSONDecode		ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive JSONDecode		ComboChoiceNoView,RadioChoiceNoView,TreeChoiceNoView,DynamicChoiceNoView,GridChoiceNoView
derive gEq				ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive gEq				ComboChoiceNoView,RadioChoiceNoView,TreeChoiceNoView,DynamicChoiceNoView,GridChoiceNoView
derive gHeaders			ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive gHeaders			ComboChoiceNoView,RadioChoiceNoView,TreeChoiceNoView,DynamicChoiceNoView,GridChoiceNoView
derive gGridRows		ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive gGridRows		ComboChoiceNoView,RadioChoiceNoView,TreeChoiceNoView,DynamicChoiceNoView,GridChoiceNoView

//* Visualization wrappers
gVisualizeText{|VisualizationHint|} fx mode val = case val of
	VHHidden x		= gVisualizeText{|* -> *|} fx mode (Hidden x)
	VHDisplay x		= gVisualizeText{|* -> *|} fx mode (Display x)
	VHEditable x	= gVisualizeText{|* -> *|} fx mode (Editable x)

gVisualizeEditor{|VisualizationHint|} fx gx hx ix val vst=:{VSt|currentPath}
	= case val of
		Just (VHHidden x)	= gVisualizeEditor{|* -> *|} fx gx hx ix (Just (Hidden x)) vst
		Just (VHDisplay x)	= gVisualizeEditor{|* -> *|} fx gx hx ix (Just (Display x)) vst
		Just (VHEditable x)	= gVisualizeEditor{|* -> *|} fx gx hx ix (Just (Editable x)) vst
		Nothing				= fx Nothing vst

gUpdate{|VisualizationHint|} 	gUpdx gDefx target upd val=:(VHEditable s,mask)	= wrapperUpdate gUpdx fromVisualizationHint VHEditable target upd val
gUpdate{|VisualizationHint|} 	gUpdx gDefx target upd val=:(VHDisplay s,mask)	= wrapperUpdate gUpdx fromVisualizationHint VHDisplay target upd val
gUpdate{|VisualizationHint|} 	gUpdx gDefx target upd val=:(VHHidden s,mask)	= wrapperUpdate gUpdx fromVisualizationHint VHHidden target upd val

gVerify{|VisualizationHint|}	fx v um options = case v of
	Just (VHEditable e)	= verifyEditable fx (Just e) um options
	Just (VHDisplay d)	= verifyDisplay fx (Just d) um options
	Just (VHHidden _)	= verifyHidden um
	_					= ([],um)
			
fromVisualizationHint :: !(VisualizationHint .a) -> .a
fromVisualizationHint (VHEditable a) = a
fromVisualizationHint (VHDisplay a) = a
fromVisualizationHint (VHHidden a) = a

toVisualizationHint :: !.a -> (VisualizationHint .a)
toVisualizationHint a = (VHEditable a)

gVisualizeText{|Hidden|} _ _ _ = []

gVisualizeEditor{|Hidden|} fx _ _ _ val vst=:{VSt | currentPath, verifyMask=[_:vm]}
	= (HiddenEditor,{VSt | vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gUpdate{|Hidden|} gUpdx gDefx target upd val = wrapperUpdate gUpdx fromHidden Hidden target upd val

gVerify{|Hidden|} _ _ um options = verifyHidden um

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)

gVisualizeText{|Display|} fx mode (Display val)	= fx mode val

gVisualizeEditor{|Display|} fx _ _ _ val vst=:{VSt|currentPath,disabled}
	# (def,vst) = fx (fmap fromDisplay val) {VSt | vst &  disabled = True}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, disabled = disabled})

gUpdate{|Display|} gUpdx gDefx target upd val = wrapperUpdate gUpdx fromDisplay Display target upd val

gVerify{|Display|} fx d um options = verifyDisplay fx (fmap fromDisplay d) um options

fromDisplay :: !(Display .a) -> .a
fromDisplay (Display a) = a

toDisplay :: !.a -> (Display .a)
toDisplay a = (Display a)

gVisualizeText{|Editable|} fx mode(Editable val) = fx mode val

gVisualizeEditor{|Editable|} fx _ _ _ val vst=:{VSt|currentPath, disabled}
	# (def,vst) = fx (fmap fromEditable val) {VSt | vst & disabled = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, disabled = disabled})

gUpdate{|Editable|} gUpdx gDefx target upd val = wrapperUpdate gUpdx fromEditable Editable target upd val

gVerify{|Editable|} fx e um options = verifyEditable fx (fmap fromEditable e) um options

fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

//Utility for gUpdate 
wrapperUpdate fx get set target upd (val,mask)
	# (w,mask) = fx target upd (get val,mask)
	= (set w,mask)
		
//Utility for gVerify	
verifyEditable fx e um options = fx e um {VerifyOptions|options & disabled = False}
verifyDisplay fx d um options = fx d um {VerifyOptions|options & disabled = True}
verifyHidden um = ([VMValid Nothing []],um)

derive JSONEncode		Hidden, Display, Editable, VisualizationHint
derive JSONDecode		Hidden, Display, Editable, VisualizationHint
derive gDefault			Hidden, Display, Editable, VisualizationHint
derive gEq				Hidden, Display, Editable, VisualizationHint
derive gHeaders			Hidden, Display, Editable, VisualizationHint
derive gGridRows		Hidden, Display, Editable, VisualizationHint

//* Framework types

instance Functor TaskValue
where
	fmap f (NoValue)		= NoValue
	fmap f (Value v s)		= Value (f v) s

//Task id

instance toString TaskId
where
	toString (TaskId topNo taskNo)		= join "-" [toString topNo,toString taskNo]

instance fromString TaskId
where
	fromString s = case split "-" s of
		[topNo,taskNo]	= TaskId (toInt topNo) (toInt taskNo)
		_				= TaskId 0 0

instance == TaskId
where
	(==) (TaskId a0 b0) (TaskId a1 b1) = a0 == a1 && b0 == b1

instance < TaskId
where
	(<) (TaskId a0 b0) (TaskId a1 b1) = if (a0 == a1) (b0 < b1) (a0 < a1)

instance toString TaskPriority
where
	toString LowPriority	= "Low"
	toString NormalPriority	= "Normal"
	toString HighPriority	= "High"

instance toString (TaskListId s)
where
	toString (TopLevelTaskList)					= "tasklist-top"
	toString (ParallelTaskList (TaskId t0 t1))	= "tasklist-parallel-" +++ toString t0 +++ "-" +++ toString t1

derive JSONEncode InteractionMask
derive JSONDecode InteractionMask

//Utility functions
dp2s :: !DataPath -> String
dp2s path = "v" + join "-" (map toString path)

s2dp :: !String -> DataPath
s2dp str 
	| textSize str < 2	= []
						= map toInt (split "-" (subString 1 (textSize str) str))

stepDataPath :: !DataPath -> DataPath
stepDataPath []		= []
stepDataPath [x]	= [inc x]
stepDataPath [x:xs]	= [x:stepDataPath xs]

shiftDataPath :: !DataPath -> DataPath
shiftDataPath path = path ++ [0]

gVisualizeText{|User|} _ val = [toString val]

gUpdate{|User|} target upd val = basicUpdateSimple target upd val

gVerify{|User|} _ um options = simpleVerify "Select a username" um options 

instance toString User
where
	toString (AnonymousUser _)					= "Anonymous"
	toString (AuthenticatedUser uid _ title)	= maybe uid (\t -> t +++ " <" +++ uid +++ ">") title

instance == User
where
	(==) (AnonymousUser a) (AnonymousUser b)					= a == b
	(==) (AuthenticatedUser a _ _) (AuthenticatedUser b _ _)	= a == b
	(==) _ _													= False

instance < User
where
	(<) (AnonymousUser a) (AnonymousUser b)					= a < b
	(<) (AuthenticatedUser a _ _) (AuthenticatedUser b _ _)	= a < b
	(<)	_ _													= False

instance toUserConstraint UserConstraint
where
	toUserConstraint r = r

instance toUserConstraint User
where
	toUserConstraint (AnonymousUser _)				= AnyUser
	toUserConstraint (AuthenticatedUser uid _ _)	= UserWithId uid

instance toUserConstraint UserId
where
	toUserConstraint userId = UserWithId userId

instance == Action
where
	(==) :: !Action !Action -> Bool
	(==) (Action name0 _) (Action name1 _) = name0 == name1
	(==) a b = a === b

actionName :: !Action -> ActionName
actionName (Action name _)	= name

actionIcon :: !Action -> Maybe String
actionIcon (Action _ options) = case [icon \\ ActionIcon icon <- options] of
	[icon]	= Just ("icon-" + icon)
	_		= Nothing

actionWeight :: !Action -> Int
actionWeight (Action _ options) = case [weight \\ ActionWeight weight <- options] of
	[weight:_]	= weight
	_			= 0 

derive JSONEncode		TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey
derive JSONDecode		TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey
derive gDefault			TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey
derive gEq				TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey
derive gVisualizeText	TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action, ActionOption, Hotkey
derive gVisualizeEditor	TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey
derive gHeaders			TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey
derive gGridRows		TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey
derive gUpdate			TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action, ActionOption, Hotkey
derive gVerify			TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action, ActionOption, Hotkey

derive class iTask TaskId, Config, ProcessStatus
	
instance toString Icon
where
	toString (Icon icon) = icon
	toString (IconView)	= "view"
	toString (IconEdit) = "edit"
	
instance descr Void
where
	toPrompt _ = {UIControlSequence|attributes = newMap, controls =[], direction = Vertical}

instance descr String
where
	toPrompt prompt = {UIControlSequence|attributes = newMap, controls = [(stringDisplay prompt,newMap)], direction = Vertical}
	
instance descr (!String,!String) 
where
	toPrompt (title,prompt) = {UIControlSequence|attributes = put TITLE_ATTRIBUTE title newMap, controls = [(stringDisplay prompt,newMap)], direction = Vertical}

instance descr (!Icon,!String,!String)
where
	toPrompt (icon,title,prompt) = {UIControlSequence|attributes = fromList [(TITLE_ATTRIBUTE,title),(ICON_ATTRIBUTE, toString icon)]
								   ,controls = [(stringDisplay prompt,newMap)]
								   ,direction = Vertical}
//instance descr (!Icon,!Title)
//where toPrompt (icon,title)	= (fromList [(TITLE_ATTRIBUTE,toString title),(ICON_ATTRIBUTE, toString icon)],[],Vertical)

instance descr Title
where
	toPrompt (Title title) = {UIControlSequence|attributes = put TITLE_ATTRIBUTE title newMap, controls = [], direction = Vertical}
	
instance descr Hint
where
	toPrompt (Hint hint) = {UIControlSequence|attributes = put HINT_ATTRIBUTE hint newMap, controls = [], direction = Vertical}
	
instance descr Icon
where
	toPrompt icon = {UIControlSequence|attributes = put ICON_ATTRIBUTE (toString icon) newMap, controls = [], direction = Vertical}

instance descr Attribute
where
	toPrompt (Attribute k v) = {UIControlSequence| attributes = put k v newMap, controls = [], direction = Vertical}
	
instance descr Att
where
	toPrompt (Att a) = toPrompt a
	
instance descr [d] | descr d
where
	toPrompt list = foldl merge {UIControlSequence| attributes = newMap, controls = [], direction = Vertical} (map toPrompt list)
	where
		merge p1 p2  = {UIControlSequence
					   |attributes = mergeAttributes p1.UIControlSequence.attributes p2.UIControlSequence.attributes
					   ,controls = p1.UIControlSequence.controls ++ p2.UIControlSequence.controls
					   ,direction = p1.UIControlSequence.direction
					   }

// Generic instances for common library types
derive JSONEncode		Map, Either, HtmlTag, HtmlAttr
derive JSONDecode		Map, Either, HtmlTag, HtmlAttr
derive gEq				Map, Either, HtmlTag, HtmlAttr, Void, Timestamp, Maybe, JSONNode

JSONEncode{|Timestamp|} (Timestamp t)	= [JSONInt t]
JSONDecode{|Timestamp|} [JSONInt t:c]	= (Just (Timestamp t), c)
JSONDecode{|Timestamp|} c				= (Nothing, c)

JSONEncode{|Void|} Void = [JSONNull]
JSONDecode{|Void|} [JSONNull:c]		= (Just Void, c)
JSONDecode{|Void|} [JSONObject []:c]= (Just Void, c)
JSONDecode{|Void|} c				= (Nothing, c)

gEq{|(->)|} _ _ fa fb		= copy_to_string fa == copy_to_string fb // HACK: Compare string representations of graphs functions are never equal
gEq{|Dynamic|} _ _			= False	// dynamics are never equal
