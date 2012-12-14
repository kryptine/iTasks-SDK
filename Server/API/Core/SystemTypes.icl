implementation module SystemTypes
from StdFunc import until

import StdInt, StdBool, StdClass, StdArray, StdTuple, StdMisc, StdList, StdFunc, StdOrdList
import List_NG, JSON_NG, HTML, Text, Util, Map, Base64, Tuple, dynamic_string
import GenVisualize, GenUpdate

from Time 		import :: Timestamp(..)
from Task		import :: TaskValue

from UIDefinition import :: UIDef(..), :: UIControlSequence, :: UIActionSet, :: UIControlGroup, :: UIActions, :: UIControls, :: UITitle, :: UIDirection(..), :: UIAnnotatedControls, :: UIAbstractContainer, :: UIFinal, :: UIAction, :: UIControl, stringDisplay
from LayoutCombinators import mergeAttributes, setMargins

//* EmailAddress
derive JSONEncode		EmailAddress
derive JSONDecode		EmailAddress
derive gEq				EmailAddress
derive gVisualizeText	EmailAddress
derive gVisualizeEditor	EmailAddress
derive gHeaders			EmailAddress
derive gGridRows		EmailAddress
derive gUpdate			EmailAddress
derive gVerify			EmailAddress

//* URL
gVisualizeText{|URL|}	_ val				= [toString val]

gVisualizeEditor{|URL|} val vst = visualizeCustom viz vst
where
	viz name touched verRes vst=:{VSt|taskId,disabled}
		| disabled
			= ([(UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap (\(URL url) -> ATag [HrefAttr url] [Text url]) val},newMap)], vst)
		| otherwise
			# val = checkMask touched val
			# ui = UIEditString defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=fmap toString val}
			= ([(ui,addVerAttributes verRes newMap)],vst)

gUpdate{|URL|} mode ust = basicUpdate mode (\json url -> maybe url (\s -> URL s) (fromJSON json))  (URL "") ust

gVerify{|URL|} _ vst = simpleVerify "Enter a uniform resource locator (URL)" vst

derive JSONEncode		URL
derive JSONDecode		URL
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

gUpdate{|Note|} mode ust = basicUpdateSimple mode (Note "") ust

gVerify{|Note|} _ vst = simpleVerify "Enter a long text" vst

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

gUpdate{|CleanCode|} mode ust = basicUpdate mode codeUpd (CleanCode "") ust
where
	codeUpd (JSONString s) _	= CleanCode s
	codeUpd _ old				= old			

gVerify{|CleanCode|} _ vst = simpleVerify "Enter a piece of Clean code" vst

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

gUpdate{|EUR|} mode ust = basicUpdateSimple mode (EUR 0) ust

gVerify{|EUR|} _ vst = simpleVerify "Enter an amount in EUR" vst

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

gUpdate{|USD|} mode ust = basicUpdateSimple mode (USD 0) ust

gVerify{|USD|} _ vst = simpleVerify "Enter an amount in USD" vst

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
		# val = checkMask touched val
		| disabled	= ([(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
		| otherwise	= ([(UIEditDate defaultSizeOpts {UIEditOpts|taskId=toString taskId,editorId=name,value=val},addVerAttributes verRes newMap)],vst)

gUpdate{|Date|} UDCreate ust = basicCreate {day = 1, mon = 1, year = 1970} ust
gUpdate{|Date|} (UDSearch d) ust = basicSearch d (\json old -> fromMaybe old (fromJSON json)) ust

gVerify{|Date|} _ vst = simpleVerify "Enter a date" vst

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

gUpdate{|Time|} UDCreate ust = basicCreate {hour = 0, min = 0, sec = 0} ust
gUpdate{|Time|} (UDSearch t) ust = basicSearch t (\json old -> fromMaybe old (fromJSON json)) ust

gVerify{|Time|} _ vst = simpleVerify "Enter a time of day" vst

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

gUpdate {|Document|} UDCreate ust = basicCreate {Document|documentId = "", contentUrl = "", name="", mime="", size = 0} ust
gUpdate {|Document|} (UDSearch s) ust=:{searchPath, currentPath, update, oldMask, newMask}
	# (cm,om)		= popMask oldMask
	# ust			= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= case fromJSON update of
			Nothing 	// Reset
				= ({Document|documentId = "", contentUrl = "", name="", mime="", size = 0},{ust & newMask = appendToMask newMask Blanked})
			Just doc 	//Update
				# ust					= {ust & newMask = appendToMask newMask (PartiallyTouched [])}
				= (doc,ust)
	| otherwise 
		= (s, {ust & newMask = appendToMask newMask cm})

gVerify{|Document|} _ vst = simpleVerify "Upload a document" vst

derive JSONEncode		Document
derive JSONDecode		Document
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

gUpdate{|Username|} mode ust = basicUpdateSimple mode (Username "") ust

gVerify{|Username|} _ vst = simpleVerify "Enter a username" vst

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

gUpdate{|Password|} mode ust = basicUpdateSimple mode (Password "") ust

gVerify{|Password|} _ vst = simpleVerify "Enter a password" vst

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
	
//Helper types for GoogleMap gUpdate instance
:: MVCUpdate = 
	{ center			:: !(Real,Real)
	, zoom				:: !Int
	, type				:: !GoogleMapType
	}	
	
:: ClickUpdate = 
	{ event				:: !ClickEvent
	, source			:: !ClickSource
	, point				:: !(Real,Real)
	}

:: ClickEvent	= LEFTCLICK | RIGHTCLICK | DBLCLICK
:: ClickSource  = MAP | MARKER (Real,Real)

:: MarkerDragUpdate = 
	{ index				:: !Int
	, point				:: !(Real,Real)
	}
derive JSONDecode MVCUpdate, ClickUpdate, ClickEvent, ClickSource, MarkerDragUpdate

gUpdate{|GoogleMap|} mode ust = basicUpdate mode parseUpdate defaultMap ust
where
	parseUpdate json orig
		# mbMVC		= fromJSON json
		| isJust mbMVC
			# {MVCUpdate|center=(lat,lng),zoom,type} = fromJust mbMVC
			= {GoogleMap | orig & perspective = {GoogleMapPerspective|orig.perspective & center = {lat=lat,lng=lng}, zoom = zoom, type = type}}
		# mbClick 	= fromJSON json
		| isJust mbClick
			# click = fromJust mbClick
			# marker = {GoogleMapMarker | position = {lat=fst click.ClickUpdate.point,lng=snd click.ClickUpdate.point}, title = Nothing, icon = Nothing, infoWindow = Nothing, draggable = True, selected = False} 
			= {GoogleMap | orig & markers = orig.GoogleMap.markers ++ [marker]}
		# mbMarkerDrag = fromJSON json
		| isJust mbMarkerDrag
			# {MarkerDragUpdate|index,point=(lat,lng)}	= fromJust mbMarkerDrag
			= {GoogleMap | orig & markers = [if (i == index) {GoogleMapMarker|m & position = {lat=lat,lng=lng}} m \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		
		| otherwise = orig

	defaultMap =
		{ GoogleMap
		| settings=settings
		, perspective=perspective
		, markers=[]
		}
	perspective =
		{ GoogleMapPerspective
		| type				= ROADMAP
		, center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
		, zoom				= 10
		}	
	settings =
		{ GoogleMapSettings
		| mapTypeControl	= True
		, panControl		= True
		, streetViewControl	= True
		, zoomControl		= True
		, scaleControl		= True
		, scrollwheel		= True
		, draggable			= True
		}

gVerify{|GoogleMap|} _ vst = alwaysValid vst

derive JSONEncode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive JSONDecode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gEq				GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gVisualizeText	GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gVisualizeEditor GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gHeaders			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gGridRows		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gUpdate			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType
derive gVerify			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType

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

gUpdate{|Scale|} mode ust
	= basicUpdate mode (\json i -> maybe i (\cur -> {Scale|i & cur = cur}) (fromJSON json)) {Scale|min=1,cur=3,max=5} ust

gVerify{|Scale|} _ vst = alwaysValid vst

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

gUpdate{|Progress|}	mode ust
	= noUpdate mode {Progress|progress=ProgressUndetermined, description = ""} ust

gVerify{|Progress|} _ vst = alwaysValid vst

gHeaders{|Progress|} _		= [""]
gGridRows{|Progress|} _ _	= Nothing

gVisualizeText{|ProgressAmount|} _ ProgressUndetermined		= ["Undetermined"]
gVisualizeText{|ProgressAmount|} _ (ProgressRatio r)		= [toString (entier (100.0 * r)) + "%"]

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

gUpdate{|HtmlInclude|} mode ust = noUpdate mode (HtmlInclude "") ust

gVerify{|HtmlInclude|} _ vst = alwaysValid vst

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

gUpdate{|FormButton|} mode ust
	= basicUpdate mode (\st b -> {FormButton|b & state = st}) {FormButton | label = "Form Button", icon="", state = NotPressed} ust

gVerify{|FormButton|} _ vst = alwaysValid vst

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

gUpdate{|Table|} mode ust
	= basicUpdate mode (\json (Table headers cells _) -> case fromJSON json of Just i = Table headers cells (Just i); _ = Table headers cells Nothing) (Table [] [] Nothing) ust

gVerify{|Table|} _ vst = alwaysValid vst

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

gUpdate{|ComboChoice|} _ _		mode ust = updateChoice mode (\idx (ComboChoice options _) -> ComboChoice options idx) (ComboChoice [] Nothing) ust	

gVerify{|ComboChoice|} _ _		v vst = customVerify (Just "Choose one item") (\(ComboChoice _ s) -> isJust s) (const "You must choose one item") v vst

instance Choice ComboChoice
where
	selectOption newSel (ComboChoice options _)					= ComboChoice options (setListOption options newSel)
	getSelection combo											= fromJust (getMbSelection combo)
	getMbSelection (ComboChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (ComboChoice options mbSel)				= fmap fst (getListOption options mbSel)

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

gUpdate{|ComboChoiceNoView|} _	mode ust = updateChoice mode (\idx (ComboChoiceNoView options _) -> ComboChoiceNoView options idx) (ComboChoiceNoView [] Nothing) ust

gVerify{|ComboChoiceNoView|} _	v vst = customVerify (Just "Choose one item") (\(ComboChoiceNoView _ s) -> isJust s) (const "You must choose one item") v vst

instance ChoiceNoView ComboChoiceNoView
where
	selectOptionNoView newSel (ComboChoiceNoView options _)		= ComboChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView combo									= fromJust (getMbSelectionNoView combo)
	getMbSelectionNoView (ComboChoiceNoView options mbSel)		= getListOption options mbSel

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

gUpdate{|RadioChoice|} _ _ mode ust
	= updateChoice mode (\idx (RadioChoice options _) -> RadioChoice options idx) (RadioChoice [] Nothing) ust

gVerify{|RadioChoice|} _ _		_ vst = simpleVerify "Choose one item" vst

instance Choice RadioChoice
where
	selectOption newSel (RadioChoice options _)					= RadioChoice options (setListOption options newSel)
	getSelection radios											= fromJust (getMbSelection radios)
	getMbSelection (RadioChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (RadioChoice options mbSel)				= fmap fst (getListOption options mbSel)

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

gUpdate{|RadioChoiceNoView|} _	mode ust
	= updateChoice mode (\idx (RadioChoiceNoView options _) -> RadioChoiceNoView options idx) (RadioChoiceNoView [] Nothing) ust

gVerify{|RadioChoiceNoView|} _	_ vst = simpleVerify "Choose one item" vst

instance ChoiceNoView RadioChoiceNoView
where
	selectOptionNoView newSel (RadioChoiceNoView options _)		= RadioChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView radios									= fromJust (getMbSelectionNoView radios)
	getMbSelectionNoView (RadioChoiceNoView options mbSel)		= getListOption options mbSel

gVisualizeText{|TreeChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))

gVisualizeEditor{|TreeChoice|} _ gx _ _ _ _ _ _ val vst=:{VSt|taskId,currentPath,disabled,verifyMask}
	# (cmv,vm)	= popMask verifyMask
	# vst		= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
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

gUpdate{|TreeChoice|} _ _		UDCreate ust	= basicCreate (TreeChoice (Tree []) Nothing) ust
gUpdate{|TreeChoice|} _ _		(UDSearch (TreeChoice options sel)) ust=:{searchPath,currentPath,update,oldMask,newMask}
	# (cm, om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= case fromJSON update of
			Just ("sel",idx,val)
				= (TreeChoice options (if val (Just idx) Nothing), {ust & newMask = appendToMask newMask (touch cm)})
			Just ("exp",idx,val)
				= (TreeChoice options sel, {ust & newMask = appendToMask newMask (if val (expand idx cm) (collapse idx cm))})
			_
				= ((TreeChoice options sel), {ust & newMask = appendToMask newMask cm})
	| otherwise
		= ((TreeChoice options sel), {ust & newMask = appendToMask newMask cm})

gVerify{|TreeChoice|} _ _		_ vst = simpleVerify "Choose an element of the tree" vst

instance Choice TreeChoice
where
	selectOption newSel (TreeChoice options _)					= TreeChoice options (setTreeOption options newSel)
	getSelection tree											= fromJust (getMbSelection tree)
	getMbSelection (TreeChoice options mbSel)					= fmap snd (getTreeOption options mbSel)
	getMbSelectionView (TreeChoice options mbSel)				= fmap fst (getTreeOption options mbSel)

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

gUpdate{|TreeChoiceNoView|} _	mode ust = updateChoice mode update (TreeChoiceNoView (Tree []) Nothing) ust
where
	update ("sel",idx,val)		(TreeChoiceNoView options _) 		= TreeChoiceNoView options (if val (Just idx) Nothing)
	update ("exp",idx,val)		(TreeChoiceNoView options sel)		= TreeChoiceNoView options sel
	update _					treechoice							= treechoice

gVerify{|TreeChoiceNoView|} _	_ vst = simpleVerify "Choose an element of the tree" vst
	
instance ChoiceNoView TreeChoiceNoView
where
	selectOptionNoView newSel (TreeChoiceNoView options _)		= TreeChoiceNoView options (setTreeOptionNoView options newSel)
	getSelectionNoView tree										= fromJust (getMbSelectionNoView tree)
	getMbSelectionNoView (TreeChoiceNoView options mbSel)		= getTreeOption options mbSel

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

gUpdate{|GridChoice|} _ _ mode ust
	= updateChoice mode (\idx (GridChoice options _) -> GridChoice options (Just idx)) (GridChoice [] Nothing) ust

gVerify{|GridChoice|} _ _ _ vst = alwaysValid vst

instance Choice GridChoice
where
	selectOption newSel (GridChoice options _)					= GridChoice options (setListOption options newSel)
	getSelection grid											= fromJust (getMbSelection grid)
	getMbSelection (GridChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (GridChoice options mbSel)				= fmap fst (getListOption options mbSel)

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

gUpdate{|GridChoiceNoView|} _ mode ust
	= updateChoice mode (\idx (GridChoiceNoView options _) -> GridChoiceNoView options (Just idx)) (GridChoiceNoView [] Nothing) ust

gVerify{|GridChoiceNoView|} _	_ vst = alwaysValid vst
	
instance ChoiceNoView GridChoiceNoView
where
	selectOptionNoView newSel (GridChoiceNoView options _)		= GridChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView grid										= fromJust (getMbSelectionNoView grid)
	getMbSelectionNoView (GridChoiceNoView options mbSel)		= getListOption options mbSel

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

gUpdate{|DynamicChoice|} fx fy	(UDSearch (DCCombo val))	ust = appFst DCCombo (gUpdate{|*->*->*|} fx fy (UDSearch val) ust)
gUpdate{|DynamicChoice|} fx fy	(UDSearch (DCRadio val))	ust = appFst DCRadio (gUpdate{|*->*->*|} fx fy (UDSearch val) ust)
gUpdate{|DynamicChoice|} fx fy	(UDSearch (DCTree val))		ust = appFst DCTree (gUpdate{|*->*->*|} fx fy (UDSearch val) ust)
gUpdate{|DynamicChoice|} fx fy	(UDSearch (DCGrid val))		ust = appFst DCGrid (gUpdate{|*->*->*|} fx fy (UDSearch val) ust)
gUpdate{|DynamicChoice|} fx fy	UDCreate 					ust = appFst DCRadio (gUpdate{|*->*->*|} fx fy UDCreate ust)

gVerify{|DynamicChoice|} fx fy	(Just (DCCombo v)) vst = gVerify{|*->*->*|} fx fy (Just v) vst
gVerify{|DynamicChoice|} fx fy	(Just (DCRadio v)) vst = gVerify{|*->*->*|} fx fy (Just v) vst
gVerify{|DynamicChoice|} fx fy	(Just (DCTree v)) vst = gVerify{|*->*->*|} fx fy (Just v) vst
gVerify{|DynamicChoice|} fx fy	(Just (DCGrid v)) vst = gVerify{|*->*->*|} fx fy (Just v) vst
gVerify{|DynamicChoice|} fx fy	Nothing vst = alwaysValid vst
	
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

gUpdate{|DynamicChoiceNoView|} fx (UDSearch (DCComboNoView val))	ust = appFst DCComboNoView (gUpdate{|*->*|} fx (UDSearch val) ust)
gUpdate{|DynamicChoiceNoView|} fx (UDSearch (DCRadioNoView val))	ust = appFst DCRadioNoView (gUpdate{|*->*|} fx (UDSearch val) ust)
gUpdate{|DynamicChoiceNoView|} fx (UDSearch (DCTreeNoView val)) 	ust = appFst DCTreeNoView (gUpdate{|*->*|} fx (UDSearch val) ust)
gUpdate{|DynamicChoiceNoView|} fx (UDSearch (DCGridNoView val)) 	ust = appFst DCGridNoView (gUpdate{|*->*|} fx (UDSearch val) ust)
gUpdate{|DynamicChoiceNoView|} fx UDCreate	 						ust = appFst DCRadioNoView (gUpdate{|*->*|} fx UDCreate ust)

gVerify{|DynamicChoiceNoView|} fx (Just (DCComboNoView v)) vst = gVerify{|*->*|} fx (Just v) vst
gVerify{|DynamicChoiceNoView|} fx (Just (DCRadioNoView v)) vst = gVerify{|*->*|} fx (Just v) vst
gVerify{|DynamicChoiceNoView|} fx (Just (DCTreeNoView v)) vst = gVerify{|*->*|} fx (Just v) vst
gVerify{|DynamicChoiceNoView|} fx (Just (DCGridNoView v)) vst = gVerify{|*->*|} fx (Just v) vst
gVerify{|DynamicChoiceNoView|} fx Nothing vst = alwaysValid vst
	
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

gUpdate{|CheckMultiChoice|} _ _	mode ust = basicUpdate mode (\json (CheckMultiChoice opts sel)	-> case fromJSON json of Just (i,v) = CheckMultiChoice opts (updateSel i v sel); _ = CheckMultiChoice opts sel)	(CheckMultiChoice [] [])										ust
where
	updateSel i True sel	= removeDup [i:sel]
	updateSel i False sel 	= removeMember i sel

gVerify{|CheckMultiChoice|} _ _	_ vst = simpleVerify "Choose a number of items" vst
	
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

updateChoice mode select empty ust = basicUpdate mode (\json choice -> maybe choice (\i -> select i choice) (fromJSON json)) empty ust


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

gUpdate{|VisualizationHint|} 	fx UDCreate									ust = wrapperUpdate fx UDCreate undef VHEditable ust 
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHEditable s))			ust = wrapperUpdate fx mode fromVisualizationHint VHEditable ust
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHDisplay s))			ust = wrapperUpdate fx mode fromVisualizationHint VHDisplay ust
gUpdate{|VisualizationHint|} 	fx mode=:(UDSearch (VHHidden s))			ust = wrapperUpdate fx mode fromVisualizationHint VHHidden ust

gVerify{|VisualizationHint|}	fx v vst	= case v of
	Just (VHEditable e)	= verifyEditable fx (Just e) vst
	Just (VHDisplay d)	= verifyDisplay fx (Just d) vst
	Just (VHHidden _)	= verifyHidden vst
	_					= vst
			
fromVisualizationHint :: !(VisualizationHint .a) -> .a
fromVisualizationHint (VHEditable a) = a
fromVisualizationHint (VHDisplay a) = a
fromVisualizationHint (VHHidden a) = a

toVisualizationHint :: !.a -> (VisualizationHint .a)
toVisualizationHint a = (VHEditable a)

gVisualizeText{|Hidden|} _ _ _ = []

gVisualizeEditor{|Hidden|} fx _ _ _ val vst=:{VSt | currentPath, verifyMask}
	# (_,vm) = popMask verifyMask	
	= (HiddenEditor,{VSt | vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gUpdate{|Hidden|} fx mode ust = wrapperUpdate fx mode fromHidden Hidden ust

gVerify{|Hidden|} _ _ vst = verifyHidden vst

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)

gVisualizeText{|Display|} fx mode (Display val)	= fx mode val

gVisualizeEditor{|Display|} fx _ _ _ val vst=:{VSt|currentPath,disabled}
	# (def,vst) = fx (fmap fromDisplay val) {VSt | vst &  disabled = True}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, disabled = disabled})

gUpdate{|Display|} fx mode ust = wrapperUpdate fx mode fromDisplay Display ust

gVerify{|Display|} fx d vst = verifyDisplay fx (fmap fromDisplay d) vst

fromDisplay :: !(Display .a) -> .a
fromDisplay (Display a) = a

toDisplay :: !.a -> (Display .a)
toDisplay a = (Display a)

gVisualizeText{|Editable|} fx mode(Editable val) = fx mode val

gVisualizeEditor{|Editable|} fx _ _ _ val vst=:{VSt|currentPath, disabled}
	# (def,vst) = fx (fmap fromEditable val) {VSt | vst & disabled = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, disabled = disabled})

gUpdate{|Editable|} fx mode ust = wrapperUpdate fx mode fromEditable Editable ust

gVerify{|Editable|} fx e vst = verifyEditable fx (fmap fromEditable e) vst

fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

//Utility for gUpdate 
wrapperUpdate fx mode get cons ust=:{USt|currentPath} = case mode of
	UDCreate
		= appFst cons (fx UDCreate ust)
	UDSearch w
		# (w,ust) = fx (UDSearch (get w)) ust
		= (cons w,{USt|ust & currentPath = stepDataPath currentPath})
		
//Utility for gVerify	
verifyEditable	fx e vst=:{staticDisplay}		= (\vst -> {vst & staticDisplay = staticDisplay}) (fx e {vst & staticDisplay = False})
verifyDisplay	fx d vst=:{staticDisplay}		= (\vst -> {vst & staticDisplay = staticDisplay}) (fx d {vst & staticDisplay = True})
verifyHidden	vst=:{verifyMask,updateMask}	= {vst & verifyMask = appendToMask verifyMask (VMValid Nothing []), updateMask = snd (popMask updateMask)}

derive JSONEncode		Hidden, Display, Editable, VisualizationHint
derive JSONDecode		Hidden, Display, Editable, VisualizationHint
derive gEq				Hidden, Display, Editable, VisualizationHint
derive gHeaders			Hidden, Display, Editable, VisualizationHint
derive gGridRows		Hidden, Display, Editable, VisualizationHint

//* Framework types

instance Functor TaskValue
where
	fmap f (NoValue)		= NoValue
	fmap f (Value v s)		= Value (f v) s

instance toString Stability
where
	toString Unstable	= "Unstable"
	toString Stable	= "Stable"
	
instance == Stability
where
	(==) Unstable	Unstable	= True
	(==) Stable		Stable		= True
	(==) _			_			= False
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

gVisualizeText{|User|} _ val = [toString val]

gUpdate{|User|} mode ust = basicUpdateSimple mode (AnonymousUser "") ust

gVerify{|User|} _ vst = simpleVerify "Select a username" vst 

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
	(==) (Action name0) (Action name1) = name0 == name1
	(==) a b = a === b

actionName :: !Action -> ActionName
actionName (Action name)		= name
actionName ActionOk				= "Ok"
actionName ActionCancel			= "Cancel"
actionName ActionYes			= "Yes"
actionName ActionNo				= "No"
actionName ActionNext			= "Next"
actionName ActionPrevious		= "Previous"
actionName ActionFinish			= "Finish"
actionName ActionContinue		= "Continue"
actionName ActionOpen			= "File/Open"
actionName ActionSave			= "File/Save"
actionName ActionSaveAs			= "File/Save as"
actionName ActionQuit			= "File/Quit"
actionName ActionHelp			= "Help/Help"
actionName ActionAbout			= "Help/About"
actionName ActionFind			= "Edit/Find"
actionName ActionNew			= "New"
actionName ActionEdit			= "Edit"
actionName ActionDelete			= "Delete"
actionName ActionRefresh		= "Refresh"
actionName ActionClose			= "Close"
	
actionIcon :: !Action -> String
actionIcon action = "icon-" +++ (replaceSubString " " "-" (toLowerCase (last (split "/" (actionName action)))))

derive JSONEncode		TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action
derive JSONDecode		TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action
derive gEq				TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action
derive gVisualizeText	TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action
derive gVisualizeEditor	TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action
derive gHeaders			TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action
derive gGridRows		TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action
derive gUpdate			TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action
derive gVerify			TaskValue, Stability, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action

derive class iTask TaskId, Config, ProcessStatus
	
instance toString Icon
where
	toString (Icon icon) = icon
	toString (IconView)	= "view"
	toString (IconEdit) = "edit"
	
instance descr Void
where toPrompt _ = (newMap,[],Vertical)

instance descr String
where toPrompt prompt	= (newMap,[(stringDisplay prompt,newMap)],Vertical)
	
instance descr (!String,!String) 
where toPrompt (title,prompt)	= (fromList [(TITLE_ATTRIBUTE,title)],[(stringDisplay prompt,newMap)],Vertical)

instance descr (!Icon,!String,!String)
where toPrompt (icon,title,prompt)	= (fromList [(TITLE_ATTRIBUTE,title),(ICON_ATTRIBUTE, toString icon)],[(stringDisplay prompt,newMap)],Vertical)
									
instance descr Title
where toPrompt (Title title)	= (put TITLE_ATTRIBUTE title newMap,[],Vertical)
	
instance descr Hint
where toPrompt (Hint hint)	= (put HINT_ATTRIBUTE hint newMap,[],Vertical)
	
instance descr Icon
where toPrompt icon	= (put ICON_ATTRIBUTE (toString icon) newMap,[],Vertical)

instance descr Attribute
where toPrompt (Attribute k v)	= (put k v newMap,[],Vertical)
	
instance descr Att
where toPrompt (Att a)			= toPrompt a
	
instance descr [d] | descr d
where
	toPrompt list = foldr merge (newMap,[],Vertical) (map toPrompt list)
	where
		merge (a1,c1,d1) (a2,c2,d2) = (mergeAttributes a1 a2, c1 ++ c2,d1)

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
