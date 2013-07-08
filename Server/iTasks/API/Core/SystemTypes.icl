implementation module iTasks.API.Core.SystemTypes
from StdFunc import until

import StdInt, StdBool, StdClass, StdArray, StdEnum, StdTuple, StdMisc, StdList, StdFunc, StdOrdList
import Data.List, Data.Functor, Text.JSON, Text.HTML, Text, Data.Map, Text.Encodings.Base64, Data.Tuple, dynamic_string, System.File
import iTasks.Framework.UIDefinition
import iTasks.Framework.Generic.Interaction
import iTasks.Framework.Generic.Visualization
import iTasks.Framework.Task, iTasks.Framework.TaskState, iTasks.Framework.Util
import iTasks.Framework.SerializationGraphCopy
import iTasks.Framework.IWorld

//For Client-Side evaluation
import graph_to_sapl_string, sapldebug, StdFile, StdMisc
import Sapl.Linker.LazyLinker, Sapl.Target.JS.CodeGeneratorJS, iTasks.Framework.ClientInterface

import System.Time, System.File, System.FilePath

from iTasks.Framework.UIDefinition import :: UIDef(..), :: UIControlSequence, :: UIActionSet, :: UIControlGroup, :: UIActions, :: UIControls, :: UITitle, :: UIDirection(..), :: UIAnnotatedControls, :: UIAbstractContainer, :: UIViewport, :: UIAction, :: UIControl, stringDisplay
from iTasks.API.Core.LayoutCombinators import mergeAttributes, setMargins

//* EmailAddress
derive JSONEncode		EmailAddress
derive JSONDecode		EmailAddress
derive gDefault			EmailAddress
derive gEq				EmailAddress
derive gVisualizeText	EmailAddress
derive gEditor			EmailAddress
derive gEditMeta		EmailAddress
derive gUpdate			EmailAddress
derive gVerify			EmailAddress

//* URL
gVisualizeText{|URL|}	_ val	= [toString val]

gEditor{|URL|} dp vv=:(URL url,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just (ATag [HrefAttr url] [Text url])},newMap)], vst)
	| otherwise
		# value = checkMaskValue mask url
		# ui = UIEditString defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value}
		= (NormalEditor [(ui,verifyAttributes vv (gEditMeta{|*|} (URL url)))],vst)

gUpdate{|URL|} target upd val = basicUpdate (\json url -> Just (maybe url (\s -> URL s) (fromJSON json))) target upd val

gVerify{|URL|} mv options = simpleVerify mv options

gEditMeta{|URL|} _ = [{label=Nothing,hint=Just "Enter a uniform resource locator (URL)"}]

derive JSONEncode		URL
derive JSONDecode		URL
derive gDefault			URL
derive gEq				URL

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

gEditor{|Note|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(setMargins 5 5 5 5 (UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap noteToHtml val}),newMap)],vst)
	| otherwise	
		# value = checkMaskValue mask ((\(Note v)  -> v) val)
		= (NormalEditor [(UIEditNote sizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},verifyAttributes vv (gEditMeta{|*|} val))],vst)
where	
	sizeOpts = {UISizeOpts|defaultSizeOpts & height = Just FlexSize, minHeight = Just WrapMin}
	
	// THIS IS A HACK!
	// The encoding of a Text constructor should escape newlines and convert them to <br> tags. Unfortunately it doesn't
	noteToHtml (Note s)	//TODO: Fix this in the toString of the Text constructor of HtmlTag type
		= case split "\n" s of
			[line]	= Text line
			lines	= SpanTag [] (intersperse (BrTag []) (map Text lines))

gUpdate{|Note|} target upd val = basicUpdateSimple target upd val

gVerify{|Note|} mv options = simpleVerify mv options
gEditMeta{|Note|} _ = [{label=Nothing,hint=Just "You may enter multiple lines of text"}]


derive gDefault			Note
derive gEq				Note

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

gEditor{|CleanCode|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled	
		# val = checkMask mask val
		= (NormalEditor [(setMargins 5 5 5 5 (UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap codeToHtml val}),newMap)],vst)
	| otherwise
		# value = checkMaskValue mask ((\(CleanCode v) -> v) val)
		= (NormalEditor [(UIEditCode sizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value} {UICodeOpts|lineNumbers=True},verifyAttributes vv (gEditMeta{|*|} val))],vst)
where	
	sizeOpts = {UISizeOpts|defaultSizeOpts & height = Just FlexSize, minHeight = Just WrapMin}
	
	codeToHtml (CleanCode s)
		= case split "\n" s of
			[line]	= Text line
			lines	= SpanTag [] (intersperse (BrTag []) (map Text lines))

gUpdate{|CleanCode|} target upd val = basicUpdate codeUpd target upd val
where
	codeUpd (JSONString s) _	= Just (CleanCode s)
	codeUpd _ old				= Just old

gVerify{|CleanCode|} mv options = simpleVerify mv options
gEditMeta{|CleanCode|} _ = [{label=Nothing,hint=Just "Enter a piece of Clean code"}]

derive gDefault		CleanCode
derive gEq			CleanCode

instance toString CleanCode
where
	toString (CleanCode s) = s

//* Money (ISO4217 currency codes are used)

gVisualizeText{|EUR|} _ val = [toString val]

gEditor{|EUR|}	dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled	
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap (\(EUR v) -> toString v) val},newMap)],vst)
	| otherwise
		# value = checkMaskValue mask ((\(EUR v) -> toReal v / 100.0) val)
		= (NormalEditor [(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gUpdate{|EUR|} target upd val = basicUpdateSimple target upd val

gVerify{|EUR|} mv options = simpleVerify mv options
gEditMeta{|EUR|} _ = [{label=Nothing,hint=Just "Enter an amount in EUR"}]

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

gEditor{|USD|}	dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled	
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
	| otherwise
		# value = checkMaskValue mask ((\(USD v) -> toReal v / 100.0) val)
		= (NormalEditor [(UIEditDecimal defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gUpdate{|USD|} target upd val = basicUpdateSimple target upd val

gVerify{|USD|} mv options = simpleVerify mv options
gEditMeta{|USD|} _ = [{label=Nothing,hint=Just "Enter an amount in USD"}]

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

//* (Local) date and time

JSONEncode{|Date|} d		= [JSONString (toString d)]

JSONDecode{|Date|} [JSONString s:c] | isDateFormat s	= (Just (fromString s), c)
JSONDecode{|Date|} c									= (Nothing, c)
isDateFormat s = size s == 10 && foldl (\ok i -> ok && (if (i == 4 || i == 7) (s.[i] == '-') (isDigit s.[i]))) True [0..9]

gVisualizeText{|Date|} _ val = [toString val]

gEditor{|Date|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
	| otherwise
		# value	= checkMaskValue mask val
		= (NormalEditor [(UIEditDate defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gDefault{|Date|} = {day = 1, mon = 1, year = 1970}
gUpdate{|Date|} target upd val = basicUpdate (\json old -> fromJSON json) target upd val
gVerify{|Date|} mv options = simpleVerify mv options
gEditMeta{|Date|} _ = [{label=Nothing,hint=Just "Enter a date (yyyy-mm-dd)"}]

derive gEq			Date

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
		addDays days date		= {Date|date & day = date.Date.day + days}
		addMonths months date	= {Date|date & mon = date.Date.mon + months}
		addYears years date		= {Date|date & year = date.Date.year + years}

		normDays date
			# monthLength = monthLengthOfDate date
			| date.day <= monthLength
				= date
				= normDays (normMonths {date & mon = date.Date.mon + 1, day = date.Date.day - monthLength})

		normMonths date
			| date.Date.mon <= 12
				= date
				= normMonths {Date|date & year = date.Date.year + 1, mon = date.Date.mon - 12}

		monthLengthOfDate date=:{Date|mon}
			| mon==2
				| isLeapYear date.Date.year
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
JSONDecode{|Time|} [JSONString s:c]	| isTimeFormat s	= (Just (fromString s), c)
JSONDecode{|Time|} c									= (Nothing, c)
isTimeFormat s = size s == 8 && foldl (\ok i -> ok && (if (i == 2 || i == 5) (s.[i] == ':') (isDigit s.[i]))) True [0..7]

gVisualizeText{|Time|} _ val = [toString val]

gEditor{|Time|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},newMap)],vst)
	| otherwise
		# value = checkMaskValue mask val
		= (NormalEditor [(UIEditTime defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gUpdate{|Time|} target upd val = basicUpdate (\json old -> fromJSON json) target upd val

gVerify{|Time|} mv options = simpleVerify mv options
gEditMeta{|Time|} _ = [{label=Nothing,hint=Just "Enter a time (hh:mm:ss)"}]

derive gDefault		Time
derive gEq			Time

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
		subSeconds s t	= {Time|t & sec = t.Time.sec - s}
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
derive gEditor	DateTime

gVisualizeText{|DateTime|} _ (DateTime date time)
	= [visualizeAsLabel date +++" "+++ visualizeAsLabel time]
gEditMeta{|DateTime|} _
	= [{label=Nothing,hint=Just "Enter a date and time"}]

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

gEditor {|Document|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(UIViewDocument defaultSizeOpts {UIViewOpts|value = val},newMap)],vst)
	| otherwise
		# value = checkMaskValue mask val
		= (NormalEditor [(UIEditDocument defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gUpdate {|Document|} [] upd (val,mask) = case fromJSON upd of
	Nothing		= ({Document|documentId = "", contentUrl = "", name="", mime="", size = 0},Blanked)// Reset
	Just doc	= (doc,Touched) //Update 

gVerify{|Document|} mv options = simpleVerify mv options
gEditMeta{|Document|} _ = [{label=Nothing,hint=Just "Upload a document"}]

derive JSONEncode		Document
derive JSONDecode		Document
derive gDefault			Document
derive gEq				Document

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

gEditor{|Username|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled	
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap (\(Username v) -> v) val},newMap)],vst)
	| otherwise
		# value = checkMaskValue mask ((\(Username v) -> v) val)
		= (NormalEditor [(UIEditString defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gUpdate{|Username|} target upd val = basicUpdateSimple target upd val
gVerify{|Username|} mv options = simpleVerify mv options
gEditMeta{|Username|} _ = [{label=Nothing,hint=Just "Enter a username"}]

derive gDefault			Username
derive gEq				Username
derive gVisualizeText	Username

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

gEditor{|Password|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled	
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = Just "********"},newMap)],vst)
	| otherwise	
		# value = checkMaskValue mask ((\(Password v) -> v) val)
		= (NormalEditor [(UIEditPassword defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},verifyAttributes vv (gEditMeta{|*|} val))],vst)
gUpdate{|Password|} target upd val = basicUpdateSimple target upd val
gVerify{|Password|} mv options = simpleVerify mv options
gEditMeta{|Password|} _ = [{label=Nothing,hint=Just "Enter a password"}]

derive gDefault			Password
derive gEq				Password

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
gEditor{|GoogleMap|} dp vv=:(val,mask,ver) vst=:{VSt|taskId}
	# editOpts	= {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Nothing}
	# opts		= mapOpts val
	= (NormalEditor [(UIEditGoogleMap defaultSizeOpts editOpts opts,verifyAttributes vv (gEditMeta{|*|} val))],vst)
where	
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
			= Just {GoogleMap | orig & perspective = {GoogleMapPerspective|orig.perspective & center = {lat=lat,lng=lng}, zoom = zoom, type = type}}
		# mbMarkerDrag = fromJSON json
		| isJust mbMarkerDrag
			# {MarkerDragUpdate|index,point=(lat,lng)}	= fromJust mbMarkerDrag
			= Just {GoogleMap | orig & markers = [if (i == index) {GoogleMapMarker|m & position = {lat=lat,lng=lng}} m \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		# mbMarkerClick = fromJSON json
		| isJust mbMarkerClick
			# {MarkerClickUpdate|index,event} = fromJust mbMarkerClick
			= Just {GoogleMap| orig & markers = [{GoogleMapMarker|m & selected = i == index} \\ m <- orig.GoogleMap.markers & i <- [0..]]}
		| otherwise	
			= Just orig

gVerify{|GoogleMap|} _ mv = alwaysValid mv
//derive gVerify GoogleMap

gDefault{|GoogleMapPerspective|} =
	{ GoogleMapPerspective
	| type				= ROADMAP
	, center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
	, zoom				= 10
	}
gDefault{|GoogleMapSettings|} =
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
derive gEditor GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gEditMeta		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gUpdate			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gVerify			GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon

//* A sliding scale
gVisualizeText{|Scale|}	_ {Scale|cur} = [toString cur]

gEditor{|Scale|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	# sliderOpts	= {UISliderOpts|minValue=(\{Scale|min} -> min) val,maxValue=(\{Scale|max} -> max) val}
	| disabled
		# val = checkMask mask val							
		# viewOpts = {UIViewOpts|value = fmap curVal val}  
		= (NormalEditor [(UIViewSlider defaultSizeOpts viewOpts sliderOpts, newMap)],vst)
	| otherwise
		# value = checkMaskValue mask (curVal val)
		# editOpts = {UIEditOpts|taskId = taskId, editorId = editorId dp, value = value}
		= (NormalEditor [(UIEditSlider defaultSizeOpts editOpts sliderOpts, verifyAttributes vv (gEditMeta{|*|} val))],vst)
where
	curVal {Scale|cur} = cur

gUpdate{|Scale|} target upd val
	= basicUpdate (\json i -> Just (maybe i (\cur -> {Scale|i & cur = cur}) (fromJSON json))) target upd val

gVerify{|Scale|} _ mv = alwaysValid mv

gDefault{|Scale|} = {Scale|min=1,cur=3,max=5}
gEditMeta{|Scale|} _	= [{label=Nothing,hint=Just "You can change the value by sliding the scale"}]

//* Progress bars
gVisualizeText{|Progress|}	_ {Progress|description} = [description]

gEditor{|Progress|} dp (val,mask,ver) vst=:{VSt|taskId}
	= (NormalEditor [(UIViewProgress defaultSizeOpts {UIViewOpts|value=Just (value val)} {UIProgressOpts|text = text val},newMap)],vst)
where
	text {Progress|description}	= description
		
	value {Progress|progress=ProgressRatio ratio} 
		| ratio < 0.0	= ProgressRatio 0.0
		| ratio > 1.0	= ProgressRatio 1.0
						= ProgressRatio ratio
	value {Progress|progress} = progress

gUpdate{|Progress|}	target upd val = val
gVerify{|Progress|} _ mv = alwaysValid mv
gEditMeta{|Progress|} _		= [{label=Nothing,hint=Nothing}]

derive gDefault			Progress

gVisualizeText{|ProgressAmount|} _ ProgressUndetermined		= ["Undetermined"]
gVisualizeText{|ProgressAmount|} _ (ProgressRatio r)		= [toString (entier (100.0 * r)) + "%"]

derive gDefault			ProgressAmount
derive gEditor 			ProgressAmount
derive gEditMeta		ProgressAmount
derive gUpdate			ProgressAmount
derive gVerify			ProgressAmount

//* Inclusion of external html files
gVisualizeText{|HtmlInclude|}	_ (HtmlInclude location)	= ["<External html: " + location + ">"]

gEditor{|HtmlInclude|} dp vv=:(HtmlInclude path,mask,ver) vst
	= (NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value=Just (IframeTag [SrcAttr path] [])},verifyAttributes vv (gEditMeta{|*|} (HtmlInclude path)))],vst)

gUpdate{|HtmlInclude|} target upd val = val

gVerify{|HtmlInclude|} _ mv = alwaysValid mv

derive gDefault HtmlInclude
derive gEditMeta HtmlInclude

//* Form buttons
gVisualizeText{|FormButton|}	_ val				= [val.FormButton.label]

gEditor{|FormButton|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	# text = Just val.FormButton.label
	# iconCls = Just val.FormButton.icon
	= (NormalEditor [(UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString "pressed")} {UIButtonOpts|text=text,iconCls=iconCls,disabled=False},verifyAttributes vv (gEditMeta{|*|} val))],vst)

gUpdate{|FormButton|} target upd val
	= basicUpdate (\st b -> Just {FormButton|b & state = st}) target upd val

gVerify{|FormButton|} _ mv = alwaysValid mv

gDefault{|FormButton|} = {FormButton | label = "Form Button", icon="", state = NotPressed}

derive gEditMeta FormButton

instance toString FormButton
where
	toString button = toString (pressed button)
	where
		pressed {FormButton|state}= case state of
			Pressed		= True
			NotPressed	= False

gVisualizeText{|ButtonState|}	_ NotPressed		= ["not pressed"]
gVisualizeText{|ButtonState|}	_ Pressed			= ["pressed"]

derive gDefault		ButtonState
derive gEditor		ButtonState
derive gEditMeta	ButtonState
derive gUpdate		ButtonState
derive gVerify		ButtonState

//* Table consisting of headers, the displayed data cells & possibly a selection
gVisualizeText{|Table|}	_ _	= ["<Table>"]

gEditor{|Table|} dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	= (NormalEditor [(UIGrid defaultSizeOpts
        {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=value val,options = options val}
        {UIGridOpts|columns = columns val,doubleClickAction=Nothing},verifyAttributes vv (gEditMeta{|*|} val))],vst)
where
	value (Table _ _ mbSel)	= maybe [] (\s->[s]) mbSel
	columns (Table headers _ _)	= headers
	options (Table _ cells _)	= map (map toString) cells
	
gUpdate{|Table|} target upd val
	= basicUpdate (\json (Table headers cells _) -> case fromJSON json of Just i = Just (Table headers cells (Just i)); _ = Just (Table headers cells Nothing)) target upd val

gVerify{|Table|} _ mv = alwaysValid mv
gDefault{|Table|} = Table [] [] Nothing

derive gEditMeta Table

toTable	:: ![a] -> Table | gEditMeta{|*|} a & gVisualizeText{|*|} a
toTable a = Table (headers a undef) (map row a) Nothing
where
	headers:: [a] a -> [String] | gEditMeta{|*|} a
	headers _ a = [fromMaybe "" label \\ {EditMeta|label} <- gEditMeta{|*|} a]

	row x =  [Text cell \\ cell <- gVisualizeText{|*|} AsRow x]
	
//* Simple tree type (used primarily for creating trees to choose from)
derive gDefault			Tree, TreeNode
derive gVisualizeText	Tree, TreeNode
derive gEditor	Tree, TreeNode
derive gEditMeta			Tree, TreeNode
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
gDefault{|ComboChoice|} _ _ = ComboChoice [] Nothing
gVisualizeText{|ComboChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))

gEditor{|ComboChoice|} fx gx _ hx _ _ _ _ _ hy _ _ dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
	| otherwise
		= (NormalEditor [(UIDropdown defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},verifyAttributes vv (gEditMeta{|*->*->*|} hx hy val))],vst)
where
	vvalue (ComboChoice options (Just sel))	= Just (hd (gx AsLabel (fst(options !! sel ))))
	vvalue _								= Nothing
	evalue (ComboChoice _ mbSel)			= maybe [] (\s->[s]) mbSel

	options (ComboChoice options _)			= [concat (gx AsLabel v) \\ (v,_) <- options]
 
gUpdate{|ComboChoice|} _ _ _ _ _ _ target upd val = updateChoice (\idx (ComboChoice options _) -> ComboChoice options idx) target upd val

gVerify{|ComboChoice|} _ _ mv options = customVerify (\(ComboChoice _ s) -> isJust s) (const "You must choose one item") mv options

instance Choice ComboChoice
where
	selectOption newSel (ComboChoice options _)					= ComboChoice options (setListOption options newSel)
	getSelection combo											= fromJust (getMbSelection combo)
	getMbSelection (ComboChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (ComboChoice options mbSel)				= fmap fst (getListOption options mbSel)

gDefault{|ComboChoiceNoView|} _ = ComboChoiceNoView [] Nothing
gVisualizeText{|ComboChoiceNoView|} fo mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))

gEditor{|ComboChoiceNoView|} _ gx _ hx _ _ dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
	| otherwise
		= (NormalEditor [(UIDropdown defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},verifyAttributes vv (gEditMeta{|*->*|} hx val))],vst)
where	
	vvalue (ComboChoiceNoView options (Just sel))	= Just (hd (gx AsLabel (options !! sel )))
	vvalue _										= Nothing
	evalue (ComboChoiceNoView _ mbSel)				= maybe [] (\s->[s]) mbSel
	
	options (ComboChoiceNoView options _)		= [concat (gx AsLabel v) \\ v <- options]

gUpdate{|ComboChoiceNoView|} _ _ _ target upd val = updateChoice (\idx (ComboChoiceNoView options _) -> ComboChoiceNoView options idx) target upd val

gVerify{|ComboChoiceNoView|} _ mv options = customVerify (\(ComboChoiceNoView _ s) -> isJust s) (const "You must choose one item") mv options

instance ChoiceNoView ComboChoiceNoView
where
	selectOptionNoView newSel (ComboChoiceNoView options _)		= ComboChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView combo									= fromJust (getMbSelectionNoView combo)
	getMbSelectionNoView (ComboChoiceNoView options mbSel)		= getListOption options mbSel

gDefault{|RadioChoice|} _ _ = RadioChoice [] Nothing
gVisualizeText{|RadioChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))

gEditor{|RadioChoice|} _ gx _ hx _ _ _ _ _ hy _ _ dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
	| otherwise
		= (NormalEditor [(UIRadioGroup defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},verifyAttributes vv (gEditMeta{|*->*->*|} hx hy val))],vst)
where
	vvalue (RadioChoice options (Just sel))	= Just (hd (gx AsLabel (fst(options !! sel ))))
	vvalue _								= Nothing
	evalue (RadioChoice _ mbSel)			= maybe [] (\i -> [i]) mbSel
	
	options (RadioChoice options _)			= [concat (gx AsLabel v) \\ (v,_) <- options]


gUpdate{|RadioChoice|} _ _ _ _ _ _ target upd val
	= updateChoice (\idx (RadioChoice options _) -> RadioChoice options idx) target upd val

gVerify{|RadioChoice|} _ _		mv options = simpleVerify mv options

instance Choice RadioChoice
where
	selectOption newSel (RadioChoice options _)					= RadioChoice options (setListOption options newSel)
	getSelection radios											= fromJust (getMbSelection radios)
	getMbSelection (RadioChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (RadioChoice options mbSel)				= fmap fst (getListOption options mbSel)

gDefault{|RadioChoiceNoView|} _ = RadioChoiceNoView [] Nothing
gVisualizeText{|RadioChoiceNoView|}	fo mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))

gEditor{|RadioChoiceNoView|} _ gx _ hx _ _ dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},newMap)],vst)
	| otherwise
		= (NormalEditor [(UIRadioGroup defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},verifyAttributes vv (gEditMeta{|*->*|} hx val))],vst)
where
	vvalue (RadioChoiceNoView options (Just sel))	= Just (hd (gx AsLabel (options !! sel )))
	vvalue _										= Nothing
	evalue (RadioChoiceNoView _ mbSel)				= maybe [] (\s->[s]) mbSel

	options (RadioChoiceNoView options _)			= [concat (gx AsLabel v) \\ v <- options]

gUpdate{|RadioChoiceNoView|} _ _ _ target upd val
	= updateChoice (\idx (RadioChoiceNoView options _) -> RadioChoiceNoView options idx) target upd val

gVerify{|RadioChoiceNoView|} _	mv options = simpleVerify mv options

instance ChoiceNoView RadioChoiceNoView
where
	selectOptionNoView newSel (RadioChoiceNoView options _)		= RadioChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView radios									= fromJust (getMbSelectionNoView radios)
	getMbSelectionNoView (RadioChoiceNoView options mbSel)		= getListOption options mbSel

gDefault{|TreeChoice|} _ _ = TreeChoice (Tree []) Nothing

gVisualizeText{|TreeChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))

gEditor{|TreeChoice|} _ gx _ hx _ _ _ _ _ hy _ _ dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	# viz		= [(UITree defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=value val,options = options val mask} {UITreeOpts|doubleClickAction=Nothing},verifyAttributes vv (gEditMeta{|*->*->*|} hx hy val))]
	= (NormalEditor viz,vst)
where
	value  (TreeChoice _ mbSel) 	= maybe [] (\s->[s]) mbSel
	
	options (TreeChoice (Tree nodes) _) msk = fst (mkTree nodes 0 )
		where
			expanded = case msk of
				TouchedWithState s 	= case fromJSON s of Just expanded = expanded; _ = []
				_					= []
				
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

gUpdate{|TreeChoice|} _ _ _ _ _ _ [] upd (TreeChoice options sel,mask) = case fromJSON upd of
	Just ("sel",idx,val)	= (TreeChoice options (if val (Just idx) Nothing), touch mask)
	Just ("exp",idx,val)	= (TreeChoice options sel, if val (expand idx mask) (collapse idx mask))
	_						= ((TreeChoice options sel), mask)

gUpdate{|TreeChoice|} _ _ _ _ _ _ target upd val = val


gVerify{|TreeChoice|} _ _ mv options = simpleVerify mv options

instance Choice TreeChoice
where
	selectOption newSel (TreeChoice options _)					= TreeChoice options (setTreeOption options newSel)
	getSelection tree											= fromJust (getMbSelection tree)
	getMbSelection (TreeChoice options mbSel)					= fmap snd (getTreeOption options mbSel)
	getMbSelectionView (TreeChoice options mbSel)				= fmap fst (getTreeOption options mbSel)

gDefault{|TreeChoiceNoView|} _ = TreeChoiceNoView (Tree []) Nothing

gVisualizeText{|TreeChoiceNoView|} fo mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))

gEditor{|TreeChoiceNoView|} _ gx _ _ _ _ dp (val,mask,ver) vst=:{VSt|taskId}
	= (NormalEditor [(UITree defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=value val,options = options val} {UITreeOpts|doubleClickAction=Nothing},newMap)],vst)
where
	value (TreeChoiceNoView _ mbSel) = maybe [] (\s->[s]) mbSel
	
	options (TreeChoiceNoView (Tree nodes) _) = fst (mkTree nodes 0)
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

gUpdate{|TreeChoiceNoView|} _ _ _ target upd val = updateChoice update target upd val
where
	update ("sel",idx,val)		(TreeChoiceNoView options _) 		= TreeChoiceNoView options (if val (Just idx) Nothing)
	update ("exp",idx,val)		(TreeChoiceNoView options sel)		= TreeChoiceNoView options sel
	update _					treechoice							= treechoice

gVerify{|TreeChoiceNoView|} _ mv options = simpleVerify mv options
	
instance ChoiceNoView TreeChoiceNoView
where
	selectOptionNoView newSel (TreeChoiceNoView options _)		= TreeChoiceNoView options (setTreeOptionNoView options newSel)
	getSelectionNoView tree										= fromJust (getMbSelectionNoView tree)
	getMbSelectionNoView (TreeChoiceNoView options mbSel)		= getTreeOption options mbSel

gDefault{|GridChoice|} _ _ = GridChoice [] Nothing

gVisualizeText{|GridChoice|} fv _ mode val = fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))	

gEditor{|GridChoice|} _ gx _ hx _ _ _ _ _ _ _ _ dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	= (NormalEditor [(UIGrid defaultSizeOpts
		{UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=value val,options = options val}
		{UIGridOpts|columns = [fromMaybe "" label\\ {EditMeta|label} <- hx undef], doubleClickAction=Nothing},newMap)],vst)
where	
	value (GridChoice options mbSel)	= maybe [] (\s->[s]) mbSel
	options (GridChoice options _)		= [gx AsRow opt \\ (opt,_) <- options]

gUpdate{|GridChoice|} _ _ _ _ _ _ target upd val
	= updateChoice (\idxs (GridChoice options _) -> GridChoice options (case idxs of [idx:_] = (Just idx); _ = Nothing)) target upd val

gVerify{|GridChoice|} _ _ _ mv = alwaysValid mv

instance Choice GridChoice
where
	selectOption newSel (GridChoice options _)					= GridChoice options (setListOption options newSel)
	getSelection grid											= fromJust (getMbSelection grid)
	getMbSelection (GridChoice options mbSel)					= fmap snd (getListOption options mbSel)
	getMbSelectionView (GridChoice options mbSel)				= fmap fst (getListOption options mbSel)

gDefault{|GridChoiceNoView|} _ = GridChoiceNoView [] Nothing

gVisualizeText{|GridChoiceNoView|} fo mode val = fromMaybe ["No item selected"] (fmap (\v -> fo mode v) (getMbSelectionNoView val))	

gEditor{|GridChoiceNoView|} _ gx _ hx jex jdx dp (val,mask,ver) vst=:{VSt|taskId,disabled}
	= (NormalEditor [(UIGrid defaultSizeOpts
			{UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=value val,options =options val}
			{UIGridOpts|columns = [fromMaybe "" label\\ {EditMeta|label} <-hx undef],doubleClickAction=Nothing},newMap)],vst)
where
	value (GridChoiceNoView options mbSel)	= maybe [] (\s->[s]) mbSel
	options (GridChoiceNoView options _)	= [gx AsRow opt \\ opt <- options]
	
gUpdate{|GridChoiceNoView|} _ _ _ target upd val
	= updateChoice (\idxs (GridChoiceNoView options _) -> GridChoiceNoView options (case idxs of [idx:_] = (Just idx); _ = Nothing)) target upd val

gVerify{|GridChoiceNoView|} _ _ mv= alwaysValid mv
	
instance ChoiceNoView GridChoiceNoView
where
	selectOptionNoView newSel (GridChoiceNoView options _)		= GridChoiceNoView options (setListOptionNoView options newSel)
	getSelectionNoView grid										= fromJust (getMbSelectionNoView grid)
	getMbSelectionNoView (GridChoiceNoView options mbSel)		= getListOption options mbSel

gDefault{|DynamicChoice|} fx fy = DCRadio (gDefault{|*->*->*|} fx fy)

gVisualizeText{|DynamicChoice|}		fv fo mode (DCRadio val)	= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCCombo val)	= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCGrid val)		= gVisualizeText{|*->*->*|} fv fo mode val
gVisualizeText{|DynamicChoice|}		fv fo mode (DCTree val)		= gVisualizeText{|*->*->*|} fv fo mode val

gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 dp (DCCombo val,mask,ver) vst
	= gEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 dp (val,mask,ver) vst
gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 dp (DCRadio val,mask,ver) vst
	= gEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 dp (val,mask,ver) vst
gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 dp (DCTree val,mask,ver) vst
	= gEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 dp (val,mask,ver) vst
gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 dp (DCGrid val,mask,ver) vst
	= gEditor{|*->*->*|} f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 dp (val,mask,ver) vst

gUpdate{|DynamicChoice|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd	(DCCombo val,mask)	= appFst DCCombo (gUpdate{|*->*->*|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd (val,mask))
gUpdate{|DynamicChoice|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd	(DCRadio val,mask)	= appFst DCRadio (gUpdate{|*->*->*|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd (val,mask))
gUpdate{|DynamicChoice|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd	(DCTree val,mask)	= appFst DCTree (gUpdate{|*->*->*|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd (val,mask))
gUpdate{|DynamicChoice|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd	(DCGrid val,mask)	= appFst DCGrid (gUpdate{|*->*->*|} gUpdx gDefx jDecx gUpdy gDefy jDecy target upd (val,mask))

gVerify{|DynamicChoice|} fx fy options (DCCombo v,mask) = gVerify{|*->*->*|} fx fy options (v,mask)
gVerify{|DynamicChoice|} fx fy options (DCRadio v,mask) = gVerify{|*->*->*|} fx fy options (v,mask)
gVerify{|DynamicChoice|} fx fy options (DCTree v,mask) = gVerify{|*->*->*|} fx fy options (v,mask)
gVerify{|DynamicChoice|} fx fy options (DCGrid v,mask) = gVerify{|*->*->*|} fx fy options (v,mask)
	
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

gDefault{|DynamicChoiceNoView|} fx = DCRadioNoView (gDefault{|*->*|} fx)

gVisualizeText{|DynamicChoiceNoView|} fo mode (DCRadioNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCComboNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCTreeNoView val)	= gVisualizeText{|*->*|} fo mode val
gVisualizeText{|DynamicChoiceNoView|} fo mode (DCGridNoView val)	= gVisualizeText{|*->*|} fo mode val

gEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 f5 f6 dp (DCComboNoView val,mask,ver) vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) vst
gEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 f5 f6 dp (DCRadioNoView val,mask,ver) vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) vst
gEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 f5 f6 dp (DCTreeNoView val,mask,ver) vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) vst
gEditor{|DynamicChoiceNoView|} f1 f2 f3 f4 f5 f6 dp (DCGridNoView val,mask,ver) vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) vst

gUpdate{|DynamicChoiceNoView|} gUpdx gDefx jDecx target upd (DCComboNoView val,mask)= appFst DCComboNoView (gUpdate{|*->*|} gUpdx gDefx jDecx target upd (val,mask))
gUpdate{|DynamicChoiceNoView|} gUpdx gDefx jDecx target upd (DCRadioNoView val,mask)= appFst DCRadioNoView (gUpdate{|*->*|} gUpdx gDefx jDecx target upd (val,mask))
gUpdate{|DynamicChoiceNoView|} gUpdx gDefx jDecx target upd (DCTreeNoView val,mask)	= appFst DCTreeNoView (gUpdate{|*->*|} gUpdx gDefx jDecx target upd (val,mask))
gUpdate{|DynamicChoiceNoView|} gUpdx gDefx jDecx target upd (DCGridNoView val,mask)	= appFst DCGridNoView (gUpdate{|*->*|} gUpdx gDefx jDecx target upd (val,mask))

gVerify{|DynamicChoiceNoView|} fx options (DCComboNoView v,mask) = gVerify{|*->*|} fx options (v,mask)
gVerify{|DynamicChoiceNoView|} fx options (DCRadioNoView v,mask) = gVerify{|*->*|} fx options (v,mask)
gVerify{|DynamicChoiceNoView|} fx options (DCTreeNoView v,mask) = gVerify{|*->*|} fx options (v,mask)
gVerify{|DynamicChoiceNoView|} fx options (DCGridNoView v,mask) = gVerify{|*->*|} fx options (v,mask)

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

gDefault{|CheckMultiChoice|} _ _ = CheckMultiChoice [] []
gVisualizeText{|CheckMultiChoice|} fv _ _ val = gVisualizeText{|* -> *|} fv  AsLabel (getSelectionViews val)

gEditor{|CheckMultiChoice|} _ gx _ hx _ _ _ _ _ hy _ _ dp vv=:(val,mask,ver) vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = Just (vvalue val)},newMap)],vst)
	| otherwise
		= (NormalEditor [(UICheckboxGroup defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},verifyAttributes vv (gEditMeta{|*->*->*|} hx hy val))],vst)
where
	vvalue (CheckMultiChoice options sel)	= join "," ([hd (gx AsLabel (fst (options !! i ))) \\ i <- sel])
	evalue (CheckMultiChoice _ sel)			= sel
	options (CheckMultiChoice options _)	= [concat (gx AsLabel v) \\ (v,_) <- options]

gUpdate{|CheckMultiChoice|} _ _ _ _ _ _ target upd val = basicUpdate (\json (CheckMultiChoice opts sel) -> case fromJSON json of Just (i,v) = Just (CheckMultiChoice opts (updateSel i v sel)); _ = (Just (CheckMultiChoice opts sel))) target upd val
where
	updateSel i True sel	= removeDup [i:sel]
	updateSel i False sel 	= removeMember i sel

gVerify{|CheckMultiChoice|} _ _	vm options = simpleVerify vm options
	
instance MultiChoice CheckMultiChoice
where
	selectOptions newSels (CheckMultiChoice options _)			= CheckMultiChoice options (setListOptions options newSels)
	getSelections (CheckMultiChoice options sels)				= fmap snd (getListOptions options sels)
	getSelectionViews (CheckMultiChoice options sels)			= fmap fst (getListOptions options sels)

// Utility functions for Choice and MultiChoice instances
touch (TouchedUnparsed r)	= TouchedUnparsed r
touch (TouchedWithState s)	= TouchedWithState s
touch (CompoundMask c)	    = CompoundMask c
touch _						= Touched

expand idx (TouchedWithState s) = case fromJSON s of
	Just list	= TouchedWithState (toJSON (removeDup [idx:list]))
	_			= TouchedWithState (toJSON [idx])
expand idx _	= TouchedWithState (toJSON [idx])
 
collapse idx (TouchedWithState s) = case fromJSON s of
	Just list	= TouchedWithState (toJSON (removeMember idx list))
	_			= TouchedWithState s
collapse idx m = m

updateChoice select target upd val = basicUpdate (\json choice -> Just (maybe choice (\i -> select i choice) (fromJSON json))) target upd val

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
derive gEditMeta			ComboChoice, RadioChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive gEditMeta			ComboChoiceNoView,RadioChoiceNoView,TreeChoiceNoView,DynamicChoiceNoView,GridChoiceNoView

//* Visualization wrappers
gVisualizeText{|VisualizationHint|} fx mode val = case val of
	VHHidden x		= gVisualizeText{|* -> *|} fx mode (Hidden x)
	VHDisplay x		= gVisualizeText{|* -> *|} fx mode (Display x)
	VHEditable x	= gVisualizeText{|* -> *|} fx mode (Editable x)

gEditor{|VisualizationHint|} fx gx dx hx jex jdx dp (val,mask,ver) vst = case val of
	VHHidden x		= gEditor{|* -> *|} fx gx dx hx jex jdx dp (Hidden x,mask,ver) vst
	VHDisplay x		= gEditor{|* -> *|} fx gx dx hx jex jdx dp (Display x,mask,ver) vst
	VHEditable x	= gEditor{|* -> *|} fx gx dx hx jex jdx dp (Editable x,mask,ver) vst

gUpdate{|VisualizationHint|} 	gUpdx gDefx jDecx target upd val=:(VHEditable s,mask)	= wrapperUpdate gUpdx fromVisualizationHint VHEditable target upd val
gUpdate{|VisualizationHint|} 	gUpdx gDefx jDecx target upd val=:(VHDisplay s,mask)	= wrapperUpdate gUpdx fromVisualizationHint VHDisplay target upd val
gUpdate{|VisualizationHint|} 	gUpdx gDefx jDecx target upd val=:(VHHidden s,mask)		= wrapperUpdate gUpdx fromVisualizationHint VHHidden target upd val

gVerify{|VisualizationHint|} fx options (v,mask) = case v of
	(VHEditable v) = verifyEditable fx options (v,mask)
	(VHDisplay v) = verifyDisplay fx options (v,mask)
	(VHHidden v) = fx options (v,mask)
	
fromVisualizationHint :: !(VisualizationHint .a) -> .a
fromVisualizationHint (VHEditable a) = a
fromVisualizationHint (VHDisplay a) = a
fromVisualizationHint (VHHidden a) = a

toVisualizationHint :: !.a -> (VisualizationHint .a)
toVisualizationHint a = (VHEditable a)

gVisualizeText{|Hidden|} _ _ _ = []

gEditor{|Hidden|} fx _ _ _ _ _ dp val vst = (HiddenEditor,vst)

gUpdate{|Hidden|} gUpdx gDefx jDecx target upd val = wrapperUpdate gUpdx fromHidden Hidden target upd val

gVerify{|Hidden|} fx options (Hidden v,mask) = fx options (v,mask)

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)

gVisualizeText{|Display|} fx mode (Display val)	= fx mode val

gEditor{|Display|} fx _ _ _ _ _ dp (val,mask,ver) vst=:{VSt|disabled}
	# (def,vst) = fx dp (fromDisplay val,mask,ver) {VSt | vst &  disabled = True}
	= (def,{VSt | vst & disabled = disabled})

gUpdate{|Display|} gUpdx gDefx jDecx target upd val = wrapperUpdate gUpdx fromDisplay Display target upd val

gVerify{|Display|} fx options (Display d,mask) = verifyDisplay fx options (d,mask)

fromDisplay :: !(Display .a) -> .a
fromDisplay (Display a) = a

toDisplay :: !.a -> (Display .a)
toDisplay a = (Display a)

gVisualizeText{|Editable|} fx mode(Editable val) = fx mode val

gEditor{|Editable|} fx _ _ _ _ _ dp (val,mask,ver) vst=:{VSt|disabled}
	# (def,vst) = fx dp (fromEditable val,mask,ver) {VSt | vst & disabled = False}
	= (def,{VSt | vst & disabled = disabled})

gUpdate{|Editable|} gUpdx gDefx jDecx target upd val = wrapperUpdate gUpdx fromEditable Editable target upd val

gVerify{|Editable|} fx options (Editable e,mask) = verifyEditable fx options (e,mask)
	
fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

//Utility for gUpdate 
wrapperUpdate fx get set target upd (val,mask)
	# (w,mask) = fx target upd (get val,mask)
	= (set w,mask)
		
//Utility for gVerify	
verifyEditable fx options mv = fx {VerifyOptions|options & disabled = False} mv
verifyDisplay fx options mv = fx {VerifyOptions|options & disabled = True} mv


derive JSONEncode		Hidden, Display, Editable, VisualizationHint
derive JSONDecode		Hidden, Display, Editable, VisualizationHint
derive gDefault			Hidden, Display, Editable, VisualizationHint
derive gEq				Hidden, Display, Editable, VisualizationHint
derive gEditMeta			Hidden, Display, Editable, VisualizationHint

//* Client-side types
JSONEncode{|Editlet|} _ _ tt = [dynamicJSONEncode tt]		
JSONDecode{|Editlet|} _ _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Editlet|} _ _ c = (Nothing,c)

gDefault{|Editlet|} fa _
	= {Editlet|value=fa,html = \_ -> RawText "", handlers=[], genDiff = \_ _ -> Nothing, appDiff = \_ x -> x}

gEq{|Editlet|} fa _ x y = fa x.Editlet.value y.Editlet.value //Only compare values

gVisualizeText{|Editlet|} fa _ mode {Editlet|value} = fa mode value

gEditor{|Editlet|} fa textA defaultA headersA jsonEncA jsonDecA _ _ _ _ jsonEncD jsonDecD dp ({Editlet|value,html,handlers,genDiff,appDiff},mask,ver) vst=:{VSt|taskId,iworld}
	# (jsScript,jsEvents, jsIV, jsGD, jsAD, iworld)	= linkEditletJS [(id, event, f) \\(ComponentEvent id event f) <- handlers] clientInit clientGenDiff clientAppDiff iworld
	# iworld									= addDiffer iworld
	= (NormalEditor [(ui jsScript jsEvents jsIV jsGD jsAD, newMap)],{VSt|vst & iworld = iworld})
where
    htmlId = "editlet-" +++ taskId +++ "-" +++ editorId dp

	ui jsScript jsEvents jsIV jsGD jsAD
		= UIEditlet defaultSizeOpts {UIEditletOpts|taskId=taskId,editorId=editorId dp,value=toJSONA value, html = toString (html htmlId)
								    ,script = Just jsScript, events = Just jsEvents, initValue = Just jsIV, genDiff = Just jsGD, appDiff = Just jsAD}
	
	toJSONA a = case jsonEncA a of
		[json:_]	= json
		_			= JSONNull
	toJSOND d = case jsonEncD d of
		[json:_]	= json
		_			= JSONNull
	
	clientInit json = case jsonDecA [json] of
		(Just a,_)	= a
		_			= abort "Editlet cannot initialize its value"
	
	serverGenDiff jsonOld jsonNew
		= case (jsonDecA [jsonOld],jsonDecA [jsonNew]) of
			((Just old,_),(Just new,_))	= case genDiff old new of
				Just diff				= Just (toJSOND diff)
				Nothing					= Nothing
			_							= Nothing
	
	clientAppDiff json old = case jsonDecD [json] of
		(Just diff,_)	= appDiff diff old
		_				= old
	
	clientGenDiff old new = case (genDiff old new) of
		Just diff		= toJSOND diff
		_				= JSONNull
	
	addDiffer iworld=:{IWorld|uiDiffers}
		= {IWorld|iworld & uiDiffers = put (taskId,editorId dp) serverGenDiff uiDiffers}

//Copy/paste from tasklet linker
linkEditletJS eventHandlers initValueFunc genDiffFunc appDiffFunc iworld=:{IWorld|world,sdkDirectory}
	/* 0. Load Clean flavour */
	# flavfile = sdkDirectory </> "Server" </> "lib" </> "SAPL" </>"clean.f"	
	# (flavres, world) = readFile flavfile world
	| isError flavres
		= abort ("Flavour file cannot be found at " +++ flavfile)
	# mbFlav = toFlavour (fromOk flavres)
	| isNothing mbFlav
		= abort "Error in flavour file"
	/* 1. First, we collect all the necessary function definitions to generate ParserState */
	# (ls, world) = generateLoaderState ["sapl"] [] world
	# saplIV = graph_to_sapl_string initValueFunc
	# (ls, a, saplIV, world) = linkByExpr ls newAppender saplIV world
	# saplGD = graph_to_sapl_string genDiffFunc
	# (ls, a, saplGD, world) = linkByExpr ls a saplGD world
	# saplAD = graph_to_sapl_string appDiffFunc
	# (ls, a, saplAD, world) = linkByExpr ls a saplAD world
	// link functions indicated by event handlers
	# (ls, a, saplEvents, world) = foldl (\(ls, a, hs, world) (e1,e2,f) = 
				let (ls2, a2, f2, world2) = linkByExpr ls a (graph_to_sapl_string f) world
				 in (ls2, a2, [(e1,e2,f2):hs], world2)) 
			(ls, a, [], world) eventHandlers
	/* 2. Generate function definitions and ParserState */
	# sapl = toString a	
	# (jsScript, mbPst) = case sapl of
		"" = ("", Nothing)
		   = let (script, pst) = handlerr (generateJS (fromJust mbFlav) False sapl) in (toString script, Just pst)
	/* 3. Generate expressions by ParserState */							
	# jsIV = toString (handlerr (exprGenerateJS (fromJust mbFlav) False saplIV mbPst))
	# jsGD = toString (handlerr (exprGenerateJS (fromJust mbFlav) False saplGD mbPst))
	# jsAD = toString (handlerr (exprGenerateJS (fromJust mbFlav) False saplAD mbPst))
	# jsEvents = map (\(id,event,saplhandler) = (id,event,toString (handlerr 
				(exprGenerateJS (fromJust mbFlav) False saplhandler mbPst)))) saplEvents
	= (jsScript, jsEvents, jsIV, jsGD, jsAD, {iworld & world=world})

handlerr (Error str) = abort ("Editlet error: " +++ str)
handlerr (Ok a) = a

gEditMeta{|Editlet|} fa _ {Editlet|value} = fa value

gUpdate{|Editlet|} fa _ jDeca _ _ jDecd [] json (ov=:{Editlet|value,appDiff},omask)
	= case jDecd [json] of
		(Just diff,_)	= ({Editlet|ov & value = appDiff diff value},Touched)
		_				= (ov,omask)

gUpdate{|Editlet|} fa _ _ _ _ _ _ _ mv = mv

gVerify{|Editlet|} fa _ _ mv = alwaysValid mv

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

subMasks :: !Int InteractionMask -> [InteractionMask]
subMasks n (CompoundMask ms) = ms
subMasks n m = repeatn n m

isTouched :: !InteractionMask -> Bool
isTouched Touched = True
isTouched (TouchedUnparsed _)	= True
isTouched (TouchedWithState _)	= True
isTouched Blanked	 			= True
isTouched (CompoundMask ms)		= or (map isTouched ms) //TODO make more efficient
isTouched _						= False

toPairMask :: !Int !InteractionMask -> InteractionMask
toPairMask len mask = split len (subMasks len mask)
where
	split 1 [mask] = mask
	split 2 masks 	= CompoundMask masks
	split n masks	= CompoundMask [split middle left,split (n - middle) right]
	where
		middle = n / 2
		(left,right) = splitAt middle masks

subVerifications :: !Int Verification -> [Verification]
subVerifications n (CompoundVerification vs) = vs
subVerifications n v = repeatn n v

toPairVerification :: !Int !Verification -> Verification
toPairVerification 0 ver = ver
toPairVerification 1 ver = ver
toPairVerification len ver = split len (subVerifications len ver)
where
	split 1 [ver] 	= ver
	split 2 vers 	= CompoundVerification vers
	split n vers	= CompoundVerification [split middle left,split (n - middle) right]
	where
		middle = n / 2
		(left,right) = splitAt middle vers

fromPairVerification :: !Int !Verification -> Verification
fromPairVerification 0 ver = ver
fromPairVerification 1 ver = ver
fromPairVerification len ver = CompoundVerification (join len ver)
where	
	join 1 ver = [ver]
	join 2 (CompoundVerification vers)			= vers
	join n (CompoundVerification [left,right])	= join middle left ++ join (n - middle) right
	where
		middle = n / 2
		
derive JSONEncode InteractionMask, Verification
derive JSONDecode InteractionMask, Verification

//Utility functions
editorId :: !DataPath -> String
editorId dp = "v" + join "-" (map toString dp)

s2dp :: !String -> DataPath
s2dp str 
	| textSize str < 2	= []
						= map toInt (split "-" (subString 1 (textSize str) str))

gVisualizeText{|User|} _ val = [toString val]
gUpdate{|User|} target upd val = basicUpdateSimple target upd val

gVerify{|User|} mv options = simpleVerify mv options 

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

derive JSONEncode		TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive JSONDecode		TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gDefault			TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gEq				TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gVisualizeText	TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gEditor			TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gEditMeta		TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, User, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gUpdate			TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action, ActionOption, Hotkey, Trigger
derive gVerify			TaskValue, ManagementMeta, ProgressMeta, TaskPriority, TaskListItem, UserConstraint, Action, ActionOption, Hotkey, Trigger

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
