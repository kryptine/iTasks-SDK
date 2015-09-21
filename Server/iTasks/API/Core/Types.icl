implementation module iTasks.API.Core.Types
from StdFunc import until, const, id
import StdInt, StdBool, StdClass, StdArray, StdEnum, StdTuple, StdMisc, StdList, StdOrdList
import GenLexOrd
import Data.Either, Data.Functor, Text.JSON, Text.HTML, Text, Text.Encodings.Base64, Data.Tuple, dynamic_string, System.File
from Data.Map import :: Map, :: Size
from Data.List import instance Functor []
import qualified Data.List as DL
import qualified Data.Map as DM
import iTasks.UI.Definition
import iTasks._Framework.Generic.Interaction
import iTasks._Framework.Generic.Visualization
import iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks._Framework.Util
import iTasks._Framework.Serialization
import iTasks._Framework.IWorld

import System.Time, System.File, System.FilePath
import iTasks._Framework.SDS
from iTasks.UI.Definition import :: UIDef(..), :: UIContent(..), :: UIForm, :: UIActions, :: UIDirection(..), :: UIBlock, :: UIViewport, :: UIAction, :: UIControl, stringDisplay
from iTasks.UI.Layout import mergeAttributes, setMargins
from iTasks.API.Core.Tasks import treturn
from iTasks.API.Common.TaskCombinators import tbind, @

instance TFunctor Task where
  tmap f x = x @ f
instance TApplicative Task where
  (<#>) tf ta = tf >>= \f -> tmap f ta
  return x    = treturn x
instance TMonad Task where
  (>>=) l r = tbind l r
  (>>|) l r = l >>= \_ -> r

instance TFunctor Maybe where
  tmap f (Just x) = Just (f x)
  tmap _ _        = Nothing

instance TApplicative Maybe where
  (<#>) (Just f) (Just x) = Just (f x)
  (<#>) _ _ = Nothing
  return x = Just x
instance TMonad Maybe where
  (>>=) (Just x) f = f x
  (>>=) _ _ = Nothing
  (>>|) l r = l >>= \_ -> r

instance TFunctor [] where
  tmap f xs = map f xs
instance TApplicative [] where
  (<#>) fs xs = [f x \\ f <- fs, x <- xs]
  return x = [x]
instance TMonad [] where
  (>>=) xs f = [y \\ x <- xs, y <- f x]
  (>>|) l r = l >>= \_ -> r

instance TFunctor (Either e) where
  tmap f (Right x) = Right (f x)
  tmap _ (Left x)  = Left x
instance TApplicative (Either e) where
  (<#>) (Right f) (Right x) = Right (f x)
  (<#>) (Left e) _ = Left e
  (<#>) _ (Left e) = Left e
  return x = Right x
instance TMonad (Either e) where
  (>>=) (Left x) _ = Left x
  (>>=) (Right x) f = f x
  (>>|) l r = l >>= \_ -> r

(@$) infixl 1 :: (a -> b) (f a) -> f b | iTask a & iTask b & TFunctor f
(@$) f x = tmap f x

JSONEncode{|RWShared|} _ _ _ _ s = []
JSONDecode{|RWShared|} _ _ _ _ s = (Nothing, s)
gEq{|RWShared|} _ _ _ _ _ = False
gDefault{|RWShared|} _ _ _ = SDSSource { SDSSource
                                       | name  = "gDefault RWShared"
                                       , read  = \_ w -> (Error (dynamic "", "No gDefault RWShared implementation"), w)
                                       , write = \_ _ w -> (Error (dynamic "", "No gDefault RWShared implementation"), w)}
undef = undef

//* EmailAddress
derive JSONEncode		EmailAddress
derive JSONDecode		EmailAddress
derive gDefault			EmailAddress
derive gEq				EmailAddress
derive gText	        EmailAddress
derive gEditor			EmailAddress
derive gEditMeta		EmailAddress
derive gUpdate			EmailAddress
derive gVerify			EmailAddress

instance toString EmailAddress
where
	toString (EmailAddress email) = email

instance html EmailAddress
where
	html (EmailAddress email) = ATag [HrefAttr ("mailto:" +++ email)] [Text email]

//* PhoneNumber
derive JSONEncode		PhoneNumber
derive JSONDecode		PhoneNumber
derive gDefault			PhoneNumber
derive gEq				PhoneNumber
derive gText	        PhoneNumber
derive gEditor			PhoneNumber
derive gEditMeta		PhoneNumber
derive gUpdate			PhoneNumber
derive gVerify			PhoneNumber

instance toString PhoneNumber
where
	toString (PhoneNumber num) = num

instance html PhoneNumber
where
	html (PhoneNumber num) = Text num

//* URL
gText{|URL|}	_ val	= [maybe "" toString val]

gEditor{|URL|} dp vv=:(URL url,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value = Just (ATag [HrefAttr url] [Text url])},'DM'.newMap)], vst)
	| otherwise
		# value = checkMaskValue mask url
		# ui = UIEditString defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value}
		= (NormalEditor [(ui,editorAttributes vv meta)],vst)

gUpdate{|URL|} target upd val iworld = basicUpdate (\json url -> Just (maybe url (\s -> URL s) (fromJSON json))) target upd val iworld

gVerify{|URL|} mv options = simpleVerify mv options

gEditMeta{|URL|} _ = [{label=Nothing,hint=Just "Enter a uniform resource locator (URL)",unit=Nothing}]

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
JSONEncode{|Note|} _ (Note txt) = [JSONString txt]

JSONDecode{|Note|} _ [JSONString txt:c] = (Just (Note txt),c)
JSONDecode{|Note|} _ c = (Nothing,c)

gText{|Note|}	_ val	    = [maybe "" toString val]

gEditor{|Note|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(setMargins 5 5 5 5 (UIViewHtml defaultSizeOpts {UIViewOpts|value = fmap noteToHtml val}),'DM'.newMap)],vst)
	| otherwise	
		# value = checkMaskValue mask ((\(Note v)  -> v) val)
		= (NormalEditor [(UIEditNote sizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes vv meta)],vst)
where	
	sizeOpts = {UISizeOpts|defaultSizeOpts & height = Just FlexSize, minHeight = Just WrapBound}
	
	// THIS IS A HACK!
	// The encoding of a Text constructor should escape newlines and convert them to <br> tags. Unfortunately it doesn't
	noteToHtml (Note s)	//TODO: Fix this in the toString of the Text constructor of HtmlTag type
		= case split "\n" s of
			[line]	= Text line
			lines	= SpanTag [] ('DL'.intersperse (BrTag []) (map Text lines))

gUpdate{|Note|} target upd val iworld = basicUpdateSimple target upd val iworld

gVerify{|Note|} mv options = simpleVerify mv options
gEditMeta{|Note|} _ = [{label=Nothing,hint=Just "You may enter multiple lines of text",unit=Nothing}]


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
JSONEncode{|CleanCode|} _ (CleanCode txt) = [JSONString txt]

JSONDecode{|CleanCode|} _ [JSONString txt:c] = (Just (CleanCode txt),c)
JSONDecode{|CleanCode|} _ c = (Nothing,c)

gText{|CleanCode|}		_ val		= [maybe "" toString val]

gVerify{|CleanCode|} mv options = simpleVerify mv options
gEditMeta{|CleanCode|} _ = [{label=Nothing,hint=Just "Enter a piece of Clean code",unit=Nothing}]

derive gEditor  CleanCode
derive gUpdate  CleanCode
derive gDefault	CleanCode
derive gEq		CleanCode

instance toString CleanCode
where
	toString (CleanCode s) = s

//* Money (ISO4217 currency codes are used)

gText{|EUR|} _ val = [maybe "" toString val]

gEditor{|EUR|}	dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled	
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap (\(EUR v) -> toString v) val},'DM'.newMap)],vst)
	| otherwise
		# value = checkMaskValue mask ((\(EUR v) -> toReal v / 100.0) val)
		= (NormalEditor [(UIEditDecimal defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes vv meta)],vst)

gUpdate{|EUR|} target upd val iworld = basicUpdateSimple target upd val iworld

gVerify{|EUR|} mv options = simpleVerify mv options
gEditMeta{|EUR|} _ = [{label=Nothing,hint=Just "Enter an amount in EUR",unit=Just (Left "&euro;")}]

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

gText{|USD|} _ val = [maybe "" toString val]

gEditor{|USD|}	dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled	
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},'DM'.newMap)],vst)
	| otherwise
		# value = checkMaskValue mask ((\(USD v) -> toReal v / 100.0) val)
		= (NormalEditor [(UIEditDecimal defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes vv meta)],vst)

gUpdate{|USD|} target upd val iworld = basicUpdateSimple target upd val iworld

gVerify{|USD|} mv options = simpleVerify mv options
gEditMeta{|USD|} _ = [{label=Nothing,hint=Just "Enter an amount in USD",unit=Just (Left "$")}]

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

JSONEncode{|Date|} _ d		= [JSONString (toString d)]

JSONDecode{|Date|} _ [JSONString s:c] | isDateFormat s	= (Just (fromString s), c)
JSONDecode{|Date|} _ c									= (Nothing, c)
isDateFormat s = size s == 10 && foldl (\ok i -> ok && (if (i == 4 || i == 7) (s.[i] == '-') (isDigit s.[i]))) True [0..9]

gText{|Date|} _ val = [maybe "" toString val]

gEditor{|Date|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},'DM'.newMap)],vst)
	| otherwise
		# value	= checkMaskValue mask val
		= (NormalEditor [(UIEditDate defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes vv meta)],vst)

gDefault{|Date|} = {Date|day = 1, mon = 1, year = 1970}
gUpdate{|Date|} target upd val iworld = basicUpdate (\json old -> fromJSON json) target upd val iworld
gVerify{|Date|} mv options = simpleVerify mv options
gEditMeta{|Date|} _ = [{label=Nothing,hint=Just "Enter a date (yyyy-mm-dd)",unit=Nothing}]

derive gEq			Date

instance toString Date
where
	toString {Date|year,mon,day}	= lpad (toString year) 4 '0' +++ "-" +++ lpad (toString mon) 2 '0' +++ "-" +++ lpad (toString day) 2 '0'

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
			| date.Date.day <= monthLength
				= date
				= normDays (normMonths {Date|date & mon = date.Date.mon + 1, day = date.Date.day - monthLength})

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

JSONEncode{|Time|} _ t					= [JSONString (toString t)]
JSONDecode{|Time|} _ [JSONString s:c]	| isTimeFormat s	= (Just (fromString s), c)
JSONDecode{|Time|} _ c									    = (Nothing, c)
isTimeFormat s = size s == 8 && foldl (\ok i -> ok && (if (i == 2 || i == 5) (s.[i] == ':') (isDigit s.[i]))) True [0..7]

gText{|Time|} _ val = [maybe "" toString val]

gEditor{|Time|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = fmap toString val},'DM'.newMap)],vst)
	| otherwise
		# value = checkMaskValue mask val
		= (NormalEditor [(UIEditTime defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes vv meta)],vst)

gUpdate{|Time|} target upd val iworld = basicUpdate (\json old -> fromJSON json) target upd val iworld

gVerify{|Time|} mv options = simpleVerify mv options
gEditMeta{|Time|} _ = [{label=Nothing,hint=Just "Enter a time (hh:mm:ss)",unit=Nothing}]

derive gDefault		Time
derive gEq			Time

instance toString Time
where
	toString {Time|hour,min,sec}	= lpad (toString hour) 2 '0' +++ ":" +++ lpad (toString min) 2 '0' +++ ":" +++ lpad (toString sec) 2 '0'

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

JSONEncode{|DateTime|} _ dt	= [JSONString (toString dt)]

JSONDecode{|DateTime|} _ [JSONString s:c]	= (Just (fromString s), c)
JSONDecode{|DateTime|} _ c				= (Nothing, c)

derive gDefault			DateTime
derive gEq				DateTime


gText{|DateTime|} AsHeader _ = [""]
gText{|DateTime|} _ (Just (DateTime date time))
	= [toSingleLineText date +++" "+++ toSingleLineText time]

gEditor{|DateTime|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value=fmap toString val},'DM'.newMap)],vst)
	| otherwise
		# value = checkMaskValue mask val
		= (NormalEditor [(UIEditDateTime defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes vv meta)],vst)

gUpdate{|DateTime|} target upd val iworld = basicUpdate (\json old -> fromJSON json) target upd val iworld
gVerify{|DateTime|} mv options = simpleVerify mv options
gEditMeta{|DateTime|} _ = [{label=Nothing,hint=Just "Enter a date and time (yyyy-mm-dd hh:mm:ss)",unit=Nothing}]

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
			| otherwise	= DateTime (dn + {Date|year = 0, mon = 0, day = 1}) tn	//We've passed midnight
	where
		dn = dx + dy
		tn = tx + ty
		
instance - DateTime
where
	(-) (DateTime dx tx) (DateTime dy ty)
        | tn <= tx   = DateTime dn tn
        | otherwise  = DateTime (dn - {Date|year=0,mon=0,day=1}) tn //We've passed midnight
    where
        dn = dx - dy
        tn = tx - ty

instance == DateTime
where
	(==) (DateTime dx tx) (DateTime dy ty)	= dx == dy && tx == ty

instance < DateTime
where
	(<) (DateTime dx tx) (DateTime dy ty)
		| dx < dy	= True
		| dx == dy	= (tx < ty)
		| otherwise	= False
		
paddedDateTimeString :: DateTime -> String
paddedDateTimeString (DateTime {Date|year,mon,day} {Time|hour,min,sec})
	=   lpad (toString year) 4 '0' +++ lpad (toString mon) 2 '0' +++ lpad (toString day) 2 '0'
	+++ lpad (toString hour) 2 '0' +++ lpad (toString min) 2 '0' +++ lpad (toString sec) 2 '0'

//* Documents
gText{|Document|} _ (Just val)
	| val.Document.size == 0			= ["No Document"]
	| otherwise							= [val.Document.name]
gText{|Document|} _ Nothing             = [""]

gEditor {|Document|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		# val = checkMask mask val
		= (NormalEditor [(UIViewDocument defaultHSizeOpts {UIViewOpts|value = val},'DM'.newMap)],vst)
	| otherwise
		# value = checkMaskValue mask val
		= (NormalEditor [(UIEditDocument defaultHSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=value},editorAttributes vv meta)],vst)

gUpdate {|Document|} [] upd (val,mask) iworld = case fromJSON upd of 
	Nothing		= (({Document|documentId = "", contentUrl = "", name="", mime="", size = 0},Blanked),iworld)// Reset
	Just doc	= ((doc,Touched),iworld) //Update

gVerify{|Document|} mv options = simpleVerify mv options
gEditMeta{|Document|} _ = [{label=Nothing,hint=Just "Upload a document",unit=Nothing}]

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


//* A sliding scale
gText{|Scale|}	_ (Just {Scale|cur}) = [toString cur]
gText{|Scale|}	_ _                  = [""]

gEditor{|Scale|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	# sliderOpts	= {UISliderOpts|minValue=val.Scale.min, maxValue=val.Scale.max, value = val.cur}
	| disabled
		# val = checkMask mask val							
		# viewOpts = {UIViewOpts|value = fmap curVal val}  
		= (NormalEditor [(UIViewSlider defaultHSizeOpts viewOpts sliderOpts, 'DM'.newMap)],vst)
	| otherwise
		# value = checkMaskValue mask (curVal val)
		# editOpts = {UIEditOpts|taskId = taskId, editorId = editorId dp, value = value}
		= (NormalEditor [(UIEditSlider defaultHSizeOpts editOpts sliderOpts, editorAttributes vv meta)],vst)
where
	curVal {Scale|cur} = cur

gUpdate{|Scale|} target upd val iworld
	= basicUpdate (\json i -> Just (maybe i (\cur -> {Scale|i & cur = cur}) (fromJSON json))) target upd val iworld

gVerify{|Scale|} _ mv = alwaysValid mv

gDefault{|Scale|} = {Scale|min=1,cur=3,max=5}
gEditMeta{|Scale|} _	= [{label=Nothing,hint=Just "You can change the value by sliding the scale",unit=Nothing}]

//* Progress bars
gText{|Progress|}	_ val  = [maybe "" (\{Progress|description} -> description) val]

gEditor{|Progress|} dp (val,mask,ver) meta vst=:{VSt|taskId}
	= (NormalEditor [(UIViewProgress defaultHSizeOpts {UIViewOpts|value=Just (value val)} {UIProgressOpts|text = text val},'DM'.newMap)],vst)
where
	text {Progress|description}	= description
		
	value {Progress|progress=ProgressRatio ratio} 
		| ratio < 0.0	= ProgressRatio 0.0
		| ratio > 1.0	= ProgressRatio 1.0
						= ProgressRatio ratio
	value {Progress|progress} = progress

gUpdate{|Progress|}	target upd val iworld = (val,iworld)
gVerify{|Progress|} _ mv = alwaysValid mv
gEditMeta{|Progress|} _		= [{label=Nothing,hint=Nothing,unit=Nothing}]

derive gDefault			Progress

gText{|ProgressAmount|} _ (Just ProgressUndetermined)	= ["Undetermined"]
gText{|ProgressAmount|} _ (Just (ProgressRatio r))		= [toString (entier (100.0 * r)) + "%"]
gText{|ProgressAmount|} _ _		                    = [""]

derive gDefault			ProgressAmount
derive gEditor 			ProgressAmount
derive gEditMeta		ProgressAmount
derive gUpdate			ProgressAmount
derive gVerify			ProgressAmount

//* Inclusion of external html files
gText{|HtmlInclude|}	_ (Just (HtmlInclude location))	= ["<External html: " + location + ">"]
gText{|HtmlInclude|}	_ _	                            = [""]

gEditor{|HtmlInclude|} dp vv=:(HtmlInclude path,mask,ver) meta vst
	= (NormalEditor [(UIViewHtml defaultSizeOpts {UIViewOpts|value=Just (IframeTag [SrcAttr path] [])},editorAttributes vv meta)],vst)

gUpdate{|HtmlInclude|} target upd val iworld = (val,iworld)

gVerify{|HtmlInclude|} _ mv = alwaysValid mv

derive gDefault HtmlInclude
derive gEditMeta HtmlInclude

//* Form buttons
gText{|FormButton|}	_ val = [maybe "" (\v -> v.FormButton.label) val]

gEditor{|FormButton|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	# text = Just val.FormButton.label
	# iconCls = Just val.FormButton.icon
	= (NormalEditor [(UIEditButton defaultSizeOpts {UIEditOpts|taskId=taskId,editorId=editorId dp,value=Just (JSONString "pressed")} {UIButtonOpts|text=text,iconCls=iconCls,disabled=False},editorAttributes vv meta)],vst)

gUpdate{|FormButton|} target upd val iworld
	= basicUpdate (\st b -> Just {FormButton|b & state = st}) target upd val iworld

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

gText{|ButtonState|}	_ (Just NotPressed)	= ["not pressed"]
gText{|ButtonState|}	_ (Just Pressed)	= ["pressed"]
gText{|ButtonState|}	_ _                 = [""]

derive gDefault		ButtonState
derive gEditor		ButtonState
derive gEditMeta	ButtonState
derive gUpdate		ButtonState
derive gVerify		ButtonState

//* Table consisting of headers, the displayed data cells & possibly a selection
gText{|Table|}	_ _	= ["<Table>"]

gEditor{|Table|} dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	= (NormalEditor [(UIGrid defaultSizeOpts
        {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=value val,options = options val}
        {UIGridOpts|columns = columns val,doubleClickAction=Nothing},editorAttributes vv meta)],vst)
where
	value (Table _ _ mbSel)	= maybe [] (\s->[s]) mbSel
	columns (Table headers _ _)	= headers
	options (Table _ cells _)	= map (map toString) cells
	
gUpdate{|Table|} target upd val iworld
	= basicUpdate (\json (Table headers cells _) -> case fromJSON json of Just i = Just (Table headers cells (Just i)); _ = Just (Table headers cells Nothing)) target upd val iworld

gVerify{|Table|} _ mv = alwaysValid mv
gDefault{|Table|} = Table [] [] Nothing

derive gEditMeta Table

toTable	:: ![a] -> Table | gText{|*|} a
toTable a = Table (headers a Nothing) (map row a) Nothing
where
	headers:: [a] (Maybe a) -> [String] | gText{|*|} a
	headers _ v = gText{|*|} AsHeader v

	row x =  [Text cell \\ cell <- gText{|*|} AsRow (Just x)]
	
//* Simple tree type (used primarily for creating trees to choose from)
derive gDefault			ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gText	        ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEditor	        ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEditMeta		ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gUpdate			ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gVerify			ChoiceTree, ChoiceTreeValue, ChoiceTreeType
		
instance Functor ChoiceTree
where
    fmap f t=:{ChoiceTree|label,type}
        = {ChoiceTree|t & label = f label, type = fmap f type}

instance Functor ChoiceTreeType
where
    fmap f LeafNode = LeafNode
    fmap f (CollapsedNode c) = CollapsedNode [fmap f x \\ x <- c]
    fmap f (ExpandedNode c) = ExpandedNode [fmap f x \\ x <- c]

derive JSONEncode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive JSONDecode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType
derive gEq				Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table, ChoiceTree, ChoiceTreeValue, ChoiceTreeType

//* Choices
gDefault{|ComboChoice|} _ = ComboChoice [] Nothing
gText{|ComboChoice|} fv mode (Just val) = fromMaybe ["No item selected"] (fmap (\v -> fv mode (Just v)) (getSelectionView val))
gText{|ComboChoice|} fv mode _          = [""]

gEditor{|ComboChoice|} fx gx _ hx _ _ dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},'DM'.newMap)],vst)
	| otherwise
		= (NormalEditor [(UIDropdown defaultHSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},editorAttributes vv (gEditMeta{|*->*|} hx val))],vst)
where
	vvalue (ComboChoice options (Just sel))	= Just (hd (gx AsSingleLine (Just (options !! sel))))
	vvalue _								= Nothing
	evalue (ComboChoice _ mbSel)			= maybe [] (\s->[s]) mbSel

	options (ComboChoice options _)			= [concat (gx AsSingleLine (Just v)) \\ v <- options]

gUpdate{|ComboChoice|} _ _ _ _ target upd val iworld = updateChoice (\idx (ComboChoice options _) -> ComboChoice options (Just idx)) target upd val iworld

gVerify{|ComboChoice|} _ mv options = customVerify (\(ComboChoice _ s) -> isJust s) (const "You must choose one item") mv options

instance Choice ComboChoice
where
	getSelectionView (ComboChoice options mbSel)				= getListOption options mbSel
	setSelectionView mbSel (ComboChoice options _)				= ComboChoice options (maybe Nothing (getListIndex options) mbSel)
    getSelectionIndex (ComboChoice _ mbSel)                     = mbSel
    setSelectionIndex mbSel (ComboChoice options _)             = ComboChoice options mbSel


gDefault{|RadioChoice|} _ = RadioChoice [] Nothing
gText{|RadioChoice|} fv mode (Just val) = fromMaybe ["No item selected"] (fmap (\v -> fv mode (Just v)) (getSelectionView val))
gText{|RadioChoice|} fv _ _ = [""]

gEditor{|RadioChoice|} _ gx _ hx _ _ dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},'DM'.newMap)],vst)
	| otherwise
		= (NormalEditor [(UIRadioGroup defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},editorAttributes vv (gEditMeta{|*->*|} hx val))],vst)
where
	vvalue (RadioChoice options (Just sel))	= Just (hd (gx AsSingleLine (Just (options !! sel))))
	vvalue _								= Nothing
	evalue (RadioChoice _ mbSel)			= maybe [] (\i -> [i]) mbSel
	
	options (RadioChoice options _)			= [concat (gx AsSingleLine (Just v)) \\ v <- options]


gUpdate{|RadioChoice|} _ _ _ _ target upd val iworld
	= updateChoice (\idx (RadioChoice options _) -> RadioChoice options (Just idx)) target upd val iworld

gVerify{|RadioChoice|} _ mv options = simpleVerify mv options

instance Choice RadioChoice
where
	getSelectionView (RadioChoice options mbSel)				= getListOption options mbSel
	setSelectionView mbSel (RadioChoice options _)				= RadioChoice options (maybe Nothing (getListIndex options) mbSel)
    getSelectionIndex (RadioChoice _ mbSel)                     = mbSel
    setSelectionIndex mbSel (RadioChoice options _)             = RadioChoice options mbSel

gDefault{|ListChoice|} _ = ListChoice [] Nothing
gText{|ListChoice|} fv mode (Just val) = fromMaybe ["No item selected"] (fmap (\v -> fv mode (Just v)) (getSelectionView val))
gText{|ListChoice|} fv _ _ = [""]

gEditor{|ListChoice|} _ gx _ hx _ _ dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = vvalue val},'DM'.newMap)],vst)
	| otherwise
		= (NormalEditor [(UIListChoice defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},'DM'.newMap/*editorAttributes vv (gEditMeta{|*->*|} hx val)*/)],vst)
where
	vvalue (ListChoice options (Just sel))	= Just (hd (gx AsSingleLine (Just (options !! sel))))
	vvalue _								= Nothing
	evalue (ListChoice _ mbSel)			= maybe [] (\i -> [i]) mbSel
	
	options (ListChoice options _)			= [concat (gx AsSingleLine (Just v)) \\ v <- options]


gUpdate{|ListChoice|} _ _ _ _ target upd val iworld
	= updateChoice (\idx (ListChoice options _) -> ListChoice options (Just idx)) target upd val iworld

gVerify{|ListChoice|} _ mv options = simpleVerify mv options

instance Choice ListChoice
where
	getSelectionView (ListChoice options mbSel)			= getListOption options mbSel
	setSelectionView mbSel (ListChoice options _)		= ListChoice options (maybe Nothing (getListIndex options) mbSel)
    getSelectionIndex (ListChoice _ mbSel)              = mbSel
    setSelectionIndex mbSel (ListChoice options _)      = ListChoice options mbSel

gDefault{|TreeChoice|} _ = TreeChoice [] Nothing

gText{|TreeChoice|} fv mode (Just val) = fromMaybe ["No item selected"] (fmap (\v -> fv mode (Just v)) (getSelectionView val))
gText{|TreeChoice|} fv _ _ = [""]

gEditor{|TreeChoice|} _ gx _ _ hx _ dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	# viz		= [(UITree defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=value val,options = options gx val mask} {UITreeOpts|doubleClickAction=Nothing}, 'DM'.newMap/*editorAttributes vv (gEditMeta{|*->*|} hx val)*/)]
	= (NormalEditor viz,vst)
where
	value  (TreeChoice _ mbSel) 	= maybe [] (\s->[s]) mbSel
	
	options vizLabel (TreeChoice nodes _) msk = fst (mkTree vizLabel nodes 0)
	where
        mkTree vizLabel [] idx = ([],idx)
        mkTree vizLabel [{ChoiceTree|label,icon,type}:r] idx
            # (children,expanded,idx`)  = case type of
                LeafNode = (Nothing,False,inc idx)
                CollapsedNode nodes
                    # (tree,idx`) = mkTree vizLabel nodes (inc idx)
                    = (Just tree,False,idx`)
                ExpandedNode nodes
                    # (tree,idx`) = mkTree vizLabel nodes (inc idx)
                    = (Just tree,True,idx`)
            # (rtree,idx`) = mkTree vizLabel r idx`
            = ([{UITreeNode|text=concat (vizLabel AsSingleLine (Just label)),iconCls=fmap (\i ->"icon-"+++i) icon,value=idx,leaf=isNothing children
                ,expanded = expanded, children=children}:rtree],idx`)

	options _ _ _ = []

gUpdate{|TreeChoice|} _ _ _ _ [] upd (TreeChoice tree sel,mask) iworld = case fromJSON upd of
	Just ("sel",idx,val)	= ((TreeChoice tree (if val (Just idx) Nothing), touch mask),iworld)
	Just ("exp",idx,val)	= ((TreeChoice (setTreeExpanded idx val tree) sel,touch mask),iworld)
	_						= (((TreeChoice tree sel), mask),iworld)

gUpdate{|TreeChoice|} _ _ _ _ target upd val iworld = (val,iworld)

gVerify{|TreeChoice|} _ mv options = simpleVerify mv options

instance Choice TreeChoice
where
	getSelectionView (TreeChoice options mbSel)		= getTreeOption options mbSel
	setSelectionView mbSel (TreeChoice options _)	= TreeChoice options (findTreeView options mbSel)
    getSelectionIndex (TreeChoice options mbSel)    = getTreeIndex options mbSel
    setSelectionIndex mbSel (TreeChoice options _)  = TreeChoice options (findTreeIndex options mbSel)

gDefault{|GridChoice|} _ = GridChoice [] Nothing

gText{|GridChoice|} fv mode (Just val) = fromMaybe ["No item selected"] (fmap (\v -> fv mode (Just v)) (getSelectionView val))	
gText{|GridChoice|} fv _ _ = [""]

gEditor{|GridChoice|} _ gx _ hx _ _ dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	= (NormalEditor [(UIGrid defaultSizeOpts
		{UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=value val,options = options val}
		{UIGridOpts|columns = columns, doubleClickAction=Nothing},'DM'.newMap)],vst)
where	
	value (GridChoice options mbSel)	= maybe [] (\s->[s]) mbSel
	options (GridChoice options _)		= [gx AsRow (Just opt) \\ opt <- options]
    columns = gx AsHeader Nothing

gUpdate{|GridChoice|} _ _ _ _ target upd val iworld
	= updateChoice (\idxs (GridChoice options _) -> GridChoice options (case idxs of [idx:_] = (Just idx); _ = Nothing)) target upd val iworld

gVerify{|GridChoice|} _ _ mv = alwaysValid mv

instance Choice GridChoice
where
	getSelectionView (GridChoice options mbSel)		= getListOption options mbSel
	setSelectionView mbSel (GridChoice options _)	= GridChoice options (maybe Nothing (getListIndex options) mbSel)
    getSelectionIndex (GridChoice _ mbSel)          = mbSel
    setSelectionIndex mbSel (GridChoice options _)  = GridChoice options mbSel

gDefault{|DynamicChoice|} fx = DCRadio (gDefault{|*->*|} fx )

gText{|DynamicChoice|}		fv mode (Just (DCRadio val))	= gText{|*->*|} fv mode (Just val)
gText{|DynamicChoice|}		fv mode (Just (DCList val))	    = gText{|*->*|} fv mode (Just val)
gText{|DynamicChoice|}		fv mode (Just (DCCombo val))	= gText{|*->*|} fv mode (Just val)
gText{|DynamicChoice|}		fv mode (Just (DCGrid val))	    = gText{|*->*|} fv mode (Just val)
gText{|DynamicChoice|}		fv mode (Just (DCTree val))	    = gText{|*->*|} fv mode (Just val)
gText{|DynamicChoice|}		fv _ _	        = [""]

gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 dp (DCCombo val,mask,ver) meta vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) meta vst
gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 dp (DCRadio val,mask,ver) meta vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) meta vst
gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 dp (DCList val,mask,ver) meta vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) meta vst
gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 dp (DCTree val,mask,ver) meta vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) meta vst
gEditor{|DynamicChoice|} f1 f2 f3 f4 f5 f6 dp (DCGrid val,mask,ver) meta vst
	= gEditor{|*->*|} f1 f2 f3 f4 f5 f6 dp (val,mask,ver) meta vst

gUpdate{|DynamicChoice|} gUpdx gDefx jEncx jDecx target upd	(DCCombo val,mask) iworld	= appFst (appFst DCCombo) (gUpdate{|*->*|} gUpdx gDefx jEncx jDecx target upd (val,mask) iworld)
gUpdate{|DynamicChoice|} gUpdx gDefx jEncx jDecx target upd	(DCRadio val,mask) iworld	= appFst (appFst DCRadio) (gUpdate{|*->*|} gUpdx gDefx jEncx jDecx target upd (val,mask) iworld)
gUpdate{|DynamicChoice|} gUpdx gDefx jEncx jDecx target upd	(DCList val,mask) iworld	= appFst (appFst DCList) (gUpdate{|*->*|} gUpdx gDefx jEncx jDecx target upd (val,mask) iworld)
gUpdate{|DynamicChoice|} gUpdx gDefx jEncx jDecx target upd	(DCTree val,mask) iworld	= appFst (appFst DCTree) (gUpdate{|*->*|} gUpdx gDefx jEncx jDecx target upd (val,mask) iworld)
gUpdate{|DynamicChoice|} gUpdx gDefx jEncx jDecx target upd	(DCGrid val,mask) iworld	= appFst (appFst DCGrid) (gUpdate{|*->*|} gUpdx gDefx jEncx jDecx target upd (val,mask) iworld)

gVerify{|DynamicChoice|} fx options (DCCombo v,mask) = gVerify{|*->*|} fx options (v,mask)
gVerify{|DynamicChoice|} fx options (DCRadio v,mask) = gVerify{|*->*|} fx options (v,mask)
gVerify{|DynamicChoice|} fx options (DCList v,mask) = gVerify{|*->*|} fx options (v,mask)
gVerify{|DynamicChoice|} fx options (DCTree v,mask) = gVerify{|*->*|} fx options (v,mask)
gVerify{|DynamicChoice|} fx options (DCGrid v,mask) = gVerify{|*->*|} fx options (v,mask)
	
instance Choice DynamicChoice
where
	getSelectionView (DCCombo choice)		    = getSelectionView choice
	getSelectionView (DCRadio choice)		    = getSelectionView choice
	getSelectionView (DCList choice)		    = getSelectionView choice
	getSelectionView (DCTree choice)		    = getSelectionView choice
	getSelectionView (DCGrid choice)		    = getSelectionView choice

	setSelectionView mbSel (DCCombo choice)		= DCCombo (setSelectionView mbSel choice)
	setSelectionView mbSel (DCRadio choice)		= DCRadio (setSelectionView mbSel choice)
	setSelectionView mbSel (DCList choice)		= DCList (setSelectionView mbSel choice)
	setSelectionView mbSel (DCTree choice)		= DCTree (setSelectionView mbSel choice)
	setSelectionView mbSel (DCGrid choice)		= DCGrid (setSelectionView mbSel choice)

    getSelectionIndex (DCCombo choice)          = getSelectionIndex choice
    getSelectionIndex (DCRadio choice)          = getSelectionIndex choice
    getSelectionIndex (DCList choice)           = getSelectionIndex choice
    getSelectionIndex (DCTree choice)           = getSelectionIndex choice
    getSelectionIndex (DCGrid choice)           = getSelectionIndex choice

    setSelectionIndex mbSel (DCCombo choice)    = DCCombo (setSelectionIndex mbSel choice)
    setSelectionIndex mbSel (DCRadio choice)    = DCRadio (setSelectionIndex mbSel choice)
    setSelectionIndex mbSel (DCList choice)     = DCList (setSelectionIndex mbSel choice)
    setSelectionIndex mbSel (DCTree choice)     = DCTree (setSelectionIndex mbSel choice)
    setSelectionIndex mbSel (DCGrid choice)     = DCGrid (setSelectionIndex mbSel choice)

gDefault{|CheckMultiChoice|} _ _ = CheckMultiChoice [] []

gText{|CheckMultiChoice|} fv _ _ (Just val) = gText{|* -> *|} fv AsSingleLine (Just (getSelectionViews val))
gText{|CheckMultiChoice|} fv _ _ _ = [""]

gEditor{|CheckMultiChoice|} _ gx _ hx _ _ _ _ _ hy _ _ dp vv=:(val,mask,ver) meta vst=:{VSt|taskId,disabled}
	| disabled
		= (NormalEditor [(UIViewString defaultSizeOpts {UIViewOpts|value = Just (vvalue val)},'DM'.newMap)],vst)
	| otherwise
		= (NormalEditor [(UICheckboxGroup defaultSizeOpts {UIChoiceOpts|taskId=taskId,editorId=editorId dp,value=evalue val,options=options val},editorAttributes vv (gEditMeta{|*->*->*|} hx hy val))],vst)
where
	vvalue (CheckMultiChoice options sel)	= join "," ([hd (gx AsSingleLine (Just (fst (options !! i )))) \\ i <- sel])
	evalue (CheckMultiChoice _ sel)			= sel
	options (CheckMultiChoice options _)	= [concat (gx AsSingleLine (Just v)) \\ (v,_) <- options]

gUpdate{|CheckMultiChoice|} _ _ _ _ _ _ _ _ target upd val iworld = basicUpdate (\json (CheckMultiChoice opts sel) -> case fromJSON json of Just (i,v) = Just (CheckMultiChoice opts (updateSel i v sel)); _ = (Just (CheckMultiChoice opts sel))) target upd val iworld
where
	updateSel i True sel	= removeDup [i:sel]
	updateSel i False sel 	= removeMember i sel

gVerify{|CheckMultiChoice|} _ _	vm options = simpleVerify vm options
	
instance MultiChoice CheckMultiChoice
where
	selectOptions newSels (CheckMultiChoice options _)			= CheckMultiChoice options (setListOptions id options newSels)
	getSelections (CheckMultiChoice options sels)				= fmap snd (getListOptions options sels)
	getSelectionViews (CheckMultiChoice options sels)			= fmap fst (getListOptions options sels)

// Utility functions for Choice and MultiChoice instances
touch (TouchedUnparsed r)	= TouchedUnparsed r
touch (TouchedWithState s)	= TouchedWithState s
touch (CompoundMask c)	    = CompoundMask c
touch _						= Touched

setTreeExpanded :: Int Bool [ChoiceTree a] -> [ChoiceTree a]
setTreeExpanded idx expanded tree = snd (expand idx tree)
where
    expand idx [] = (Just idx,[])
    expand idx [t=:{ChoiceTree|type=LeafNode}:ts]
        | idx == 0 = (Nothing,[t:ts])
        # (mbIdx,ts) = expand (idx - 1) ts
        = (mbIdx,[t:ts])
    expand idx [t=:{ChoiceTree|type=CollapsedNode children}:ts]
        | idx == 0  = (Nothing,[if expanded {ChoiceTree|t&type=ExpandedNode children} {ChoiceTree|t&type=CollapsedNode children}:ts])
        | otherwise = case expand (idx - 1) children of
            (Nothing,children) //Found in children
                = (Nothing,[{ChoiceTree|t & type = CollapsedNode children}:ts])
            (Just idx,children) //Not found, keep looking
                # (mbIdx,ts) = expand idx ts
                = (mbIdx,[t:ts])
    expand idx [t=:{ChoiceTree|type=ExpandedNode children}:ts] 
        | idx == 0  = (Nothing,[if expanded {ChoiceTree|t&type=ExpandedNode children} {ChoiceTree|t&type=CollapsedNode children}:ts])
        | otherwise = case expand (idx - 1) children of
            (Nothing,children) //Found in children
                = (Nothing,[{ChoiceTree|t & type = ExpandedNode children}:ts])
            (Just idx,children) //Not found, keep looking
                # (mbIdx,ts) = expand idx ts
                = (mbIdx,[t:ts])

updateChoice select target upd val
    = basicUpdate (\json choice -> Just (maybe choice (\i -> select i choice) (fromJSON json))) target upd val

setListOption :: !(o -> s) ![(v,o)] !s -> (Maybe Int) | gEq{|*|} s
setListOption targetFun options newSel
	= case setListOptions targetFun options [newSel] of
		[idx:_]	= Just idx
		_		= Nothing

setListOptions :: !(o -> s) ![(v,o)] ![s] -> [Int] | gEq{|*|} s
setListOptions targetFun options sels
	= [idx \\ (_,option) <- options & idx <- [0..] | gIsMember (targetFun option) sels]
where
	gIsMember x [hd:tl]	= hd===x || gIsMember x tl
	gIsMember x []		= False
	
getListOption :: ![a] !(Maybe Int) -> Maybe a
getListOption options mbSel = case getListOptions options (maybeToList mbSel) of
	[a] = Just a
	_	= Nothing

getListIndex :: ![a] a -> Maybe Int | gEq{|*|} a
getListIndex options sel = case [idx \\ opt <- options & idx <- [0..] | opt === sel] of
    [idx:_] = Just idx
    _       = Nothing

getListOptions :: ![a] ![Int] -> [a]
getListOptions options sels = [opt \\ opt <- options & idx <- [0..] | isMember idx sels]

getTreeOption :: ![ChoiceTree a] !(Maybe Int) -> Maybe a
getTreeOption tree mbSel = case getListOption (treeToList tree) mbSel of
    Nothing  = Nothing
    Just mba = mba

getTreeIndex :: ![ChoiceTree a] !(Maybe Int) -> Maybe Int
getTreeIndex tree mbSel = maybe Nothing (\sel -> (indices tree !! sel)) mbSel
where
    indices [] = []
    indices [c=:{ChoiceTree|value,type}:cs] = [toMaybe value] ++ indices (choiceTreeChildren c) ++ indices cs

    toMaybe (ChoiceNode i)  = Just i
    toMaybe _               = Nothing

findTreeIndex :: ![ChoiceTree a] !(Maybe Int) -> Maybe Int
findTreeIndex tree Nothing = Nothing
findTreeIndex tree (Just idx) = case find idx 0 tree of
    Left i  = Just i
    _       = Nothing
where
    find idx i [] = Right i
    find idx i [c=:{ChoiceTree|value,type}:cs]
        | isChoice idx value = Left i
        | otherwise = case find idx (i + 1) (choiceTreeChildren c) of
            Left i = Left i
            Right i` = find idx i` cs

    isChoice idx (ChoiceNode i) = idx == i
    isChoice idx _              = False

findTreeView :: ![ChoiceTree a] !(Maybe a) -> Maybe Int | gEq{|*|} a //TODO: Merge with findTreeIndex
findTreeView tree Nothing = Nothing
findTreeView tree (Just sel) = case find sel 0 tree of
    Left i  = Just i
    _       = Nothing
where
    find sel i [] = Right i
    find sel i [c=:{ChoiceTree|label}:cs]
        | label === sel = Left i
        | otherwise = case find sel (i + 1) (choiceTreeChildren c) of
            Left i = Left i
            Right i` = find sel i` cs

choiceTreeChildren :: (ChoiceTree v) -> [ChoiceTree v]
choiceTreeChildren {ChoiceTree|type=LeafNode} = []
choiceTreeChildren {ChoiceTree|type=CollapsedNode nodes} = nodes
choiceTreeChildren {ChoiceTree|type=ExpandedNode nodes} = nodes

ifExpanded :: ChoiceTreeValue [ChoiceTreeValue] [ChoiceTree v] -> ChoiceTreeType v
ifExpanded value expanded nodes
    | isMember value expanded   = ExpandedNode nodes
                                = CollapsedNode nodes

ifExpandedGroup :: String [ChoiceTreeValue] [ChoiceTree v] -> ChoiceTreeType v
ifExpandedGroup group expanded nodes = ifExpanded (GroupNode group) expanded nodes

ifExpandedChoice :: Int [ChoiceTreeValue] [ChoiceTree v] -> ChoiceTreeType v
ifExpandedChoice idx expanded nodes = ifExpanded (ChoiceNode idx) expanded nodes

instance == ChoiceTreeValue
where
    (==) (GroupNode x) (GroupNode y) = x == y
    (==) (ChoiceNode x) (ChoiceNode y) = x == y
    (==) _ _ = False

treeToList :: [ChoiceTree a] -> [Maybe a]
treeToList [] = []
treeToList [{ChoiceTree|label,type=LeafNode}:r] = [Just label:treeToList r]
treeToList [{ChoiceTree|label,type=CollapsedNode children}:r] = [Just label:treeToList children ++ treeToList r]
treeToList [{ChoiceTree|label,type=ExpandedNode children}:r] = [Just label:treeToList children ++ treeToList r]

derive JSONEncode		ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive JSONDecode		ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive gEq				ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice
derive gEditMeta		ComboChoice, RadioChoice, ListChoice, TreeChoice, GridChoice, DynamicChoice, CheckMultiChoice

//* Visualization wrappers
gText{|VisualizationHint|} fx mode (Just val) = case val of
	VHHidden x		= gText{|* -> *|} fx mode (Just (Hidden x))
	VHDisplay x		= gText{|* -> *|} fx mode (Just (Display x))
	VHEditable x	= gText{|* -> *|} fx mode (Just (Editable x))
gText{|VisualizationHint|} fx _ _ = [""]

gEditor{|VisualizationHint|} fx gx dx hx jex jdx dp (val,mask,ver) meta vst = case val of
	VHHidden x		= gEditor{|* -> *|} fx gx dx hx jex jdx dp (Hidden x,mask,ver) meta vst
	VHDisplay x		= gEditor{|* -> *|} fx gx dx hx jex jdx dp (Display x,mask,ver) meta vst
	VHEditable x	= gEditor{|* -> *|} fx gx dx hx jex jdx dp (Editable x,mask,ver) meta vst

gUpdate{|VisualizationHint|} 	gUpdx gDefx jEncx jDecx target upd val=:(VHEditable s,mask) iworld	= wrapperUpdate gUpdx fromVisualizationHint VHEditable target upd val iworld
gUpdate{|VisualizationHint|} 	gUpdx gDefx jEncx jDecx target upd val=:(VHDisplay s,mask) iworld	= wrapperUpdate gUpdx fromVisualizationHint VHDisplay target upd val iworld
gUpdate{|VisualizationHint|} 	gUpdx gDefx jEncx jDecx target upd val=:(VHHidden s,mask) iworld	= wrapperUpdate gUpdx fromVisualizationHint VHHidden target upd val iworld

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

gText{|Hidden|} _ _ _ = []

gEditor{|Hidden|} fx _ _ _ _ _ dp val meta vst = (HiddenEditor,vst)

gUpdate{|Hidden|} gUpdx gDefx jEncx jDecx target upd val iworld = wrapperUpdate gUpdx fromHidden Hidden target upd val iworld

gVerify{|Hidden|} fx options (Hidden v,mask) = fx options (v,mask)

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)

gText{|Display|} fx mode (Just (Display val))	= fx mode (Just val)
gText{|Display|} fx mode Nothing               = fx mode Nothing

gEditor{|Display|} fx _ _ _ _ _ dp (val,mask,ver) meta vst=:{VSt|disabled}
	# (def,vst) = fx dp (fromDisplay val,mask,ver) meta {VSt | vst &  disabled = True}
	= (def,{VSt | vst & disabled = disabled})

gUpdate{|Display|} gUpdx gDefx jEncx jDecx target upd val iworld = wrapperUpdate gUpdx fromDisplay Display target upd val iworld

gVerify{|Display|} fx options (Display d,mask) = verifyDisplay fx options (d,mask)

fromDisplay :: !(Display .a) -> .a
fromDisplay (Display a) = a

toDisplay :: !.a -> (Display .a)
toDisplay a = (Display a)

gText{|Editable|} fx mode (Just (Editable val))    = fx mode (Just val)
gText{|Editable|} fx mode Nothing                  = fx mode Nothing

gEditor{|Editable|} fx _ _ _ _ _ dp (val,mask,ver) meta vst=:{VSt|disabled}
	# (def,vst) = fx dp (fromEditable val,mask,ver) meta {VSt | vst & disabled = False}
	= (def,{VSt | vst & disabled = disabled})

gUpdate{|Editable|} gUpdx gDefx jEncx jDecx target upd val iworld = wrapperUpdate gUpdx fromEditable Editable target upd val iworld

gVerify{|Editable|} fx options (Editable e,mask) = verifyEditable fx options (e,mask)
	
fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

gText{|Row|} gVizx mode (Just (Row val)) = gVizx mode (Just val)
gText{|Row|} gVizx mode Nothing = gVizx mode Nothing

gEditor{|Row|} gEditx _ _ _ _ _ dp (Row val,mask,ver) meta vst
 = appFst (applyToControls (setDirection Horizontal)) (gEditx dp (val,mask,ver) meta vst)
gUpdate{|Row|} gUpdx gDefx jEncx jDecx target upd val iworld = wrapperUpdate gUpdx (\(Row x) -> x) Row target upd val iworld
gVerify{|Row|} gVerx options (Row x,mask) = gVerx options (x,mask)
	
gText{|Col|} gVizx mode (Just (Col val)) = gVizx mode (Just val)
gText{|Col|} gVizx mode Nothing = gVizx mode Nothing

gEditor{|Col|} gEditx _ _ _ _ _ dp (Col val,mask,ver) meta vst
 = appFst (applyToControls (setDirection Vertical)) (gEditx dp (val,mask,ver) meta vst)
gUpdate{|Col|} gUpdx gDefx jEncx jDecx target upd val iworld = wrapperUpdate gUpdx (\(Col x) -> x) Col target upd val iworld
gVerify{|Col|} gVerx options (Col x,mask) = gVerx options (x,mask)
	
//Utility for gEditor
applyToControls f (NormalEditor controls) = NormalEditor (map (appFst f) controls)
applyToControls f (OptionalEditor controls) = OptionalEditor (map (appFst f) controls)
applyToControls f def = def

//Utility for gUpdate
wrapperUpdate fx get set target upd (val,mask) iworld
	# ((w,mask),iworld) = fx target upd (get val,mask) iworld
	= ((set w,mask),iworld)

//Utility for gVerify	
verifyEditable fx options mv = fx {VerifyOptions|options & disabled = False} mv
verifyDisplay fx options mv = alwaysValid mv


derive JSONEncode		Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive JSONDecode		Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gDefault			Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gEq				Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gEditMeta		Hidden, Display, Editable, VisualizationHint, Row, Col
derive gText            EditableList, EditableListAdd

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

derive class iTask TaskListFilter

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

derive JSONEncode		TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger
derive JSONDecode		TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger
derive gDefault			TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger
derive gEq				TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger
derive gText	        TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger
derive gEditor			TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger
derive gEditMeta		TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger
derive gUpdate			TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger
derive gVerify			TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action, ActionOption, Hotkey, Trigger

derive class iTask TaskId, Config, ProcessStatus
	
instance toString Icon
where
	toString (Icon icon) = icon
	toString (IconView)	= "view"
	toString (IconEdit) = "edit"
	
instance descr ()
where
	toPrompt _ = 'DM'.newMap

instance descr Void
where
	toPrompt _ = 'DM'.newMap

instance descr String
where
	toPrompt hint = 'DM'.fromList [(HINT_ATTRIBUTE,hint)]
	
instance descr (!String,!String)
where
	toPrompt (title,hint) = 'DM'.fromList [(TITLE_ATTRIBUTE,title),(HINT_ATTRIBUTE,hint)]

instance descr (!Icon,!String,!String)
where
	toPrompt (icon,title,hint) = 'DM'.fromList [(ICON_ATTRIBUTE,toString icon),(TITLE_ATTRIBUTE,title),(HINT_ATTRIBUTE,hint)]

instance descr Title
where
	toPrompt (Title title) = 'DM'.fromList [(TITLE_ATTRIBUTE,title)]
	
instance descr Label
where
	toPrompt (Label label) = 'DM'.fromList [(LABEL_ATTRIBUTE,label)]

instance descr Hint
where
	toPrompt (Hint hint) = 'DM'.fromList [(HINT_ATTRIBUTE,hint)]
	
instance descr Icon
where
	toPrompt icon = 'DM'.fromList [(ICON_ATTRIBUTE,toString icon)]

instance descr Attribute
where
	toPrompt (Attribute k v) = 'DM'.fromList [(k,v)]
	
instance descr Att
where
	toPrompt (Att a) = toPrompt a
	
instance descr [d] | descr d
where
	toPrompt list = foldl mergeAttributes 'DM'.newMap (map toPrompt list)


derive JSONEncode		Icon
derive JSONDecode		Icon
derive gDefault			Icon
derive gEq				Icon
derive gText	        Icon
derive gEditMeta		Icon
derive gUpdate			Icon
derive gVerify			Icon

gEditor{|Icon|} _ (Icon icon,msk,ver) meta vst = (NormalEditor [(UIIcon defaultFSizeOpts {UIIconOpts|iconCls="icon-"+++icon,tooltip=Nothing} ,'DM'.newMap)], vst)

// Generic instances for common library types
derive JSONEncode		Either, MaybeError, HtmlTag, HtmlAttr
derive JSONDecode		Either, MaybeError, HtmlTag, HtmlAttr
derive gEq				Either, MaybeError, HtmlTag, HtmlAttr, Timestamp, JSONNode

gEq{|()|} _ _ = True
JSONEncode{|()|} _ () = [JSONNull]
JSONDecode{|()|} _ [JSONNull:c]		= (Just (), c)
JSONDecode{|()|} _ [JSONObject []:c]= (Just (), c)
JSONDecode{|()|} _ c				= (Nothing, c)

JSONEncode{|Timestamp|} _ (Timestamp t)	= [JSONInt t]
JSONDecode{|Timestamp|} _ [JSONInt t:c]	= (Just (Timestamp t), c)
JSONDecode{|Timestamp|} _ c				= (Nothing, c)

gEq{|Void|} _ _ = True
JSONEncode{|Void|} _ Void = [JSONNull]
JSONDecode{|Void|} _ [JSONNull:c]		= (Just Void, c)
JSONDecode{|Void|} _ [JSONObject []:c]= (Just Void, c)
JSONDecode{|Void|} _ c				= (Nothing, c)

gEq{|(->)|} _ _ fa fb		= copy_to_string fa == copy_to_string fb // HACK: Compare string representations of graphs functions are never equal
gEq{|Dynamic|} _ _			= False	// dynamics are never equal

derive JSONEncode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive JSONDecode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEq        SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan

gDefault{|{}|} _ = undef
gEditMeta{|{}|} _ _ = undef
gEditor{|{}|} _ _ _ _ _ _ _ _ _ _ = undef
gText{|{}|} _ _ _ = undef
gUpdate{|{}|} _ _ _ _ _ _ _ _ = undef
gVerify{|{}|} _ _ _ = undef

