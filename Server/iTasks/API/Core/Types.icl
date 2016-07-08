implementation module iTasks.API.Core.Types
from StdFunc import until, const, id, o
import StdInt, StdBool, StdClass, StdArray, StdEnum, StdTuple, StdMisc, StdList, StdOrdList
import GenLexOrd
import Data.Either, Data.Functor, Text.JSON, Text.HTML, Text, Text.Encodings.Base64, Data.Tuple, dynamic_string, System.File
from Data.Map import :: Map (..)
from Data.List import instance Functor []
import qualified Data.List as DL
import qualified Data.Map as DM
import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.Editor.Builtin, iTasks.UI.Editor.Combinators, iTasks.UI.Editor.Common
import iTasks.UI.Prompt
import iTasks._Framework.Generic.Visualization
import iTasks._Framework.Task, iTasks._Framework.TaskState, iTasks._Framework.Util
import iTasks._Framework.Serialization
import iTasks._Framework.IWorld

import System.Time, System.File, System.FilePath
import iTasks._Framework.SDS
from iTasks.UI.Definition import :: UI(..), :: UIDirection(..), stringDisplay
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

instance toString PhoneNumber
where
	toString (PhoneNumber num) = num

instance html PhoneNumber
where
	html (PhoneNumber num) = Text num

//* URL
gText{|URL|}	_ val	= [maybe "" toString val]

gEditor{|URL|} = whenDisabled
		(liftEditor (\(URL s) -> ATag [HrefAttr s] [Text s]) (\_ -> URL "") (htmlView 'DM'.newMap))
		(liftEditor (\(URL s) -> s) (\s -> URL s) (withHintAttributes "uniform resource locator (URL)" (textField 'DM'.newMap)))

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

gEditor{|Note|} = whenDisabled
		(liftEditor noteToHtml (\_ -> Note "") (htmlView 'DM'.newMap))
		(liftEditor (\(Note s) -> s) (\s -> Note s) (withHintAttributes "note" (textArea 'DM'.newMap)))
where
	// THIS IS A HACK!
	// The encoding of a Text constructor should escape newlines and convert them to <br> tags. Unfortunately it doesn't
	noteToHtml (Note s)	//TODO: Fix this in the toString of the Text constructor of HtmlTag type
		= case split "\n" s of
			[line]	= Text line
			lines	= SpanTag [] ('DL'.intersperse (BrTag []) (map Text lines))

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
	

//* Money (ISO4217 currency codes are used)
gText{|EUR|} _ val = [maybe "" toString val]

gEditor{|EUR|} = whenDisabled
		(liftEditor toString (\_ -> EUR 0) (textView 'DM'.newMap))
		(liftEditor (\(EUR v) -> toReal v / 100.0) (\v -> EUR (toInt (100.0 * v))) (withHintAttributes "amount in EUR" (decimalField 'DM'.newMap)))

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

gEditor{|USD|} = whenDisabled
		(liftEditor toString (\_ -> USD 0) (textView 'DM'.newMap))
		(liftEditor (\(USD v) -> toReal v / 100.0) (\v -> USD (toInt (100.0 * v))) (withHintAttributes "amount in USD" (decimalField 'DM'.newMap)))

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

gEditor{|Date|} = whenDisabled
		(liftEditor toString fromString (textView 'DM'.newMap))
		(liftEditorAsymmetric toString parseDate (withHintAttributes "date (yyyy-mm-dd)" (textField 'DM'.newMap)))
where
	parseDate s = if (isDateFormat s) (Ok (fromString s)) (Error "you need to enter a date in the format yyyy-mm-dd")

gDefault{|Date|} = {Date|day = 1, mon = 1, year = 1970}

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

gEditor{|Time|} = whenDisabled
		(liftEditor toString fromString (textView 'DM'.newMap))
		(liftEditorAsymmetric toString parseTime (withHintAttributes "time (hh:mm:ss)" (textField 'DM'.newMap)))
where
	parseTime s = if (isTimeFormat s) (Ok (fromString s)) (Error "you need to enter a time in the format hh:mm:ss")

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

gEditor{|DateTime|} = whenDisabled
		(liftEditor toString fromString (textView 'DM'.newMap))
		(liftEditorAsymmetric toString parseDateTime (withHintAttributes "date/time (yyyy-mm-dd hh:mm:ss)" (textField 'DM'.newMap)))
where
	parseDateTime s = if True (Ok (fromString s)) (Error "you need to enter a date/time in the format yyyy-mm-dd hh:mm:ss")

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

gEditor {|Document|} = liftEditor toView fromView (documentField 'DM'.newMap)
where
	toView {Document|documentId,contentUrl,name,mime,size} = (documentId,contentUrl,name,mime,size)
	fromView (documentId,contentUrl,name,mime,size) = {Document|documentId=documentId,contentUrl=contentUrl,name=name,mime=mime,size=size}

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
	
instance toString AttachException
where
	toString InstanceNotFound	= "Cannot find task instance to attach"
	toString InstanceEvalError	= "Error in attached task instance "

derive class iTask	FileException, ParseException, CallException, SharedException, RPCException, OSException, AttachException
derive class iTask	FileError

//* A sliding scale
gText{|Scale|}	_ (Just {Scale|cur}) = [toString cur]
gText{|Scale|}	_ _                  = [""]

gEditor{|Scale|} = liftEditor toSlider fromSlider (slider ('DM'.fromList [("min",JSONInt 1),("max",JSONInt 5)]))
where
	toSlider {Scale|cur} = cur
	fromSlider cur = {Scale|min=1,cur=cur,max=5}

gDefault{|Scale|} = {Scale|min=1,cur=3,max=5}

//* Progress bars
gText{|Progress|}	_ val  = [maybe "" (\{Progress|description} -> description) val]

gEditor{|Progress|} = liftEditor toView fromView (progressBar 'DM'.newMap)
where
	toView {Progress|progress,description} = (value,Just description)
	where
		value = case progress of
			ProgressUndetermined = Nothing
			(ProgressRatio r) = Just (entier (100.0 * r))

	fromView _ = defaultValue

derive gDefault			Progress

gText{|ProgressAmount|} _ (Just ProgressUndetermined)	= ["Undetermined"]
gText{|ProgressAmount|} _ (Just (ProgressRatio r))		= [toString (entier (100.0 * r)) + "%"]
gText{|ProgressAmount|} _ _		                    = [""]

derive gDefault			ProgressAmount
derive gEditor 			ProgressAmount

//* Inclusion of external html files
gText{|HtmlInclude|}	_ (Just (HtmlInclude location))	= ["<External html: " + location + ">"]
gText{|HtmlInclude|}	_ _	                            = [""]

gEditor{|HtmlInclude|} = liftEditor toView fromView (htmlView 'DM'.newMap)
where
	toView (HtmlInclude src) = IframeTag [SrcAttr src] []
	fromView _ 				 = defaultValue

derive gDefault HtmlInclude

//* Form buttons
gText{|FormButton|}	_ val = [maybe "" (\v -> v.FormButton.label) val]

gEditor{|FormButton|} = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|taskId,mode}
		# text = val.FormButton.label
		# iconCls = val.FormButton.icon
		# attr = 'DM'.unions [textAttr text,iconClsAttr iconCls,enabledAttr True,editAttrs taskId (editorId dp) (Just (JSONString "pressed"))]
		= (Ok (uia UIButton attr,newFieldMask), vst)

	onEdit = basicEdit (\st b -> Just {FormButton|b & state = st})

	onRefresh dp val=:{FormButton|state=new} {FormButton|state=old} mask vst
		= (Ok (if (old === new) NoChange (ChangeUI [SetAttribute "value" (toJSON new)] []),mask),val,vst)

gDefault{|FormButton|} = {FormButton | label = "Form Button", icon="", state = NotPressed}

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

//* Table consisting of headers, the displayed data cells & possibly a selection
gText{|Table|}	_ _	= ["<Table>"]
gEditor{|Table|} = liftEditor toGrid fromGrid (grid (multipleAttr False))
where
	toGrid (Table header rows mbSel) = ({ChoiceGrid|header=header,rows=rows},maybeToList mbSel)
	fromGrid ({ChoiceGrid|header,rows},sel) = Table header rows (listToMaybe sel)

gDefault{|Table|} = Table [] [] Nothing

toTable	:: ![a] -> Table | gText{|*|} a
toTable a = Table (headers a Nothing) (map row a) Nothing
where
	headers:: [a] (Maybe a) -> [String] | gText{|*|} a
	headers _ v = gText{|*|} AsHeader v

	row x =  [Text cell \\ cell <- gText{|*|} AsRow (Just x)]
	
derive JSONEncode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table
derive JSONDecode		Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table
derive gEq				Scale, Progress, ProgressAmount, HtmlInclude, FormButton, ButtonState, Table

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

//* Visualization wrappers
gText{|VisualizationHint|} fx mode (Just val) = case val of
	VHHidden x		= gText{|* -> *|} fx mode (Just (Hidden x))
	VHDisplay x		= gText{|* -> *|} fx mode (Just (Display x))
	VHEditable x	= gText{|* -> *|} fx mode (Just (Editable x))
gText{|VisualizationHint|} fx _ _ = [""]

gEditor{|VisualizationHint|} fx gx dx jex jdx = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = case val of
		VHHidden x		= (gEditor{|* -> *|} fx gx dx jex jdx).Editor.genUI dp (Hidden x) vst
		VHDisplay x		= (gEditor{|* -> *|} fx gx dx jex jdx).Editor.genUI dp (Display x) vst
		VHEditable x	= (gEditor{|* -> *|} fx gx dx jex jdx).Editor.genUI dp (Editable x) vst


	onEdit dp e val=:(VHEditable s) mask ust = wrapperUpdate fx.Editor.onEdit fromVisualizationHint VHEditable dp e val mask ust
	onEdit dp e val=:(VHDisplay s) mask ust = wrapperUpdate fx.Editor.onEdit fromVisualizationHint VHDisplay dp e val mask ust
	onEdit dp e val=:(VHHidden s) mask ust = wrapperUpdate fx.Editor.onEdit fromVisualizationHint VHHidden dp e val mask ust

	onRefresh dp (VHEditable new) (VHEditable old) mask vst
		# (change,Editable val,vst) = (gEditor{|* -> *|} fx gx dx jex jdx).Editor.onRefresh dp (Editable new) (Editable old) mask vst 
		= (change,VHEditable val,vst)
	onRefresh dp (VHDisplay new) (VHDisplay old) mask vst   
		# (change,Display val,vst) = (gEditor{|* -> *|} fx gx dx jex jdx).Editor.onRefresh dp (Display new) (Display old) mask vst 
		= (change,VHDisplay val,vst)
	onRefresh dp (VHHidden new) (VHHidden old) mask vst     
		# (change,Hidden val,vst) = (gEditor{|* -> *|} fx gx dx jex jdx).Editor.onRefresh dp (Hidden new) (Hidden old) mask vst 
		= (change,VHHidden val,vst)
	onRefresh _ _ val mask vst = (Ok (NoChange,mask),val,vst)

fromVisualizationHint :: !(VisualizationHint .a) -> .a
fromVisualizationHint (VHEditable a) = a
fromVisualizationHint (VHDisplay a) = a
fromVisualizationHint (VHHidden a) = a

toVisualizationHint :: !.a -> (VisualizationHint .a)
toVisualizationHint a = (VHEditable a)

gText{|Hidden|} _ _ _ = []

gEditor{|Hidden|} fx _ _ _ _ = emptyEditor

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)

gText{|Display|} fx mode (Just (Display val))	= fx mode (Just val)
gText{|Display|} fx mode Nothing               = fx mode Nothing

gEditor{|Display|} ex j _ _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (Display val) vst=:{VSt|mode}
		# (def,vst) = ex.Editor.genUI dp val {VSt|vst & mode = View}
		= (def,{VSt|vst & mode = mode})

	onEdit dp e val mask ust = wrapperUpdate ex.Editor.onEdit fromDisplay Display dp e val mask ust

	onRefresh dp (Display new) (Display old) mask vst=:{VSt|mode}
		# (change,val,vst) = ex.Editor.onRefresh dp new old mask {VSt|vst & mode = View}
		= (change,Display val,{VSt|vst & mode = mode})


fromDisplay :: !(Display .a) -> .a
fromDisplay (Display a) = a

toDisplay :: !.a -> (Display .a)
toDisplay a = (Display a)

gText{|Editable|} fx mode (Just (Editable val))    = fx mode (Just val)
gText{|Editable|} fx mode Nothing                  = fx mode Nothing

gEditor{|Editable|} ex _ _ _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh}
where
	genUI dp val vst=:{VSt|mode}
		# (def,vst) = ex.Editor.genUI dp (fromEditable val) {VSt | vst & mode = Update}
		= (def,{VSt | vst & mode = mode})

	onRefresh dp (Editable new) (Editable old) mask vst
		# (change,val,vst) = ex.Editor.onRefresh dp new old mask vst
		= (change,Editable val,vst)

	onEdit dp e val mask ust = wrapperUpdate ex.Editor.onEdit fromEditable Editable dp e val mask ust

fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

gText{|Row|} gVizx mode (Just (Row val)) = gVizx mode (Just val)
gText{|Row|} gVizx mode Nothing = gVizx mode Nothing

gEditor{|Row|} ex _ _ _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh} 
where
	genUI dp (Row val) vst
		= case ex.Editor.genUI dp val vst of
			(Ok (ui,mask),vst) = (Ok (setHorizontal ui,mask),vst)
			(Error e,vst)      = (Error e,vst)
	where	
		setHorizontal (UI type attr items) = UI type ('DM'.union (directionAttr Horizontal) attr) items

	onEdit dp e val mask ust = wrapperUpdate ex.Editor.onEdit (\(Row x) -> x) Row dp e val mask ust

	onRefresh dp (Row new) (Row old) mask vst
		= case ex.Editor.onRefresh dp new old mask vst of
			(Ok change,val,vst) = (Ok change, Row val,vst)
			(Error e,val,vst)   = (Error e,Row val,vst)

gText{|Col|} gVizx mode (Just (Col val)) = gVizx mode (Just val)
gText{|Col|} gVizx mode Nothing = gVizx mode Nothing

gEditor{|Col|} ex _ _ _ _ = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (Col val) vst
		= case ex.Editor.genUI dp val vst of
			(Ok (ui,mask),vst) = (Ok (setVertical ui,mask),vst)
			(Error e,vst)      = (Error e,vst)
	where	
		setVertical (UI type attr items) = UI type ('DM'.union (directionAttr Horizontal) attr) items

	onEdit dp e val mask ust = wrapperUpdate ex.Editor.onEdit (\(Col x) -> x) Col dp e val mask ust

	onRefresh dp (Col new) (Col old) mask vst
		= case ex.Editor.onRefresh dp new old mask vst of
			(Ok change,val,vst) = (Ok change, Col val,vst)
			(Error e,val,vst)   = (Error e,Col val,vst)
	
wrapperUpdate fx get set target upd val mask vst
	# (mbmask,w,vst) = fx target upd (get val) mask vst
	= (mbmask,set w,vst)

derive JSONEncode		Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive JSONDecode		Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gDefault			Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gEq				Hidden, Display, Editable, VisualizationHint, Row, Col, EditableList, EditableListAdd
derive gText            EditableList, EditableListAdd

//* Framework types

instance Functor TaskValue
where
	fmap f (NoValue)		= NoValue
	fmap f (Value v s)		= Value (f v) s

instance Functor Task
where
	fmap f t = t @ f

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

derive class iTask TaskId, Config, ProcessStatus
	
instance toString Icon
where
	toString (Icon icon) = icon
	toString (IconView)	= "view"
	toString (IconEdit) = "edit"
	
instance toPrompt (!Icon,!String,!String)
where
	toPrompt (icon,title,hint) 
		# (UI type attr items) = stringDisplay hint
		# attr = 'DM'.put ICON_ATTRIBUTE (JSONString (toString icon)) attr
		# attr = 'DM'.put TITLE_ATTRIBUTE (JSONString title) attr
		= UI type attr items
instance toPrompt Title
where
	toPrompt (Title title) = uia UIEmpty ('DM'.fromList [(TITLE_ATTRIBUTE,JSONString title)])
	
instance toPrompt Label
where
	toPrompt (Label label) = uia UIEmpty ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)])

instance toPrompt Hint
where
	toPrompt (Hint hint) = uia UIEmpty ('DM'.fromList [(HINT_ATTRIBUTE,JSONString hint)])
	
instance toPrompt Icon
where
	toPrompt icon = uia UIEmpty ('DM'.fromList [(ICON_ATTRIBUTE,JSONString (toString icon))])

instance toPrompt Attribute
where
	toPrompt (Attribute k v) = uia UIEmpty ('DM'.fromList [(k,JSONString v)])
	
instance toPrompt Att
where
	toPrompt (Att a) = toPrompt a
	
instance toPrompt [d] | toPrompt d
where
	toPrompt list = ui UIEmpty

derive JSONEncode		Icon
derive JSONDecode		Icon
derive gDefault			Icon
derive gEq				Icon
derive gText	        Icon

gEditor{|Icon|} = liftEditor toView fromView (icon 'DM'.newMap)
where
	toView (Icon name) = ("icon-"+++name,Nothing)
	fromView _ = defaultValue

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

gEq{|(->)|} _ _ fa fb		= copy_to_string fa == copy_to_string fb // HACK: Compare string representations of graphs functions are never equal
gEq{|Dynamic|} _ _			= False	// dynamics are never equal

gDefault{|{}|} _ = undef
gEditor{|{}|} _ _ _ _ _ = emptyEditor
gText{|{}|} _ _ _ = undef

derive JSONEncode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive JSONDecode SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEq        SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault   SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gEditor    SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gText      SVGElt, SVGAttr, SVGAlign, SVGColor, SVGDefer, SVGFillOpacity, SVGFuncIRI, SVGLengthAdjust, SVGLengthUnit, SVGLineCap, SVGFillRule, SVGLineJoin, SVGMeetOrSlice, SVGStrokeMiterLimit, SVGPaint, SVGStrokeDashArray, SVGStrokeDashOffset, SVGStrokeWidth, SVGTransform, SVGZoomAndPan
derive gDefault   HtmlAttr
derive gEditor    HtmlAttr
derive gText      HtmlAttr
