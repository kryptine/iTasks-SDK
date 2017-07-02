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
import iTasks.SDS.Definition
from iTasks.UI.Definition import :: UI(..), :: UIDirection(..), stringDisplay
from iTasks.WF.Tasks.Core import treturn
from iTasks.API.Common.TaskCombinators import tbind, @

from iTasks.API.Extensions.Form.Pikaday import pikadayDateField

instance Functor Task where
  fmap f x = x @ f
instance TApplicative Task where
  (<#>) tf ta = tf >>= \f -> fmap f ta
  return x    = treturn x
instance TMonad Task where
  (>>=) l r = tbind l r
  (>>|) l r = l >>= \_ -> r

instance TApplicative Maybe where
  (<#>) (Just f) (Just x) = Just (f x)
  (<#>) _ _ = Nothing
  return x = Just x
instance TMonad Maybe where
  (>>=) (Just x) f = f x
  (>>=) _ _ = Nothing
  (>>|) l r = l >>= \_ -> r

instance TApplicative [] where
  (<#>) fs xs = [f x \\ f <- fs, x <- xs]
  return x = [x]
instance TMonad [] where
  (>>=) xs f = [y \\ x <- xs, y <- f x]
  (>>|) l r = l >>= \_ -> r

instance TApplicative (Either e) where
  (<#>) (Right f) (Right x) = Right (f x)
  (<#>) (Left e) _ = Left e
  (<#>) _ (Left e) = Left e
  return x = Right x
instance TMonad (Either e) where
  (>>=) (Left x) _ = Left x
  (>>=) (Right x) f = f x
  (>>|) l r = l >>= \_ -> r

JSONEncode{|SDS|} _ _ _ _ s = []
JSONDecode{|SDS|} _ _ _ _ s = (Nothing, s)
gEq{|SDS|} _ _ _ _ _ = False
gDefault{|SDS|} _ _ _ = SDSSource { SDSSource
                                  | name  = "gDefault RWShared"
                                  , read  = \_ w -> (Error (dynamic "", "No gDefault RWShared implementation"), w)
                                  , write = \_ _ w -> (Error (dynamic "", "No gDefault RWShared implementation"), w)}

//* (Local) date and time
toTime :: DateTime -> Time
toTime {DateTime|hour,min,sec} = {Time|hour=hour,min=min,sec=sec}

toDate :: DateTime -> Date
toDate {DateTime|year,mon,day} = {Date|year=year,mon=mon,day=day}

toDateTime :: Date Time -> DateTime
toDateTime {Date|year,mon,day} {Time|hour,min,sec} = {DateTime|year=year,mon=mon,day=day,hour=hour,min=min,sec=sec}

instance toString Date
where
	toString {Date|year,mon,day} = lpad (toString year) 4 '0' +++ "-" +++ lpad (toString mon) 2 '0' +++ "-" +++ lpad (toString day) 2 '0'

parseDate :: String -> MaybeErrorString Date //Expected format: "yyyy-mm-dd"
parseDate s
	//Check format
	| size s == 10 && foldl (\ok i -> ok && (if (i == 4 || i == 7) (s.[i] == '-') (isDigit s.[i]))) True [0..9]
		= Ok {Date|year = toInt (s %(0,3)), mon = toInt (s %(5,6)), day = toInt (s %(8,9))}
	| otherwise = Error "date needs to be formatted as yyyy-mm-dd"

instance fromString Date
where
	fromString s = fromOk (parseDate s)

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

JSONEncode{|Date|} _ d = [JSONString (toString d)]
JSONDecode{|Date|} _ [JSONString s:c] = case parseDate s of (Ok d) = (Just d,c) ; _ = (Nothing,c)
JSONDecode{|Date|} _ c = (Nothing, c)

gText{|Date|} _ val = [maybe "" toString val]

gEditor{|Date|} = pikadayDateField /*whenDisabled
		(liftEditor toString fromString (textView 'DM'.newMap))
		(liftEditorAsymmetric toString parseDate (withHintAttributes "date (yyyy-mm-dd)" (textField 'DM'.newMap)))
*/

gDefault{|Date|} = {Date|day = 1, mon = 1, year = 2017}
derive gEq Date

instance toString Time
where
	toString {Time|hour,min,sec} = lpad (toString hour) 2 '0' +++ ":" +++ lpad (toString min) 2 '0' +++ ":" +++ lpad (toString sec) 2 '0'

parseTime :: String -> MaybeErrorString Time // Expected format: "hh:mm:ss"
parseTime s
	//Check format
	| size s == 8 && foldl (\ok i -> ok && (if (i == 2 || i == 5) (s.[i] == ':') (isDigit s.[i]))) True [0..7]
		= Ok {Time|hour = toInt (s %(0,1)), min = toInt (s %(3,4)), sec = toInt (s %(6,7)) }
	| otherwise = Error "time needs to be formatted as hh:mm:ss"

instance fromString Time
where
	fromString s = fromOk (parseTime s)

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

JSONEncode{|Time|} _ t = [JSONString (toString t)]
JSONDecode{|Time|} _ [JSONString s:c] = case parseTime s of (Ok t) = (Just t, c) ; _ = (Nothing,c)
JSONDecode{|Time|} _ c = (Nothing, c)

gText{|Time|} _ val = [maybe "" toString val]

gEditor{|Time|} = whenDisabled
		(liftEditor toString fromString (textView 'DM'.newMap))
		(liftEditorAsymmetric toString parseTime (withHintAttributes "time (hh:mm:ss)" (textField 'DM'.newMap)))

derive gDefault		Time
derive gEq			Time

instance toString DateTime
where
	toString {DateTime|year,mon,day,hour,min,sec} 
		= toString {Date|year=year,mon=mon,day=day} +++ " " +++ toString {Time|hour=hour,min=min,sec=sec}

parseDateTime :: String -> MaybeErrorString DateTime //Expected format: "yyyy-mm-dd hh:mm:ss"
parseDateTime s
	//Check format
	| size s == 19 && foldl (\ok i -> ok && check i s.[i]) True [0..18]
		= Ok {DateTime|year = toInt (s %(0,3)), mon = toInt (s %(5,6)), day = toInt (s %(8,9)) 
					  ,hour = toInt (s %(11,12)), min = toInt (s %(14,15)), sec = toInt (s %(17,18))}
	| otherwise = Error "date/time needs to be formatted as yyyy-mm-dd hh:mm:ss"
where
	check i c 
		| i == 4 || i == 7 = c == '-'
		| i == 10 = c == ' '
		| i == 13 || i == 16 = c == ':'
		| otherwise = isDigit c

instance fromString DateTime
where
	fromString s	= fromOk (parseDateTime s)

instance == DateTime
where
	(==) x y = x.DateTime.year == y.DateTime.year && x.DateTime.mon == y.DateTime.mon && x.DateTime.day == y.DateTime.day
		     && x.DateTime.hour == y.DateTime.hour && x.DateTime.min == y.DateTime.min && x.DateTime.sec == y.DateTime.sec
	
instance < DateTime
where
	(<) x y 
		| x.DateTime.year < y.DateTime.year	= True
		| x.DateTime.year == y.DateTime.year 
            && x.DateTime.mon < y.DateTime.mon = True
		| x.DateTime.year == y.DateTime.year 
			&& x.DateTime.mon == y.DateTime.mon 
			&& x.DateTime.day < y.DateTime.day = True
		| x.DateTime.year == y.DateTime.year 
			&& x.DateTime.mon == y.DateTime.mon 
			&& x.DateTime.day == y.DateTime.day
			&& x.DateTime.hour < y.DateTime.hour = True
		| x.DateTime.year == y.DateTime.year 
			&& x.DateTime.mon == y.DateTime.mon 
			&& x.DateTime.day == y.DateTime.day
			&& x.DateTime.hour ==  y.DateTime.hour
			&& x.DateTime.min <  y.DateTime.min = True
		| x.DateTime.year == y.DateTime.year 
			&& x.DateTime.mon == y.DateTime.mon 
			&& x.DateTime.day == y.DateTime.day
			&& x.DateTime.hour ==  y.DateTime.hour
			&& x.DateTime.min ==  y.DateTime.min
			&& x.DateTime.sec <  y.DateTime.sec = True
		| otherwise = False

JSONEncode{|DateTime|} _ dt	= [JSONString (toString dt)]
JSONDecode{|DateTime|} _ [JSONString s:c] = case parseDateTime s of (Ok dt) = (Just dt, c) ; _ = (Nothing, c)
JSONDecode{|DateTime|} _ c = (Nothing, c)

gText{|DateTime|} AsHeader _ = [""]
gText{|DateTime|} _ (Just ({DateTime|year,mon,day,hour,min,sec}))
	= [toSingleLineText {Date|year=year,mon=mon,day=day} +++" "+++ toSingleLineText {Time|hour=hour,min=min,sec=sec}]

gEditor{|DateTime|} = whenDisabled
		(liftEditor toString fromString (textView 'DM'.newMap))
		(liftEditorAsymmetric toString parseDateTime (withHintAttributes "date/time (yyyy-mm-dd hh:mm:ss)" (textField 'DM'.newMap)))

derive gDefault			DateTime
derive gEq				DateTime

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

class toInstanceNo t :: t -> InstanceNo
instance toInstanceNo InstanceNo where toInstanceNo no = no
instance toInstanceNo TaskId where toInstanceNo (TaskId no _) = no

instance == Action
where
	(==) :: !Action !Action -> Bool
	(==) (Action name0) (Action name1) = name0 == name1

derive JSONEncode		TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive JSONDecode		TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gDefault			TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gEq				TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gText	        TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action
derive gEditor			TaskValue, InstanceConstants, InstanceProgress, ValueStatus, TaskInstance, TaskListItem, Action

derive class iTask TaskId, Config, ProcessStatus

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
