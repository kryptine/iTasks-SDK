implementation module iTasks.Extensions.DateTime

import iTasks.WF.Definition
import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.SDS
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Core
import iTasks.WF.Combinators.Common
import iTasks.SDS.Combinators.Common
import iTasks.SDS.Sources.System
from iTasks.Internal.Task import mkInstantTask
import iTasks.Internal.IWorld
import iTasks.Internal.TaskEval
import iTasks.Internal.Util
import iTasks.UI.Definition
import iTasks.UI.Editor
import iTasks.UI.Editor.Controls
import iTasks.UI.Editor.Modifiers

import iTasks.Internal.SDS

import StdBool, StdArray, StdEnum, StdList, StdString, StdFunctions

import Text, Text.GenJSON, Text.GenPrint, System.Time
import Data.Maybe, Data.Error, Data.Func, Data.Functor
import qualified Data.Map as DM

from iTasks.Extensions.Form.Pikaday import pikadayDateField
from iTasks.Internal.Util import tmToDateTime

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

gEditor{|Date|} = pikadayDateField

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

gEditor{|Time|} = selectByMode view edit edit
where
	view = ignoreEditorWrites $ bijectEditorValue toString fromString textView
	edit
		= mapEditorWriteError (\s -> Just <$> parseTime s)
		$ injectEditorValue toString parseTime
		$ withDynamicHintAttributes "time (hh:mm:ss)" $ withEditModeAttr textField

derive gDefault		Time
derive gEq			Time
derive gPrint       Time

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

gEditor{|DateTime|} = selectByMode view edit edit
where
	view = ignoreEditorWrites $ bijectEditorValue toString fromString textView
	edit
		= mapEditorWriteError (\s -> Just <$> parseDateTime s)
		$ injectEditorValue toString parseDateTime
		$ withDynamicHintAttributes "date/time (yyyy-mm-dd hh:mm:ss)" $ withEditModeAttr textField

derive gDefault			DateTime
derive gEq				DateTime

timestampToGmDateTime :: !Timestamp -> DateTime
timestampToGmDateTime timestamp = tmToDateTime (toGmTime timestamp)

timestampToLocalDateTime :: !Timestamp -> Task DateTime
timestampToLocalDateTime ts = mkInstantTask timestampToLocalDateTime`
where
    timestampToLocalDateTime` _ iworld=:{world}
        # (tm, world) = toLocalTime ts world
        = (Ok (tmToDateTime tm), {iworld & world = world})

localDateToTimestamp :: !Date -> Task Timestamp
localDateToTimestamp {Date|day,mon,year} = mkInstantTask localDateToTimestamp`
where
    localDateToTimestamp` _ iworld=:{world}
        # (ts, world) = mkTime {Tm|sec = 0, min = 0, hour = 0, mday = day, mon = mon - 1, year = year - 1900, wday = 0, yday = 0, isdst = -1} world
	    = (Ok ts, {iworld & world = world})

localDateTimeToTimestamp :: !DateTime -> Task Timestamp
localDateTimeToTimestamp {DateTime|day,mon,year,hour,min,sec} = mkInstantTask localDateTimeToTimestamp`
where
    localDateTimeToTimestamp` _ iworld=:{world}
        # (ts, world) = mkTime {Tm|sec = sec, min = min, hour = hour, mday = day, mon = mon - 1, year = year - 1900, wday = 0, yday = 0, isdst = -1} world
	    = (Ok ts, {iworld & world = world})

utcDateToTimestamp :: !Date -> Timestamp
utcDateToTimestamp {Date|day,mon,year} =
    timeGm {Tm|sec = 0, min = 0, hour = 0, mday = day, mon = mon - 1, year = year - 1900, wday = 0, yday = 0, isdst = -1}

utcDateTimeToTimestamp :: !DateTime -> Timestamp
utcDateTimeToTimestamp {DateTime|day,mon,year,hour,min,sec} =
    timeGm {Tm|sec = sec, min = min, hour = hour, mday = day, mon = mon - 1, year = year - 1900, wday = 0, yday = 0, isdst = -1}

waitForTime :: !Bool !Time -> Task Time
waitForTime withUI time
	| withUI    = waitWithUI "Wait for time" currentTime time
	| otherwise =
		get currentDate >>- \today ->
		let target = toDateTime today time in
		get currentTime >>- \now
			| now <= time -> waitWithoutUI target @ toTime
			# (Timestamp target) = utcDateTimeToTimestamp target
			# target = timestampToGmDateTime (Timestamp (target + 3600*24))
			-> waitWithoutUI target @ toTime

waitForDate :: !Bool !Date -> Task Date
waitForDate withUI date
	| withUI    = waitWithUI "Wait for date" currentDate date
	| otherwise = waitWithoutUI (toDateTime date {Time | hour=0,min=0,sec=0}) @ toDate
	
waitForDateTime :: !Bool !DateTime -> Task DateTime
waitForDateTime withUI datetime
	| withUI    = waitWithUI "Wait for date and time" currentDateTime datetime
	| otherwise = waitWithoutUI datetime

waitForTimer :: !Bool !Int -> Task DateTime
waitForTimer withUI interval =
	get currentTimestamp >>- \(Timestamp now) ->
	timestampToLocalDateTime (Timestamp (now + interval)) >>-
	waitForDateTime withUI

waitWithUI :: !String !(sds () d ()) !d -> Task d | RWShared sds & <, toString, iTask d
waitWithUI title share target =
	Title title @>> Hint ("Wait until " +++ toString target) @>> viewSharedInformation [] share >>*
	[OnValue (ifValue (\now -> target <= now) return)]

waitWithoutUI :: !DateTime -> Task DateTime
waitWithoutUI datetime =
	localDateTimeToTimestamp datetime >>- \timestamp ->
	let
		timespec = timestampToSpec timestamp
		param = {start=timespec,interval={tv_sec=1,tv_nsec=0}}
	in
	Task (eval param) >>*
	[OnValue (ifValue ((<=) timespec) \_ -> get currentDateTime)]
where
	eval _ DestroyEvent _ iworld
		= (DestroyedResult, iworld)
	eval param event {taskId,lastEval} iworld
		# (Ok (ReadingDone now),iworld) = readRegister taskId (sdsFocus param iworldTimespec) iworld
		= (ValueResult (Value now False) (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (Task (eval param)), iworld)

dateTimeStampedShare :: !(sds p b (DateTime,c)) -> SDSLens p b c | gText{|*|}, TC p & TC b & TC c & RWShared sds
dateTimeStampedShare sds
	= sdsTranslate "dateTimeStampedShare" (\p->(p, ()))
		(sdsStamp sds currentDateTime (\x y->(x, y)))
