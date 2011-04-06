implementation module Util

import StdList, StdFile, StdMisc, StdArray, StdString, StdTuple, StdGeneric, StdOrdList, Maybe, Time, Text, JSON
from Types	import :: Date{..}, :: Time{..}, :: DateTime(..), :: IWorld{localDateTime,timestamp}
from iTasks import serialize, deserialize
from Base64 import base64Encode, base64Decode

fileExtension :: !String -> String
fileExtension filename = case (split "." filename) of
	[_]		= ""
	parts	= last parts
	
baseName :: !String -> String
baseName path = last (split "\\" path)

app :: !(.a -> .b) !.a -> .b
app f x = f x

seqSt :: !(a .st -> .st) ![a] !.st -> .st
seqSt f [] st = st
seqSt f [x:xs] st = seqSt f xs (f x st)

mapSt :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],!.st)
mapSt f [] st = ([], st)
mapSt f [x:xs] st
	# (y, st) = f x st
	# (ys, st) = mapSt f xs st
	= ([y:ys], st)

mb2list	:: !(Maybe [a]) -> [a]
mb2list	Nothing = []
mb2list (Just a) = a

list2mb	:: ![a] -> (Maybe [a])
list2mb [] = Nothing
list2mb a = (Just a)

pad :: Int Int -> String
pad len num = (createArray (max 0 (len - size nums)) '0' ) +++ nums
where 
	nums = toString num
	
decFormat :: Int -> String
decFormat x = toString (x / 100) +++ "." +++ pad 2 (x rem 100)

currentTime :: !*IWorld -> (!Time,!*IWorld)
currentTime iworld=:{localDateTime=l=:(DateTime _ time)} = (time,iworld)

currentDate :: !*IWorld -> (!Date,!*IWorld)
currentDate iworld=:{localDateTime=l=:(DateTime date _)} = (date,iworld)
	
currentDateTime :: !*IWorld -> (!DateTime,!*IWorld)
currentDateTime iworld=:{localDateTime} = (localDateTime,iworld)

currentTimestamp :: !*IWorld -> (!Timestamp,!*IWorld)
currentTimestamp iworld=:{timestamp} = (timestamp,iworld)

currentDateTimeWorld :: !*World -> (!DateTime,!*World)
currentDateTimeWorld world
	# (tm,world)	= localTime world
	# date			= {Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year}
	# time			= {Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec}
	= (DateTime date time,world)

instance toString (Maybe a) | toString a
where
	toString Nothing	= ""
	toString (Just x)	= toString x

appFst	:: (.a -> .c) (.a,.b) -> (.c,.b)
appFst f (a,b) = (f a,b)

appSnd	:: (.b -> .c) (.a,.b) -> (.a,.c)
appSnd f (a,b) = (a,f b)

appFst3 :: (.a -> .d) (.a,.b,.c) -> (.d,.b,.c)
appFst3 f (a,b,c) = (f a,b,c)

appSnd3 :: (.b -> .d) (.a,.b,.c) -> (.a,.d,.c)
appSnd3 f (a,b,c) = (a,f b,c)

appThd3 :: (.c -> .d) (.a,.b,.c) -> (.a,.b,.d)
appThd3 f (a,b,c) = (a,b,f c)

fromOBJECT	:: (OBJECT x)	-> x
fromOBJECT	(OBJECT x)	= x

fromCONS	:: (CONS x)		-> x
fromCONS	(CONS x)	= x

fromFIELD	:: (FIELD x)	-> x
fromFIELD	(FIELD x)	= x

fromPAIRX	:: (PAIR x y)	-> x
fromPAIRX	(PAIR x _)	= x

fromPAIRY	:: (PAIR x y)	-> y
fromPAIRY	(PAIR _ y)	= y

isRecordType :: !GenericTypeDefDescriptor -> Bool
isRecordType {gtd_conses} = case gtd_conses of
	[{gcd_fields}]	= not (isEmpty gcd_fields)
	_				= False
	
isRecordCons :: !GenericConsDescriptor -> Bool
isRecordCons {gcd_fields} = not (isEmpty gcd_fields)

replaceInList :: !(a a -> Bool) !a ![a] -> [a]
replaceInList cond new []         = [new]
replaceInList cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replaceInList cond new xs]

splitWith :: !(a -> Bool) ![a] -> (![a],![a])
splitWith f [] = ([],[])
splitWith f [x:xs]
	| f x	= let (y,n) = splitWith f xs in ([x:y],n)
			= let (y,n)	= splitWith f xs in (y,[x:n])

sortByIndex :: ![(!Int,!a)] -> [a]
sortByIndex l = map snd (sortBy (\(a,_) (b,_) -> a < b) l)

intersperse :: !a ![a] -> [a]
intersperse i [] = []
intersperse i [x] = [x]
intersperse i [x:xs] = [x,i:intersperse i xs]

encodeFunc :: !a -> [JSONNode]
encodeFunc f = [JSONString (base64Encode (serialize f))]

decodeFunc :: !JSONNode -> Maybe a
decodeFunc (JSONString str)	= Just (deserialize {s` \\ s` <-: base64Decode str})
decodeFunc _				= Nothing
