implementation module Util

import StdList, StdFile, StdMisc, StdArray, StdString, StdGeneric, Maybe, Time, Text
from Types	import :: Date{..}, :: Time{..}, :: DateTime(..)

fileExtension :: !String -> String
fileExtension filename = case (split "." filename) of
	[_]		= ""
	parts	= last parts
	
baseName :: !String -> String
baseName path = last (split "\\" path)

mapSt :: (a *st -> (b,*st)) [a] *st -> ([b],*st)
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

currentTime :: !*World -> (!Time,!*World)
currentTime world
	# (tm,world) = localTime world
	= ({Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec},world)

currentDate :: !*World -> (!Date,!*World)
currentDate world
	# (tm,world) = localTime world
	= ({Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year},world)

currentDateTime :: !*World -> (!DateTime,!*World)
currentDateTime world
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
