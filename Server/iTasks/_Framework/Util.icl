implementation module iTasks._Framework.Util

import StdBool, StdChar, StdList, StdFile, StdMisc, StdArray, StdString, StdTuple, StdFunc, StdGeneric, StdOrdList
import Data.Maybe, Data.Tuple, System.Time, System.OS, Text, System.FilePath, System.Directory, Text.JSON, Data.Void, Data.Error, GenEq
from iTasks._Framework.IWorld 		import :: IWorld{current}, :: TaskEvalState
from iTasks.API.Core.Types	        import :: Date{..}, :: Time{..}, :: DateTime(..)

mb2list	:: !(Maybe [a]) -> [a]
mb2list	Nothing = []
mb2list (Just a) = a

list2mb	:: ![a] -> (Maybe [a])
list2mb [] = Nothing
list2mb a = (Just a)

voidNothing :: Maybe Void
voidNothing = Nothing

decFormat :: !Int -> String
decFormat x = toString (x / 100) +++ "." +++ lpad (toString (x rem 100)) 2 '0'

currentLocalDateTimeWorld :: !*World -> (!DateTime,!*World)
currentLocalDateTimeWorld world = appFst tmToDateTime (localTime world)
	
currentUTCDateTimeWorld :: !*World -> (!DateTime,!*World)
currentUTCDateTimeWorld world = appFst tmToDateTime (gmTime world)

timestampToGmDateTime :: !Timestamp -> DateTime
timestampToGmDateTime timestamp = tmToDateTime (toGmTime timestamp)

tmToDateTime :: !Tm -> DateTime
tmToDateTime tm
	# date	= {Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year}
	# time	= {Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec}
	= DateTime date time

dateToTimestamp :: !Date -> Timestamp
dateToTimestamp {Date|day,mon,year}
	= mkTime {Tm|sec = 0, min = 0, hour = 0, mday = day, mon = mon - 1, year = year - 1900, wday = 0, yday = 0, isdst = False}

datetimeToTimestamp :: !DateTime -> Timestamp
datetimeToTimestamp (DateTime {Date|day,mon,year} {Time|hour,min,sec})
	= mkTime {Tm|sec = sec, min = min, hour = hour, mday = day, mon = mon - 1, year = year - 1900, wday = 0, yday = 0, isdst = False}

instance toString (Maybe a) | toString a
where
	toString Nothing	= ""
	toString (Just x)	= toString x

toCanonicalPath	:: !FilePath !*World -> (!FilePath,!*World)
toCanonicalPath path world
	| isAbsolute path = (canonicalize path,world)
	| otherwise
		= case getCurrentDirectory world of
			(Ok curDir,world)	= (canonicalize (curDir</>path), world)
			(_,world)		= (canonicalize path,world)
where
	isAbsolute path = IF_POSIX_OR_WINDOWS (startsWith {pathSeparator} path) (size path >= 2 && isUpper path.[0] && path.[1] == ':')

	canonicalize path = join {pathSeparator} (undot [] (split {pathSeparator} path))

	undot acc []				= reverse acc
	undot []  ["..":ds]			= undot [] ds
	undot [_:acc] ["..":ds]			= undot acc ds
	undot acc [".":ds]			= undot acc ds
	undot [] ["":ds]			= undot [""] ds //Only allowed at the beginning
	undot acc ["":ds]			= undot acc ds
	undot acc [d:ds] 			= undot [d:acc] ds

kvGet :: k ![(k,v)]		-> Maybe v	| Eq k // Linear search
kvGet m []				= Nothing
kvGet m [(k,v):kvs]		= if (k == m) (Just v) (kvGet m kvs)

kvSet :: k v ![(k,v)]	-> [(k,v)]	| Eq k //Linear search
kvSet m nv []			= [(m,nv)]
kvSet m nv [(k,v):kvs]	= if (k == m) [(k,nv): kvs] [(k,v):kvSet m nv kvs]

kvSetOnce :: k v ![(k,v)]	-> [(k,v)]	| Eq k //Linear search
kvSetOnce m nv []			= [(m,nv)]
kvSetOnce m nv [(k,v):kvs]	= if (k == m) [(k,v):kvs] [(k,v):kvSet m nv kvs]


