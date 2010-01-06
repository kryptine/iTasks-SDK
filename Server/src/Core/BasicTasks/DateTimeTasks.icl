implementation module DateTimeTasks

import StdInt
import TSt, Void
import Time

import CommonDomain
import CoreCombinators

tm2time :: Tm -> Time
tm2time tm = {Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec} 

tm2date :: Tm -> Date
tm2date tm = {Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year }

getCurrentTime :: Task Time
getCurrentTime = mkInstantTask "getCurrentTime" getCurrentTime`
where
	getCurrentTime` tst
		# (tm,tst) = accWorldTSt localTime tst
		= (tm2time tm ,tst)
	
getCurrentDate :: Task Date
getCurrentDate = mkInstantTask "getCurrentDate" getCurrentDate`
where
	getCurrentDate` tst
		# (tm,tst) = accWorldTSt localTime tst
		= (tm2date tm,tst)

waitForTime :: !Time -> Task Void
waitForTime time = mkMonitorTask "waitForTime" waitForTime`
where
	waitForTime` tst
		# (tm,tst) = accWorldTSt localTime tst
		| tm2time tm < time
			# tst = setStatus [Text "Waiting until ": visualizeAsHtmlLabel time] tst
			= (Void,{tst & activated = False})
		| otherwise
			= (Void,{tst & activated = True})

waitForDate :: !Date -> Task Void
waitForDate date = mkMonitorTask "waitForDate" waitForDate`
where
	waitForDate` tst
		# (tm,tst) = accWorldTSt localTime tst
		| tm2date tm < date
			# tst = setStatus [Text "Waiting until ": visualizeAsHtmlLabel date] tst
			= (Void,{tst & activated = False})
		| otherwise
			= (Void,{tst & activated = True})

waitForTimer :: !Time -> Task Void
waitForTimer time
	= getCurrentTime >>= \now -> waitForTime (now + time)

