implementation module DateTimeTasks

import StdInt
import TSt, Void
import Time

import CommonDomain
import CoreCombinators

getCurrentTime :: Task Time
getCurrentTime = mkInstantTask "getCurrentTime" (accWorldTSt currentTime)
	
getCurrentDate :: Task Date
getCurrentDate = mkInstantTask "getCurrentDate" (accWorldTSt currentDate)

getCurrentDateTime :: Task DateTime
getCurrentDateTime = mkInstantTask "getCurrentDateTime" (accWorldTSt currentDateTime)

waitForTime :: !Time -> Task Void
waitForTime time = mkMonitorTask "waitForTime" waitForTime`
where
	waitForTime` tst
		# (now,tst) = accWorldTSt currentTime tst
		| now < time
			# tst = setStatus [Text "Waiting until ": visualizeAsHtmlLabel time] tst
			= (Void,{tst & activated = False})
		| otherwise
			= (Void,{tst & activated = True})

waitForDate :: !Date -> Task Void
waitForDate date = mkMonitorTask "waitForDate" waitForDate`
where
	waitForDate` tst
		# (now,tst) = accWorldTSt currentDate tst
		| now < date
			# tst = setStatus [Text "Waiting until ": visualizeAsHtmlLabel date] tst
			= (Void,{tst & activated = False})
		| otherwise
			= (Void,{tst & activated = True})

waitForTimer :: !Time -> Task Void
waitForTimer time
	= getCurrentTime >>= \now -> waitForTime (now + time)

