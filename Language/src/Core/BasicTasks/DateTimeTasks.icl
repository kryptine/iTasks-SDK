implementation module DateTimeTasks

import StdInt
import TSt, Void
import Time

import CommonDomain
import CoreCombinators

getCurrentTime :: Task Time
getCurrentTime = mkInstantTask "getCurrentTime" (mkTaskFunction (accWorldTSt currentTime))
	
getCurrentDate :: Task Date
getCurrentDate = mkInstantTask "getCurrentDate" (mkTaskFunction (accWorldTSt currentDate))

getCurrentDateTime :: Task DateTime
getCurrentDateTime = mkInstantTask "getCurrentDateTime" (mkTaskFunction (accWorldTSt currentDateTime))

waitForTime :: !Time -> Task Void
waitForTime time = mkMonitorTask "waitForTime" waitForTime`
where
	waitForTime` tst
		# (now,tst) = accWorldTSt currentTime tst
		| now < time
			# tst = setStatus [Text "Waiting until ": visualizeAsHtmlLabel time] tst
			= (TaskBusy,tst)
		| otherwise
			= (TaskFinished Void,tst)

waitForDate :: !Date -> Task Void
waitForDate date = mkMonitorTask "waitForDate" waitForDate`
where
	waitForDate` tst
		# (now,tst) = accWorldTSt currentDate tst
		| now < date
			# tst = setStatus [Text "Waiting until ": visualizeAsHtmlLabel date] tst
			= (TaskBusy,tst)
		| otherwise
			= (TaskBusy,tst)

waitForTimer :: !Time -> Task Void
waitForTimer time
	= getCurrentTime >>= \now -> waitForTime (now + time)
