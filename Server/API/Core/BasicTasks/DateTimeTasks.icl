implementation module DateTimeTasks

import StdInt
import TSt, Types, Void
import Time

import CoreCombinators, Util

getCurrentTime :: Task Time
getCurrentTime = mkInstantTask "Get current time" "Determine the current time" (mkTaskFunction (accWorldTSt currentTime))
	
getCurrentDate :: Task Date
getCurrentDate = mkInstantTask "Get current date" "Determine the current date" (mkTaskFunction (accWorldTSt currentDate))

getCurrentDateTime :: Task DateTime
getCurrentDateTime = mkInstantTask "Get current datetime" "Determine the current date and time." (mkTaskFunction (accWorldTSt currentDateTime))

waitForTime :: !Time -> Task Void
waitForTime time = mkMonitorTask "Wait for time" ("Wait until " +++ toString time) waitForTime`
where
	waitForTime` tst
		# (now,tst) = accWorldTSt currentTime tst
		| now < time
			# tst = setStatus [Text "Waiting until ": visualizeAsHtmlLabel time] tst
			= (TaskBusy,tst)
		| otherwise
			= (TaskFinished Void,tst)

waitForDate :: !Date -> Task Void
waitForDate date = mkMonitorTask "Wait for date" ("Wait until " +++ toString date) waitForDate`
where
	waitForDate` tst
		# (now,tst) = accWorldTSt currentDate tst
		| now < date
			# tst = setStatus [Text "Waiting until ": visualizeAsHtmlLabel date] tst
			= (TaskBusy,tst)
		| otherwise
			= (TaskBusy,tst)

waitForTimer :: !Time -> Task Void
waitForTimer time = getCurrentTime >>= \now -> waitForTime (now + time)
