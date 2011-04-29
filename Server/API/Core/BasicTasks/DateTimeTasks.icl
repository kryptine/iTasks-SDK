implementation module DateTimeTasks

import StdInt, Error, TSt, Types, Void, Util, Time, Shared, CoreCombinators, OutputTasks
from CommonCombinators	import stop

getCurrentTime :: Task Time
getCurrentTime = mkInstantTask ("Get current time", "Determine the current time") (mkTaskFunction (accIWorldTSt currentTime))
	
getCurrentDate :: Task Date
getCurrentDate = mkInstantTask ("Get current date", "Determine the current date") (mkTaskFunction (accIWorldTSt currentDate))

getCurrentDateTime :: Task DateTime
getCurrentDateTime = mkInstantTask ("Get current datetime", "Determine the current date and time.") (mkTaskFunction (accIWorldTSt currentDateTime))

waitForTime :: !Time -> Task Void
waitForTime time =
		waitUntil ("Wait for time", ("Wait until " +++ toString time)) pred sharedCurrentTime
	>>| stop
where	
	pred now = time < now

waitForDate :: !Date -> Task Void
waitForDate date =
		waitUntil ("Wait for date", ("Wait until " +++ toString date)) pred sharedCurrentDate
	>>| stop
where
	pred now = date < now

waitForTimer :: !Time -> Task Void
waitForTimer time = getCurrentTime >>= \now -> waitForTime (now + time)

sharedCurrentDateTime :: ReadOnlyShared DateTime
sharedCurrentDateTime = makeReadOnlyShared currentDateTime
		
sharedCurrentTime :: ReadOnlyShared Time
sharedCurrentTime = makeReadOnlyShared currentTime
		
sharedCurrentDate :: ReadOnlyShared Date
sharedCurrentDate = makeReadOnlyShared currentDate
