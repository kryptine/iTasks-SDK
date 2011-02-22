implementation module DateTimeTasks

import StdInt, Error, TSt, Types, Void, Util, Time, Shared, CoreCombinators, MonitorTasks
from TaskPanel			import :: InteractiveTaskType(..)
from SharedTasks		import qualified readShared
from CommonCombinators	import stop

getCurrentTime :: Task Time
getCurrentTime = mkInstantTask ("Get current time", "Determine the current time") (mkTaskFunction (accWorldTSt currentTime))
	
getCurrentDate :: Task Date
getCurrentDate = mkInstantTask ("Get current date", "Determine the current date") (mkTaskFunction (accWorldTSt currentDate))

getCurrentDateTime :: Task DateTime
getCurrentDateTime = 'SharedTasks'.readShared sharedCurrentDateTime

waitForTime :: !Time -> Task Void
waitForTime time =
		monitorTask ("Wait for time", ("Wait until " +++ toString time)) view pred True sharedCurrentTime
	>>| stop
where	
	view _ = "Waiting until " <+++ time//html [Text "Waiting until ",visualizeAsHtmlLabel time]
	pred now = time < now

waitForDate :: !Date -> Task Void
waitForDate date =
		monitorTask ("Wait for date", ("Wait until " +++ toString date)) view pred True sharedCurrentDate
	>>| stop
where
	view _ = "Waiting until " <+++ date//html [Text "Waiting until ",visualizeAsHtmlLabel date]
	pred now = date < now

waitForTimer :: !Time -> Task Void
waitForTimer time = getCurrentTime >>= \now -> waitForTime (now + time)

sharedCurrentDateTime :: ReadOnlyShared DateTime
sharedCurrentDateTime = makeReadOnlyShared read
where
	read iworld=:{world}
		# (dateTime,world) = currentDateTime world
		= (dateTime,{iworld & world = world})
		
sharedCurrentTime :: ReadOnlyShared Time
sharedCurrentTime = makeReadOnlyShared read
where
	read iworld=:{world}
		# (dateTime,world) = currentTime world
		= (dateTime,{iworld & world = world})
		
sharedCurrentDate :: ReadOnlyShared Date
sharedCurrentDate = makeReadOnlyShared read
where
	read iworld=:{world}
		# (dateTime,world) = currentDate world
		= (dateTime,{iworld & world = world})

