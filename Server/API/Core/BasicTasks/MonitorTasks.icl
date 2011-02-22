implementation module MonitorTasks

import StdBool, StdTuple, StdFunc, Util, Shared, Task, TSt, InteractiveTasks
from Shared				import :: Shared
from TaskPanel			import :: InteractiveTaskType(..)

monitorTask	 :: !(m -> v) !(m -> Bool) !Bool !(Shared m w) -> Task m | iTask m & iTask v
monitorTask view pred autoContinue shared
	= mkInteractiveTask ("Monitor task","Monitors a shared state") Monitor (makeMonitorTask shared view pred autoContinue)

monitorTaskA :: !(m -> v) ![TaskAction m] !(AutoActionEvents m) !(Shared m w) -> Task (!ActionEvent,!Maybe m) | iTask m & iTask v
monitorTaskA view actions autoEvents shared
	= mkInteractiveTask ("Monitor task actions","Monitors a shared state") Monitor (makeMonitorTaskA shared view actions autoEvents)

makeMonitorTask :: !(Shared m w) !(m -> v) !(m -> Bool) !Bool !*TSt -> (!TaskResult m,!*TSt) | iTask m & iTask v
makeMonitorTask shared view pred autoContinue tst
	= appFst (mapTaskResult (fromJust o snd)) (makeMonitorTaskA shared view actions autoEvents tst)
where
	actions
		| autoContinue	= []
		| otherwise		= [(ActionContinue,pred`)]
		
	pred` Invalid	= False
	pred` (Valid v)	= pred v
	
	autoEvents Invalid				= Nothing
	autoEvents (Valid v)
		| autoContinue && pred v	= Just (ActionContinue,"")
		| otherwise					= Nothing
		
makeMonitorTaskA	:: !(Shared m w) !(m -> v) ![TaskAction m] !(AutoActionEvents m) !*TSt -> (!TaskResult (!ActionEvent,!Maybe m),!*TSt) | iTask m & iTask v
makeMonitorTaskA shared view actions autoEvents tst
	# shared = toReadOnlyShared shared
	= makeInteractiveTask (Just (SharedAbout shared)) view (Hidden,\_ _ -> Void) actions autoEvents (SharedUpdate shared) tst
