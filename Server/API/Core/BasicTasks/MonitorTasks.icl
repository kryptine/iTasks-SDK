implementation module MonitorTasks

import StdBool, StdTuple, StdFunc, Util, Shared, Task, TSt, InteractiveTasks
from Shared				import :: Shared
from TaskPanel			import :: InteractiveTaskType(..)

monitorTask :: !d !(m -> v) !(m -> Bool) !Bool !(Shared m w) -> Task m | descr d & iTask m & iTask v
monitorTask d view pred autoContinue shared
	= mkInteractiveTask d Monitor (makeMonitorTask shared view pred autoContinue)

monitorTaskA :: !d !(m -> v) ![TaskAction m] !(AutoActionEvents m) !(Shared m w) -> Task (!ActionEvent,!Maybe m) | descr d & iTask m & iTask v
monitorTaskA d view actions autoEvents shared
	= mkInteractiveTask d Monitor (makeMonitorTaskA shared view actions autoEvents)

makeMonitorTask :: !(Shared m w) !(m -> v) !(m -> Bool) !Bool -> TaskFunctions m | iTask m & iTask v
makeMonitorTask shared view pred autoContinue
	= mapTaskFunctions (fromJust o snd) (makeMonitorTaskA shared view actions autoEvents)
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
		
makeMonitorTaskA	:: !(Shared m w) !(m -> v) ![TaskAction m] !(AutoActionEvents m) -> TaskFunctions (!ActionEvent,!Maybe m) | iTask m & iTask v
makeMonitorTaskA shared view actions autoEvents
	# shared = toReadOnlyShared shared
	= makeInteractiveTask (Just (SharedAbout shared)) view (Hidden,\_ _ -> Void) actions (Just autoEvents) (SharedUpdate shared)
