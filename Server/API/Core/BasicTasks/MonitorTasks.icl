implementation module MonitorTasks

import StdBool, StdTuple, StdFunc, Util, Shared, Task, TSt, InteractiveTasks
from Shared				import :: Shared
from TaskPanel			import :: InteractiveTaskType(..)

monitor :: !d !(m -> v) !(m -> Bool) !Bool !(Shared m w) -> Task m | descr d & iTask m & iTask v
monitor d view pred autoContinue shared
	= mkInteractiveTask d Monitor (makeMonitorTask shared view pred autoContinue)

monitorA :: !d !(m -> v) ![TaskAction m] !(AutoActionEvents m) !(Shared m w) -> Task (!Action,!Maybe m) | descr d & iTask m & iTask v
monitorA d view actions autoEvents shared
	= mkInteractiveTask d Monitor (makeMonitorTaskA shared view actions autoEvents)

wait :: !d !Bool !(Shared (Maybe m) w) -> Task m | descr d & iTask m
wait d autoContinue shared
	= mapTask fromJust (mkInteractiveTask d Monitor (makeMonitorTask shared (const (Hidden Void)) isJust autoContinue))

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
		| autoContinue && pred v	= Just ActionContinue
		| otherwise					= Nothing
		
makeMonitorTaskA	:: !(Shared m w) !(m -> v) ![TaskAction m] !(AutoActionEvents m) -> TaskFunctions (!Action,!Maybe m) | iTask m & iTask v
makeMonitorTaskA shared view actions autoEvents
	# shared = toReadOnlyShared shared
	= makeInteractiveTask (Just (SharedAbout shared)) view (Hidden,\_ _ -> Void) actions (Just autoEvents) (SharedUpdate shared)
