implementation module MonitorTasks

import StdBool, StdTuple, StdFunc, Util, Shared, Task, TSt, InteractiveTasks
from Shared				import :: Shared

monitor :: !d !(m -> v) !(m -> Bool) !Bool !(Shared m w) -> Task m | descr d & iTask m & iTask v
monitor d view pred autoContinue shared
	= mkInteractiveTask d Monitor (makeMonitorTask shared (Just view) pred autoContinue)

monitorA :: !d !(m -> v) ![TaskAction m] !(AutoActionEvents m) !(Shared m w) -> Task (!Action,!m) | descr d & iTask m & iTask v
monitorA d view actions autoEvents shared
	= mkInteractiveTask d Monitor (makeMonitorTaskA shared (Just view) actions autoEvents)

wait :: !d !Bool !(Shared (Maybe m) w) -> Task m | descr d & iTask m
wait d autoContinue shared
	= mapTask fromJust (mkInteractiveTask d Monitor (makeMonitorTask shared noView isJust autoContinue))

makeMonitorTask :: !(Shared m w) !(Maybe (m -> v)) !(m -> Bool) !Bool -> TaskFunctions m | iTask m & iTask v
makeMonitorTask shared mbView pred autoContinue
	= mapTaskFunctions snd (makeMonitorTaskA shared mbView actions autoEvents)
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
		
makeMonitorTaskA	:: !(Shared m w) !(Maybe (m -> v)) ![TaskAction m] !(AutoActionEvents m) -> TaskFunctions (!Action,!m) | iTask m & iTask v
makeMonitorTaskA shared` mbView actions autoEvents = case mbView of
	Just view	= mapTaskFunctions (appSnd fromJust) (makeInteractiveTask (Just (SharedAbout shared)) view (const (Hidden Void),\_ _ -> Void) actions (Just autoEvents) (SharedUpdate shared))
	Nothing		= mapTaskFunctions (appSnd fromJust) (makeInteractiveTask noAbout id (const (Hidden Void),\_ _ -> Void) actions (Just autoEvents) (SharedUpdate shared))
where
	shared = toReadOnlyShared shared`
	
noView :: Maybe (a -> Void)
noView = Nothing

noAbout :: Maybe (About Void)
noAbout = Nothing