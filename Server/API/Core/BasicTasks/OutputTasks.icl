implementation module OutputTasks

import StdTuple, StdList, StdOrdList, StdBool, StdMisc, Functor
import Types, Util, TSt, ExceptionCombinators, CoreTasks, InteractionTasks, CoreCombinators, CommonCombinators, SystemData
from StdFunc 		import id, const, o
from SharedTasks	import sharedStore, :: SharedStoreId
from SharedTasks	import readShared, writeShared

//Local output
showMessage :: !d !a -> Task a | descr d & iTask a
showMessage d a = showMessage` d a noView

showMessageA :: !d ![Action] !a -> Task (!Action,!a) | descr d & iTask a
showMessageA d actions a = showMessageA` d actions a noView

showMessageAbout :: !d !about -> Task about | descr d & iTask about
showMessageAbout d about = showMessage` d about (Just id)

showMessageAboutA :: !d !(about -> v) ![Action]	!about -> Task (!Action,!about) | descr d & iTask about & iTask v
showMessageAboutA d aboutView actions about = showMessageA` d actions about (Just aboutView)

showMessage` d a mbAboutView = OutputTask PassiveOutput @>> interactLocal d (msgAboutView a mbAboutView) (const (okAction (Just a))) Void
showMessageA` d actions a mbAboutView
	= OutputTask PassiveOutput @>> interactLocal d (msgAboutView a mbAboutView) (const (UserActions (map (\action -> (action,Just (action,a))) actions))) Void
msgAboutView a mbAboutView _ = maybe [] (\view -> [DisplayView (view a)]) mbAboutView

showInstruction :: !String !instruction	!a -> Task a | html instruction & iTask a
showInstruction title instr a = showInstruction` (title,instr) a noView

showInstructionAbout :: !String !instruction !about -> Task about | html instruction & iTask about
showInstructionAbout title instr about = showInstruction` (title,instr) about (Just id)

showInstruction` d a mbAboutView = OutputTask ActiveOutput @>> interactLocal d (msgAboutView a mbAboutView) (const (okAction (Just a))) Void

// Monitor (shared output) tasks
monitor :: !d !(r -> v) !(r -> Bool) !Bool !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w
monitor d view pred autoContinue shared = monitor` d (Just view) pred autoContinue id voidNothing shared

monitorA :: !d !(r -> v) !(r -> Bool) ![PredAction r] !(Shared r w) -> Task (!Maybe Action,!r) | descr d & iTask r & iTask v & iTask w
monitorA d view pred actions shared = monitorA` d view pred actions voidNothing shared

monitorAbout :: !d !(r -> v) !(r -> Bool) !Bool !about !(Shared r w) -> Task r | descr d & iTask r & iTask v & iTask w & iTask about
monitorAbout d view pred autoContinue about shared = monitor` d (Just view) pred autoContinue id (Just about) shared

monitorAboutA :: !d !(r -> v) !(r -> Bool) ![PredAction r] !about !(Shared r w) -> Task (!Maybe Action,!r) | descr d & iTask r & iTask v & iTask w & iTask about
monitorAboutA d view pred actions about shared = monitorA` d view pred actions (Just about) shared

wait :: !d !(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w
wait d shared = monitor` d noView isJust True fromJust voidNothing shared

waitAbout :: !d !about !(Shared (Maybe r) w) -> Task r | descr d & iTask r & iTask w & iTask about
waitAbout d about shared = monitor` d noView isJust True fromJust (Just about) shared

waitUntil :: !d !(r -> Bool) !(Shared r w) -> Task r | descr d & iTask r & iTask w
waitUntil d pred shared = monitor` d noView pred True id voidNothing shared

waitUntilAbout :: !d !(r -> Bool) !about !(Shared r w) -> Task r | descr d & iTask r & iTask w & iTask about
waitUntilAbout d pred about shared = monitor` d noView pred True id (Just about) shared

monitor` d mbView pred autoContinue transF mbAbout shared
	= OutputTask PassiveOutput @>> interact d (maybe (\_ _ _ -> []) (\view _ m _ -> addAbout mbAbout [DisplayView (view m)]) mbView) termFunc Void shared
where
	termFunc _ m _
		| autoContinue
			| continue
				= StopInteraction (transF m)
			| otherwise
				= UserActions []
		| otherwise
			= UserActions [(ActionContinue,if continue (Just (transF m)) Nothing)]
	where
		continue = pred m
		
monitorA` d view pred actions mbAbout shared
	= OutputTask PassiveOutput @>> interact
		d
		(\_ r _ -> addAbout mbAbout [DisplayView (view r)])
		(\_ r _ -> if (pred r) (StopInteraction (Nothing,r)) ((fromPredActionsLocal id (\action r -> (Just action,r)) actions) r))
		Void
		shared

waitForTime :: !Time -> Task Time
waitForTime time =
		waitUntil ("Wait for time", ("Wait until " +++ toString time)) pred sharedCurrentTime
where	
	pred now = time < now

waitForDate :: !Date -> Task Date
waitForDate date =
		waitUntil ("Wait for date", ("Wait until " +++ toString date)) pred sharedCurrentDate
where
	pred now = date < now

waitForTimer :: !Time -> Task Time
waitForTimer time = readShared sharedCurrentTime >>= \now -> waitForTime (now + time)

noView :: Maybe (a -> Void)
noView = Nothing