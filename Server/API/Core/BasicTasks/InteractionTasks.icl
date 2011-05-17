implementation module InteractionTasks

import StdList, StdBool, StdInt, StdTuple, Util, HtmlUtil, Error, StdMisc
import iTaskClass, Task, TSt, CoreTasks, CoreCombinators, Shared, ExceptionCombinators, TuningCombinators
from StdFunc		import o
from SharedTasks	import createSharedStore
from iTasks			import dynamicJSONEncode, dynamicJSONDecode
from CoreTasks		import :: InteractionPart, :: InteractionTerminators

interactLocal :: !d !(l -> [InteractionPart l]) !(l -> InteractionTerminators a) !l -> Task a | descr d & iTask l & iTask a
interactLocal d partFunc termFunc l = LocalInteractionTask @>> interact d (\l _ _ -> map toSharedRes (partFunc l)) (\l _ _ -> termFunc l) l nullShared`
where
	toSharedRes (UpdateView (formView,putback))	= UpdateView (formView,\mbV -> (putback mbV,Nothing))
	toSharedRes (Update label l)				= Update label (l,Nothing)
	toSharedRes (DisplayView v)					= DisplayView v
	
	nullShared` :: SymmetricShared Void
	nullShared` = nullShared

chooseAction :: !d !(r -> [(!Action,!Maybe a)]) !(Shared r w) -> Task a | descr d & iTask a & iTask w
chooseAction d actionsF shared = interact d (\_ _ _ -> []) (\_ r _ -> UserActions (actionsF r)) Void shared

chooseActionConst :: !d ![(!Action,a)] -> Task a | descr d & iTask a
chooseActionConst d actions = interactLocal d (const []) (const (UserActions (map (appSnd Just) actions))) Void

