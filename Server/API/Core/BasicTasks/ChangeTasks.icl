implementation module ChangeTasks

import StdList, StdArray, StdTuple, StdMisc
import TSt

applyChangeToProcess :: !ProcessId !ChangeDyn !ChangeLifeTime  -> Task Void
applyChangeToProcess pid change lifetime
	= mkInstantTask "Apply a change to a process" ("Apply a " +++ lt +++ " change to task " +++ pid)
		(\tst -> (TaskFinished Void, applyChangeToTaskTree pid (lifetime,change) tst))
where
	lt = case lifetime of
		CLTransient = "transient"
		CLPersistent _	= "persistent"
		