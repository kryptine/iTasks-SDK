implementation module ChangeTasks

import StdList, StdArray, StdTuple, StdMisc
import TSt

applyChangeToProcess :: !ProcessId !Dynamic !ChangeLifeTime  -> Task Void
applyChangeToProcess pid change lifetime
	= mkInstantTask "applyChangeToProcess" (\tst -> (Void, applyChangeToTaskTree pid change lifetime tst))