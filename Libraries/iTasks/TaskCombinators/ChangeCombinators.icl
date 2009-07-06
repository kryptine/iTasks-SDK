implementation module ChangeCombinators

import StdList, StdArray, StdTuple, StdMisc
import TSt, Engine

applyChangeToProcess :: !ProcessId !(Change a) !ChangeLifeTime  -> Task Void | TC a
applyChangeToProcess pid change lifetime
	= mkBasicTask "applyChangeToProcess" (\tst -> (Void, applyChangeToTaskTree pid change lifetime tst))
