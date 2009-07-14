implementation module ChangeCombinators

import StdList, StdArray, StdTuple, StdMisc
import TSt, Engine

applyChangeToProcess :: !ProcessId !Dynamic !ChangeLifeTime  -> Task Void
applyChangeToProcess pid change lifetime
	= mkBasicTask "applyChangeToProcess" (\tst -> (Void, applyChangeToTaskTree pid change lifetime tst))