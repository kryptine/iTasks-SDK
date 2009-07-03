implementation module ChangeCombinators

import StdList, StdArray, StdTuple, StdMisc
import TSt, Engine

applyChangeToProcess :: !ProcessId !String !(Change a)  -> Task Void | TC a
applyChangeToProcess pid label change
	= mkBasicTask "applyChangeToProcess" (\tst -> (Void, applyChangeToTaskTree pid label change tst))
