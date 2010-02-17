definition module Trace
/**
* This module provides functions for inspecting and visualizing
* the important datastructures of the iTasks framework
*/
from Html		import :: HtmlTag
from ProcessDB	import :: Process
from TaskTree	import :: TaskTree

import JSON

:: TraceTree =
	{ cls			:: String
	, user			:: String
	, uiProvider	:: String
	, leaf			:: Bool
	, iconCls		:: String
	, taskId 		:: String
	, taskLabel 	:: String
	, traceValue	:: String
	, taskClass		:: String
	, children		:: [TraceTree]
	}

derive JSONEncode TraceTree
derive JSONDecode TraceTree

traceProcesses 		:: [Process]		-> HtmlTag

traceTaskTree		:: TaskTree			-> TraceTree
traceTaskForest		:: [TaskTree]		-> String