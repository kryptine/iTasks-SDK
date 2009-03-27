definition module Trace
/**
* This module provides functions for inspecting and visualizing
* the important datastructures of the iTasks framework
*/
from Html		import :: HtmlTag
from ProcessDB	import :: Process
from TaskTree	import :: TaskTree

traceProcesses 		:: [Process]		-> HtmlTag

traceTaskTree		:: TaskTree			-> HtmlTag
traceTaskForest		:: [TaskTree]		-> HtmlTag