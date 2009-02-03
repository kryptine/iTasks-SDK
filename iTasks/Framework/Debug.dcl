definition module Debug
/**
* This module provides functions for inspecting and visualizing
* the important datastructures of the iTasks framework
*/
import Html
from ProcessDB	import :: Process
from TaskTree	import :: TaskTree

traceProcesses 		:: [Process]		-> HtmlTag

traceTaskTree		:: TaskTree			-> HtmlTag
traceTaskForest		:: [TaskTree]		-> HtmlTag