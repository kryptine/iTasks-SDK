definition module FIXMEDebug
/**
* FIXME: This module's name clashes with a module named Debug in the Esther
* libraries which we include for some reason
*
* This module provides functions for inspecting and visualizing
* the important datastructures of the iTasks framework
*/
import Html
from ProcessDB	import :: Process
from TaskTree	import :: HtmlTree, :: TaskTree


traceProcesses 		:: [Process]		-> HtmlTag

traceTaskTree		:: HtmlTree			-> HtmlTag
traceTaskTree2		:: TaskTree			-> HtmlTag

traceTaskForest		:: [HtmlTree]		-> HtmlTag
traceTaskForest2	:: [TaskTree]		-> HtmlTag