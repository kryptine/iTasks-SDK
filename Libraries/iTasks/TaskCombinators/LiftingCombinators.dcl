definition module LiftingCombinators
/*
* Combinators for lifting "World" functions to the task domain.
*/
from	TSt		import :: Task, :: TSt
from	iTasks	import class iTask
import	GenPrint, GenParse, GenVisualize, GenUpdate

appWorld :: !(*World -> *World)			-> Task Void
accWorld :: !(*World -> *(!a,!*World))	-> Task a | iTask a