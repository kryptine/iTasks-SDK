definition module LiftingCombinators
/*
* Combinators for lifting "World" functions to the task domain.
*/
from	TSt		import :: Task, :: TSt
from	iTasks	import class iTask
import	GenVisualize, GenUpdate

/**
* Evaluate a "World" function that does not yield any result once.
*
* @param The function to evaluate
*
* @param A Void task that evaluates the function
*/
appWorld :: !(*World -> *World)			-> Task Void

/**
* Evaluate a "World" function that also returns a value once.
*
* @param The function to evaluate
*
* @param A Void task that evaluates the function
*/
accWorld :: !(*World -> *(!a,!*World))	-> Task a | iTask a