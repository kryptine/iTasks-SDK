definition module LiftingCombinators
/*
* Combinators for lifting "World" functions to the task domain.
*/

import Task
from Error import ::MaybeError(..)
from OSError import ::MaybeOSError, ::OSError, ::OSErrorCode, ::OSErrorMessage
from TSt import :: TSt

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

/**
* Evaluate a "World" function that also returns a MaybeError value.
* If the MaybeError value is Error, the error is transformed.
* @param The function to evaluate
* @param Error transformation function
*
* @param A Void task that evaluates the function
*/
accWorldError   :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err

accWorldOSError :: !(*World -> (!MaybeOSError a, !*World))             -> Task a | iTask a
