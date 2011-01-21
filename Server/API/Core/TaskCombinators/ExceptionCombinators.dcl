definition module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/

from TSt import :: Task, class iTask
import GenVisualize, GenUpdate

/**
* Exception combinator.
*
* @param The normal task which will possibly raise an exception of type e
* @param The exception handling task which gets the exception as parameter
* @return The combined task
*/
try  :: !(Task a) !(e -> Task a) 	-> Task a 	| iTask a & iTask e

/**
* Exception throwing. This will trough an exception of arbitrary type e which has to be caught
* by a higher level exception handler combinator.
*
* @param The exception value
* @return The combined task
*/
throw 			:: !e 				-> Task a 	| iTask a & TC e