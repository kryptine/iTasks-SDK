definition module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/

from TSt		import :: Task
from FilePath	import :: FilePath
from File		import :: FileError
from OSError	import :: OSError, :: OSErrorMessage, :: OSErrorCode
import iTaskClass, GenVisualize, GenUpdate

// some predefined exception types used by library tasks
:: FileException	= FileException !FilePath !FileError
:: ParseException	= CannotParse !String
:: CallException	= CallFailed !OSError
:: SharedException	= SharedException !String
:: RPCException		= RPCException !String
:: OSException		= OSException !OSError

instance toString FileException, ParseException, CallException, SharedException, RPCException, OSException

/**
* Exception combinator.
*
* @param The normal task which will possibly raise an exception of type e
* @param The exception handling task which gets the exception as parameter
* @return The combined task
*/
try 		:: !(Task a) (e -> Task a) 			-> Task a 	| iTask a & TC, toString e

/**
* Exception throwing. This will trough an exception of arbitrary type e which has to be caught
* by a higher level exception handler combinator.
*
* @param The exception value
* @return The combined task
*/
throw		:: !e 								-> Task a 	| iTask a & TC, toString e

/**
* Catches all exceptions.
*
* @param The normal task which will possibly raise an exception of any type
* @param The exception handling task
*/
catchAll	:: !(Task a) (String -> Task a)		-> Task a | iTask a
