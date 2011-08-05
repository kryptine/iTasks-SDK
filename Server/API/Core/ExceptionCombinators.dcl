definition module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/

from Task		import :: Task
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
:: WorkOnException	= WorkOnNotFound | WorkOnEvalError | WorkOnDependencyCycle

derive class iTask	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException
instance toString	FileException, ParseException, CallException, SharedException, RPCException, OSException, WorkOnException

/**
* Exception combinator.
*
* @param Task: The normal task which will possibly raise an exception of type e
* @param Handler: The exception handling task which gets the exception as parameter
* @return The combined task
*
* @gin-title Try block
* @gin-icon catch
*/
try 		:: !(Task a) (e -> Task a) 			-> Task a 	| iTask a & iTask, toString e

/**
* Exception throwing. This will throw an exception of arbitrary type e which has to be caught
* by a higher level exception handler combinator.
*
* @param Value: The exception value
* @return The combined task
* 
* @gin-title Raise exception
* @gin-icon error
*/
throw		:: !e 								-> Task a 	| iTask a & iTask, toString e

/**
* Catches all exceptions.
*
* @param Task: The normal task which will possibly raise an exception of any type
* @param Handler: The exception handling task
* 
* @gin-title Catch all exceptions
* @gin-icon catch
*/
catchAll	:: !(Task a) (String -> Task a)		-> Task a | iTask a
