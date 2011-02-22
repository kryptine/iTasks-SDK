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
:: FileException = FileException !FilePath !FileError
:: ParseException = CannotParse !String
:: CallException = CallFailed !OSError
:: DirectoryException = CannotCreate
:: SharedException = SharedException !String

derive class iTask FileException, ParseException, CallException, DirectoryException, SharedException

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