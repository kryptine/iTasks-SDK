definition module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/

from TSt	import :: Task
from iDataSettings	import class iCreateAndPrint, class iParse, class iSpecialStore, class iData

import iDataForms

/**
* Exception combinator.
*
* @param The normal task which will possibly raise an exception of type e
* @param The exception handling task which gets the exception and the normal task as parameters
* @return The combined task
*/
(<^>) infixl  1  :: !(Task a) !(e (Task a) -> Task a) 	-> Task a 	| iData a & iData e

/**
* Exception raising. This will raise an exception of arbitrary type e which has to be caught
* by a higher level exception handler combinator.
*
* @param The exception value
* @return The combined task
*/
raise 			:: !e 									-> Task a 	| iData a & TC e