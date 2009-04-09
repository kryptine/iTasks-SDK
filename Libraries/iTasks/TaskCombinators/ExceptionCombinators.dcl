definition module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/

from TSt	import :: Task, ::TSt, ::ChangeCondition(..)
from iDataSettings	import class iCreateAndPrint, class iParse, class iSpecialStore, class iData

import iDataForms


/**
* Change is possible combinator.
*
* @param The normal task can be replaced by another one
* @param The exception handling task which gets the exception as parameter
* @return The combined task
*/

(<\/>) infixl  1  :: !(Task a) !(e -> Task a) 	-> Task a 	| iData a & iData e

/**
* Change raising. This will raise an exception of arbitrary type e which has to be caught
* by a lower level change handler combinator.
*
* @param The exception value
* @return The combined task
*/
pushChangeRequest :: !ChangeCondition  !e !(Task a) -> Task a | iData a & TC e	

/**
* Exception combinator.
*
* @param The normal task which will possibly raise an exception of type e
* @param The exception handling task which gets the exception as parameter
* @return The combined task
*/
(<^>) infixl  1  :: !(Task a) !(e -> Task a) 	-> Task a 	| iData a & iData e

/**
* Exception raising. This will raise an exception of arbitrary type e which has to be caught
* by a higher level exception handler combinator.
*
* @param The exception value
* @return The combined task
*/
raise 			:: !e 									-> Task a 	| iData a & TC e