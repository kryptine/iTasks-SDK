definition module ChangeCombinators
/**
* This module defines combinators for manipulating the changelist of a running workflow process.
*/

from TSt			import ::Task, ::TSt, ::Change(..),  ::ChangeCondition(..), ::ChangeResult
from Types			import ::ProcessId
from TaskTree		import ::TaskProperties
from iDataSettings	import class iCreateAndPrint, class iParse, class iSpecialStore, class iData
import iDataForms

/**
* Administer a change to another (running) workflow process
*
* @param A process id
* @param A label for identifying the change
* @param The change
*
* @return The task that will do the change
*/
applyChangeToProcess :: !ProcessId !String !(Change a)  -> Task Void | TC a


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
