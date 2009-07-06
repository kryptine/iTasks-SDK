definition module ChangeCombinators
/**
* This module defines combinators for manipulating the changelist of a running workflow process.
*/

from TSt			import ::Task, ::TSt, ::Change, ::ChangeLifeTime
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
applyChangeToProcess :: !ProcessId !(Change a) !ChangeLifeTime  -> Task Void | TC a