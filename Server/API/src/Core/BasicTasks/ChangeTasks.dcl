definition module ChangeTasks
/**
* This module defines combinators for manipulating the changelist of a running workflow process.
*/

from TSt		import ::Task, ::TSt, ::ChangeLifeTime, :: ChangeDyn
from Types		import ::ProcessId
from Void		import :: Void

/**
* Administer a change to another (running) workflow process
*
* @param A process id
* @param The change
* @param The change's lifetime
*
* @return The task that will do the change
*/
applyChangeToProcess :: !ProcessId !ChangeDyn !ChangeLifeTime  -> Task Void