definition module iTasks.WF.Tasks.SDS
/**
* This module provides the core tasks for accessing shared data sources.
*/
from iTasks.WF.Definition import :: Task
import iTasks.SDS.Definition

/**
* Reads shared data once.
*
* @param Shared: A shared reference
* @return The value read
* @throws SharedException
*/
get :: !(sds () a w) -> Task a | TC a & Readable sds & TC w

/**
* Writes shared data.
*
* @param Value: A value to write
* @param Shared: A shared reference
* @return The value written
* @throws SharedException
*/
set :: !a !(sds () r a)  -> Task a | TC a & TC r & Writeable sds

/**
* Updates shared data in one atomic operation.
*
* @param Shared: A shared reference
* @param Update function: A function modifying the shared value
* @return The value written
* @throws SharedException
*/
upd :: !(r -> w) !(sds () r w) -> Task w | TC r & TC w & RWShared sds

/**
* Reads shared data continously
*
* @param Shared: A shared reference
* @return The value read
* @throws SharedException
*/
watch :: !(sds () r w) -> Task r | TC r & TC w & Readable, Registrable sds
