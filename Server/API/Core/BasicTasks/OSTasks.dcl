definition module OSTasks
/*
* This module provides tasks to interface with the underlying OS
*/
import FilePath, Task
from File	import ::FileError(..)
from Shared	import :: ReadOnlyShared

/**
* Calls an external executable. The call is non-blocking.
*
* @param a message shown to the user while the process is running
* @param path to the executable
* @param a list of command-line arguments
* @return return-code of the process
* @throws CallException
*/
callProcess :: !FilePath ![String] -> Task (ReadOnlyShared (Maybe Int))
