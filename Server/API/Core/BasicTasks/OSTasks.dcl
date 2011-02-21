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
callProcessWait :: !message !FilePath ![String] -> Task Int | iTask message

/**
* Calls an external executable. The call is blocking and should only
* be used for executables terminating within a very short time.
*
* @param path to the executable
* @return return-code of the process
* @param a list of command-line arguments
* @throws CallException
*/
callProcessBlocking :: !FilePath ![String] -> Task Int

callProcess :: !FilePath ![String] -> Task (ReadOnlyShared (Maybe Int))

/**
* Checks if a file exists.
*
* @param path to the file
* @return True if file exists
*/
fileExists :: !FilePath -> Task Bool

/**
* Checks if a file is a directory.
*
* @param path to the file
* @return True if file exists and is a directory
*/
//isDirectory :: !Path -> Task Bool

/**
* Creates a directory.
*
* @param path of directory to create; only final dir in path is created, intermediate ones have to exist
* @throws DirectoryException
*/
//createDirectory :: !Path -> Task Void
