definition module OSTasks

import iTasks

:: Path :== String

// Exceptions
:: FileException = FileException !Path !FileProblem
:: FileProblem = CannotOpen | CannotClose | IOError
:: CallException = CallFailed !Path
:: DirectoryException = CannotCreate

derive gPrint		FileException, FileProblem, CallException, DirectoryException
derive gParse		FileException, FileProblem, CallException, DirectoryException
derive gVisualize	FileException, FileProblem, CallException, DirectoryException
derive gUpdate		FileException, FileProblem, CallException, DirectoryException
derive gHint		FileException, FileProblem, CallException, DirectoryException
derive gError		FileException, FileProblem, CallException, DirectoryException

/**
* Calls an external executable. The call is non-blocking.
*
* @param path to the executable
* @return return-code of the process
* @throws CallException
*/
callProcess :: !Path ->	Task Int

/**
* Calls an external executable. The call is blocking and should only
* be used for executables terminating within a very short time.
*
* @param path to the executable
* @return return-code of the process
* @throws CallException
*/
callProcessBlocking :: !Path -> Task Int

/**
* Reads a textfile from disc.
*
* @param path to the file
* @return content of the file
* @throws FileException
*/
readTextFile :: !Path -> Task String

/**
* Writes string to a textfile.
*
* @param path to the file
* @param string written to file
* @throws FileException
*/
writeTextFile :: !String !Path -> Task Void

/**
* Checks if a file exists.
*
* @param path to the file
* @return True if file exists
*/
fileExists :: !Path -> Task Bool

/**
* Checks if a file is a directory.
*
* @param path to the file
* @return True if file exists and is a directory
*/
isDirectory :: !Path -> Task Bool

/**
* Creates a directory.
*
* @param path of directory to create; only final dir in path is created, intermediate ones have to exist
* @throws DirectoryException
*/
createDirectory :: !Path -> Task Void

/**
* Returns the path of the iTasks-server executable.
*
* @return path of the iTasks-server executable
*/
getAppPath :: Task String
