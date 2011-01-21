definition module OSTasks
/*
* This module provides tasks to interface with the underlying OS
*/
import Types
from Directory import :: Path(..), ::DiskName, :: PathStep(..)

// Appends a list of path steps to a path.
(+<) infixr 5 :: !Path	![PathStep]	-> Path

// Exceptions
:: FileException = FileException !String !FileProblem
:: FileProblem = CannotOpen | CannotClose | IOError
:: CallException = CallFailed !String
:: DirectoryException = CannotCreate

derive class iTask Path, FileException, FileProblem, CallException, DirectoryException

/**
* Generate a platform dependent string representation of a path.
*
* @param a path
* @return the path's string representation
*/
pathToPDString :: !Path -> Task String

/**
* Calls an external executable. The call is non-blocking.
*
* @param a message shown to the user while the process is running
* @param path to the executable
* @param a list of command-line arguments
* @return return-code of the process
* @throws CallException
*/
callProcess :: !message !Path ![String] -> Task Int | html message

/**
* Calls an external executable. The call is blocking and should only
* be used for executables terminating within a very short time.
*
* @param path to the executable
* @return return-code of the process
* @param a list of command-line arguments
* @throws CallException
*/
callProcessBlocking :: !Path ![String] -> Task Int

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
* Returns the path of the directory including the iTasks-server executable.
* A platform dependent string representation is generated.
*
* @return path of the directory including the iTasks-server executable
*/
getAppPath :: Task String
