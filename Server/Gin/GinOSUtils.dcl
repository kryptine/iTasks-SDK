definition module GinOSUtils

import Void

:: Path :== String

// Exceptions
:: FileException = FileException !Path !FileProblem
:: FileProblem = CannotOpen | CannotClose | IOError
:: CallException = CallFailed !Path

:: OSResult a b = OSOK a | OSError b

isOSError :: (OSResult a b) -> Bool
getOSResult :: (OSResult a b) -> a
getOSError :: (OSResult a b) -> b

formatOSError :: (OSResult a b) -> String | toString b

instance toString FileException
instance toString FileProblem
instance toString CallException

/**
* Calls an external executable. The call is blocking and should only
* be used for executables terminating within a very short time.
*
* @param path to the executable
* @return return-code of the process
*/
osCallProcessBlocking :: !Path *World -> (OSResult Int CallException, *World)

/**
* Reads a textfile from disc.
*
* @param path to the file
* @return content of the file
*/
osReadTextFile :: !Path *World -> (OSResult String FileException, *World)

/**
* Writes string to a textfile.
*
* @param path to the file
* @param string written to file
*/
osWriteTextFile :: !String !Path *World -> (OSResult Void FileException, *World)

//Path utils
appendTrailingSeparator :: !Path -> Path
(+/+) infixr 5 :: !Path !Path -> Path
quote :: String -> String

