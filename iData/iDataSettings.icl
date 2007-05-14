implementation module iDataSettings

import iDataHandler, ArgEnv, StdList, iDataTrivial, StdArray


// global names setting depending on kind of server used

ThisExe ::  String
ThisExe = myName 

myName =: toString (reverse (takeWhile (\c -> (<>) c '\\') (drop 4 (reverse [x \\ x <-: getCommandLine.[0]])))) // +++ " iTasks"



MyDir ::  String
MyDir  =: mkString (takeWhile ((<>) '.') (mkList ThisExe))


MyAbsDir ::  String
MyAbsDir  =:  toString (reverse (dropWhile (\c -> (<>) c '\\') (drop 4 (reverse [x \\ x <-: getCommandLine.[0]]))))
