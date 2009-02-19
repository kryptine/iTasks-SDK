definition module Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/

import StdEnv, StdGeneric, GenBimap

from TSt			import :: Workflow
from Http			import :: HTTPRequest, :: HTTPResponse

from iDataSettings	import ThisExe

//Global settings
applicationVersionNr			:== ThisExe +++ "_Version" 

userVersionNr thisUser			:== "User" +++ toString thisUser +++ "_VersionPNr"
usersessionVersionNr thisUser	:== "User" +++ toString thisUser +++ "_VersionSNr"

/**
* Creates the iTasks system from a set of workflow definitions
*
* @param  A list of workflow definitions
* @return A list of predicate/handler pairs that can be plugged into a server
*         or CGI wrapper
*/
engine :: [Workflow] -> [(!String -> Bool, HTTPRequest *World -> (!HTTPResponse, !*World))]