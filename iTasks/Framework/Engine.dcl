definition module Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/

import StdEnv, StdGeneric, StdBimap

from TSt			import :: Workflow
from iDataSettings	import ThisExe

//Global settings
iTaskVersion 					:== "2.0 - Januari 2009 - "

traceId							:== "User_Trace" 
refreshId						:== "User_refresh"
applicationVersionNr			:== ThisExe +++ "_Version" 

userVersionNr thisUser			:== "User" +++ toString thisUser +++ "_VersionPNr"
usersessionVersionNr thisUser	:== "User" +++ toString thisUser +++ "_VersionSNr"

/**
* Starts the task engine with a list of workflow definitions.
*
* @param A list of available workflows
* @param The world
* @return The world
*/
startEngine :: ![Workflow] !*World -> *World