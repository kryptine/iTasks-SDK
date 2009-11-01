definition module SystemTasks
/**
* This module provides tasks for interacting with the iTasks engine
*/
from TSt		import :: Task
from UserDB		import :: User

from iTasks		import class iTask
import GenPrint, GenParse, GenVisualize, GenUpdate

/**
* Returns the user currently logged in the iTask system
*/
getCurrentUser ::  Task User

/**
* Get default value
*/
getDefaultValue :: Task a | iTask a