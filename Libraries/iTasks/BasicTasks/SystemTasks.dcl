definition module SystemTasks
/**
* This module provides tasks for interacting with the iTasks engine
*/
from TSt		import :: Task
from UserDB		import :: User
/**
* Returns the user currently logged in the iTask system
*/
getCurrentUser ::  Task User