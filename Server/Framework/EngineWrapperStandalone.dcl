definition module EngineWrapperStandalone
/**
* This module wraps the iTasks engine in a simple
* standalone web server. This allows for easy testing and playing
* with the system
*/
import Engine

/**
* Starts the task engine with a list of workflow definitions.
*
* @param A task to start
* @param The world
* @return The world
*/
startEngine :: (Task a) !*World -> *World | iTask a
