definition module EngineWrapperStandalone
/**
* This module wraps the iTasks engine in a simple
* standalone web server. This allows for easy testing and playing
* with the system
*/
import iTaskClass
from Engine import class Publishable, instance Publishable (Task a) | iTask a, instance Publishable [PublishedTask], :: PublishedTask

/**
* Starts the task engine with a list of workflow definitions.
*
* @param A task to start
* @param The world
* @return The world
*/
startEngine :: a !*World -> *World | Publishable a


