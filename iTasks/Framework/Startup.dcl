definition module Startup

import iDataSettings, StdBimap
import BasicCombinators

/**
* Starts the task engine with a single "main" workflow definition.
*
* @param A task which will be started as main task
* @param The user id of the user to whom the main task will be assigned
* @param The world
* @return The world
*/
startTaskEngine :: !(LabeledTask a) !Int !*World -> *World  	| iData a