definition module MonitorTasks

// This module provides tasks for monitoring a shared state.

import Task
from Shared				import :: Shared
from InteractiveTasks	import :: TaskAction, :: ActionEvent, :: AutoActionEvents, :: Verified

/**
* Monitor a shared state using a functional view.
* A predicate determines when to continue.
*
* @param A view function
* @param A predicate determining when to continue
* @param A flag indicating if to finish the task automatically if condition is true or let the user press a continue-button
* @param A reference to the shared state
* @return The last value of the monitored state
*/
monitorTask		:: !(m -> v) !(m -> Bool) !Bool !(Shared m w) -> Task m | iTask m & iTask v

/**
* Monitor a shared state using a functional view.
* A function generating automatically triggered action events and a list of actions can be provided.
*
* @param A view function
* @param A list of actions
* @param A function generating auto events
* @param A reference to the shared state
* @return The last value of the monitored state + the generated action event
*/
monitorTaskA	:: !(m -> v) ![TaskAction m] !(AutoActionEvents m) !(Shared m w) -> Task (!ActionEvent,!Maybe m) | iTask m & iTask v
