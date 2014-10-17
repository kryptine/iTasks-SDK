definition module Incidone.Simulation.TrainingTasks
import iTasks
/**
* This module provides support for controlling the incidone application during
* exercises, experiments and demonstrations.
*
* It enables a facilitator to inject fake calls and messages into the system.
*
* It should in the future also be possible to fake sensors such as AIS positions
* for the contacts used in the exercise
*/


/**
* This toplevel task allows a facilitator to control an exercise.
*/
controlExercise :: Task ()
