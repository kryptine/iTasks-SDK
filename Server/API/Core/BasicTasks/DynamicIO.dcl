definition module DynamicIO
/**
* This module defines task for reading and writing dynamics to and from files
* 
* WARNING: this will only work if the application is compiled with the "Enable dynamics" project options set !!!
*/

import 	StdDynamic
import iTasks

/**
* Writes a task as dynamic to a file
*
* @param The file name
* @param The task
*
* @return Boolean indicating succesful write
*/
writeDynamicTask 	:: !String !(Task a) 	-> Task Bool 			| iTask a
/**
* Read a dynamic (possibly created by another application) from a file
*
* @param The file name
*
* @return Boolean indicating succesful read
* @return The task
*/
readDynamicTask 	:: !String 				-> Task (Bool,Task a) 	| iTask a
