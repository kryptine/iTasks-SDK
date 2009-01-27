definition module ExceptionCombinators
/**
* This module contains iTask combinators for Exception Handling
*/

import TSt

/* 
<^>				:: Evaluate the task; An exception of type e raised by this task, will be catched by the closest handler.
				   One can use the function create a proper task value or signal the fact that an exception has occured.  
raise 			:: Raises an exception of type e which will be catched by the closest parent handler for this type
*/

(<^>) infix  1  :: !(e -> a) !(Task a) 						-> Task a 					| iData a   & TC e			// assigns an exception Handler
raise 			:: e 										-> Task a 					| iCreate a & TC e			// rases an exception
