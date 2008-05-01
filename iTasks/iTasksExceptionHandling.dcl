definition module iTasksExceptionHandling

// *********************************************************************************************************************************
// This module contains iTask combinators for Exception Handling
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksHandler

/* 
<^>				:: Evaluate the task; An exception of type e raised by this task, will be catched by the closest handler.
				   One can use the function create a proper task value or signal the fact that an exception has occured.  
Raise 			:: Raises an exception of type e which will be catched by the closest parent handler for this type
*/

(<^>) infix  1  :: !(e -> a) !(Task a) 						-> Task a 					| iData a   & TC e			// assigns an exception Handler
Raise 			:: e 										-> Task a 					| iCreate a & TC e			// rases an exception
