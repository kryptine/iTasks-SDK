definition module iTasksTypes

// *********************************************************************************************************************************
// This module contains the global type definition as visable for the iTask end user
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//

import InternaliTasksCommon

// ******************************************************************************************************
// iTask options + overloaded operators to set the options
// iTasks inherits the following options from the iData library: Lifespan, SorageFormat, Mode
// iTasks have as additional option: GarbageCollect and EvaluationOption (default is on server)
// ******************************************************************************************************


class (<<@) infixl 3 b :: !(Task a) !b 	-> Task a 	 
instance <<@		  Lifespan						// default: Session
					, StorageFormat					// default: PlainString
					, Mode							// default: Edit
					, GarbageCollect				// default: Collect

:: EvaluationOption	= UseAjax  						// use Ajax technology to update part of a page, only works if Ajax enabled 
					| OnClient 						// use SAPL to update part of a page on the client, only works if Client enabled and Sapl is running...

class (@>>) infixl 7 b ::  !b !(Task a) -> Task a | iData a	
instance @>>		  EvaluationOption						// default: the *whole* page will be updated when a form has been modified


