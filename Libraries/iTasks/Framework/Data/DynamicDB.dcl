definition module DynamicDB
/**
* This module provides an abstract database to store Dynamics.
* Given the nature of dynamics being executable code they need some special
* treatment when stored and loaded. Therefore these are stored in an external "database"
*/

import StdMaybe
import Types, HSt

class DynamicDB st
where
	createDynamic	:: !Dynamic 				!*st -> (!DynamicId,!*st)
	updateDynamic	:: !Dynamic !DynamicId		!*st -> (!Bool, !*st)
	deleteDynamic	:: !DynamicId				!*st -> (!Bool, !*st)
	deleteDynamics	:: ![DynamicId]				!*st -> (!Bool, !*st) //Mass-delete for garbage collection purposes
	getDynamic		:: !DynamicId				!*st -> (!Maybe Dynamic, !*st)

instance DynamicDB HSt
