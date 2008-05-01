definition module iTasksDB

// *********************************************************************************************************************************
// iTasks for easy database creation and access - based on iData
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksHandler

db_prefix 		:== "iDBase-"

:: DBid a

/*
mkDBid	:: create a typed database identificator; only Database and TxtFile are currently supported
readDB	:: read the database
writeDB :: write the database
*/

mkDBid 	:: !String !Lifespan -> (DBid a)

readDB	:: !(DBid a) 		-> Task a | iData a
writeDB	:: !(DBid a) !a 	-> Task a | iData a
