definition module iTasksDB

//	super simple database creation and access based on iData
// (c) mjp 2007 

// choose the kind of storage you want to use

db_prefix 		:== "iDBase-"

:: DBid a

import iTasks

/*
mkDBid	:: create a typed database identificator
			only Database and TxtFile are currently supported
readDB	:: read the database
writeDB :: write the database
*/

mkDBid 	:: String Lifespan -> (DBid a)

readDB	:: (DBid a) 	-> Task a | iData a
writeDB	:: (DBid a) a 	-> Task a | iData a

