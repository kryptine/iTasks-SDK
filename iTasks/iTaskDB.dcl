definition module iTaskDB

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

readDB2 :: read the database, each and everytime the application is evaluated
			dangerous: not referential transparent, only use it if you know what you are doing !
*/

mkDBid 	:: String Lifespan -> (DBid a)

readDB	:: (DBid a) 	-> Task a | iData a
writeDB	:: (DBid a) a 	-> Task a | iData a

readDB2	:: (DBid a) 	-> Task a | iData a
