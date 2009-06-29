definition module LiftingCombinators
/*
* Some iTasks combinators for lifting other domains to the iTask domain:
*/
from TSt	import :: Task, :: TSt

import iDataFormlib

/*
appIData		:: lift iData editors to iTask domain
appIData2		:: lift iData editors to iTask domain, and pass iDataTasknumber in addition for naming convenience
*/
appIData 		:: !(IDataFun a) 								-> Task a 		| iData a
appIData2 		:: !(String *HSt -> *(!Form a,!*HSt)) 			-> Task a		| iData a 

/*
appHSt			:: lift iData *HSt domain to TSt domain. string used for tracing
*/
appHSt			:: !String !(*HSt -> (!a,!*HSt)) 				-> Task a		| iData a

/*
appWorld		:: lift *World domain to TSt domain. string used for tracing
*/
appWorld 		:: !String !(*World -> *(!a,!*World)) 			-> Task a		| iData a







