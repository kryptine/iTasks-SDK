definition module LiftingCombinators
/*
* Some iTasks combinators for lifting other domains to the iTask domain:
*/
from TSt	import :: Task, :: TSt

import iDataFormlib

/* 
(*>>)			:: lift functions of type (TSt -> (a,TSt)) to iTask domain 
(*#>)			:: lift functions of (TSt -> TSt) to iTask domain 
*/
(*=>) infix 4 	:: !(TSt -> (!a,!TSt)) !(a -> Task b) 			-> Task b
(*#>) infix 4 	:: !(TSt -> TSt)     !(Task a) 					-> Task a

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







