definition module iTasksLiftingCombinators

// *********************************************************************************************************************************
// Some iTasks combinators for lifting other domains to the iTask domain:
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksHandler

/* 
(*>>)			:: lift functions of type (TSt -> (a,TSt)) to iTask domain 
(*#>)			:: lift functions of (TSt -> TSt) to iTask domain 
appIData		:: lift iData editors to iTask domain
appIData2		:: lift iData editors to iTask domain, and pass iDataTasknumber in addition for naming convenience
appHStOnce		:: lift iData *HSt domain to TSt domain, will be executed only once; string used for tracing
appHSt			:: lift iData *HSt domain to TSt domain, will be executed on each invocation; string used for tracing
liftHst			:: lift iData *HSt domain to the TSt domain
appWorldOnce	:: lift *World domain to TSt domain, will be executed only once; string used for tracing
appWorld		:: lift *World domain to TSt domain, will be executed on each invocation; string used for tracing
*/

(*=>) infix 4 	:: !(!TSt -> (!a,!TSt)) !(a -> Task b) 			-> Task b
(*#>) infix 4 	:: !(!TSt -> TSt)     !(Task a) 				-> Task a
appIData 		:: !(IDataFun a) 								-> Task a 		| iData a
appIData2 		:: !(!String !*HSt -> *(!Form a!,!*HSt)) 		-> Task a		| iData a 
appHStOnce 		:: !String !(!*HSt -> (!a,!*HSt)) 				-> Task a		| iData a
appHSt			:: !String !(!*HSt -> (!a,!*HSt)) 				-> Task a		| iData a
liftHst 		:: !(*HSt -> *(.a,*HSt)) !*TSt 					-> *(.a,*TSt)
appWorldOnce 	:: !String !(!*World -> *(!a,!*World)) 			-> Task a		| iData a
appWorld 		:: !String !(!*World -> *(!a,!*World)) 			-> Task a		| iData a







