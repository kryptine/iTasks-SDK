definition module LiftingCombinators
/*
* Some iTasks combinators for lifting other domains to the iTask domain:
*/
from TSt	import :: Task, :: TSt

from	iTasks import class iTask
import	GenPrint, GenParse, GUICore

/*
appWorld		:: lift *World domain to TSt domain. string used for tracing
*/
appWorld 		:: !String !(*World -> *(!a,!*World)) 			-> Task a		| iTask a