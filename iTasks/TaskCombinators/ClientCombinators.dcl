definition module ClientCombinators
/**
* This modules provides combinators to delegate pieces of work to be
* executed in the browser. This is achieved by moving part of the
* workflow code to an interpreter running in the client framework
*/

import TSt

class (@>>) infixl 7 b ::  !b !(Task a) -> Task a | iData a	
instance @>>		  EvaluationOption				
