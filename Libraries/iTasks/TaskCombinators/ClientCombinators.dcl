definition module ClientCombinators
/**
* This modules provides combinators to delegate pieces of work to be
* executed in the browser. This is achieved by moving part of the
* workflow code to an interpreter running in the client framework
*/

from TSt	import :: Task
from Types	import :: EvaluationOption

from iTasks	import class iTask
import GenPrint, GenParse, GenVisualize, GenUpdate

class (@>>) infixl 7 b ::  !b !(Task a) -> Task a | iTask a	
instance @>>		  EvaluationOption				
