implementation module PromptingCombinators
/**
* This module contains some iTask combinators for html prompting
*/

import StdList, StdFunc
import iDataTrivial, iDataFormlib
import TSt, BasicCombinators

displayHtml	:: ![HtmlTag] -> Task a	| iCreateAndPrint a
displayHtml html = mkBasicTask "displayHtml" (Task displayTask`)
where
	displayTask` tst
		# tst = setOutput html tst
		= (createDefault, {tst & activated = False})

displayValue :: !a -> Task a | iData a
displayValue a = displayHtml [toHtml a ]

(?>>) infixr 5 	:: ![HtmlTag] !(Task a) -> Task a | iData a
(?>>) prompt task		
	= allTasksCond "?>>" displayAll (\list -> length list > 0) [("prompt",displayHtml prompt),("task",task)] =>> \list -> return_V (hd list)

(<<?) infixl 5 	:: !(Task a) ![HtmlTag] -> Task a | iData a
(<<?) task prompt
	= allTasksCond "<<?" displayAll (\list -> length list > 0) [("task",task),("prompt",displayHtml prompt)] =>> \list -> return_V (hd list)
