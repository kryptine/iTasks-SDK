implementation module PromptingCombinators
/**
* This module contains some iTask combinators for html prompting
*/

import StdList, StdFunc
import TSt, EditTasks, BasicCombinators

(?>>) infixr 5 	:: ![HtmlTag] !(Task a) -> Task a | iData a
(?>>) prompt task		
	= allTasksCond "?>>" TTVertical (\list -> length list > 0) [("prompt",displayHtml prompt),("task",task)] =>> \list -> return_V (hd list)

(<<?) infixl 5 	:: !(Task a) ![HtmlTag] -> Task a | iData a
(<<?) task prompt
	= allTasksCond "<<?" TTVertical (\list -> length list > 0) [("task",task),("prompt",displayHtml prompt)] =>> \list -> return_V (hd list)
