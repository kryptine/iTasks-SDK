implementation module PromptingCombinators
/**
* This module contains some iTask combinators for html prompting
*/

import StdList
import TSt, UITasks, BasicCombinators, TuningCombinators

(?>>) infixr 5 	:: ![HtmlTag] !(Task a) -> Task a | iData a
(?>>) prompt task		
	= 				(allTasksCond "?>>" (\list -> length list > 0) [("prompt",displayHtml prompt),("task",task)]) 	<<@ TTVertical
	>>= \list ->	return (hd list)

(<<?) infixl 5 	:: !(Task a) ![HtmlTag] -> Task a | iData a
(<<?) task prompt
	= 				(allTasksCond "<<?" (\list -> length list > 0) [("task",task),("prompt",displayHtml prompt)]) 	<<@ TTVertical
	>>= \list ->	return (hd list)
