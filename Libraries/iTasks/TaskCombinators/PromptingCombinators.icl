implementation module PromptingCombinators
/**
* This module contains some iTask combinators for html prompting
*/

import StdList
import TSt, UITasks, BasicCombinators, TuningCombinators

(?>>) infixr 5 	:: ![HtmlTag] !(Task a) -> Task a | iData a
(?>>) prompt task = parallel "?>>" (\list -> length list > 0) (\_ [x:xs] -> x) [("prompt",displayHtml prompt),("task",task)] <<@ TTVertical

(<<?) infixl 5 	:: !(Task a) ![HtmlTag] -> Task a | iData a
(<<?) task prompt = parallel "<<?" (\list -> length list > 0) (\_ [x:xs] -> x) [("task",task),("prompt",displayHtml prompt)] <<@ TTVertical