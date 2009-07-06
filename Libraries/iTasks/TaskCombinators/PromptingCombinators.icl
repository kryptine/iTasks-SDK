implementation module PromptingCombinators
/**
* This module contains some iTask combinators for html prompting
*/

import StdList, StdMisc
import TSt, UITasks, BasicCombinators, TuningCombinators

(?>>) infixr 5 	:: ![HtmlTag] !(Task a) -> Task a | iData a
(?>>) prompt task = parallel "?>>" (\list -> length list > 0) hd undef [displayHtml prompt <<@ "prompt",task <<@ "task"] <<@ TTVertical

(<<?) infixl 5 	:: !(Task a) ![HtmlTag] -> Task a | iData a
(<<?) task prompt = parallel "<<?" (\list -> length list > 0) hd undef [task <<@ "task",displayHtml prompt <<@ "task"] <<@ TTVertical