definition module PromptingCombinators
/**
* This module provides some combinators for displaying information along with tasks
*/
import TSt, iDataWidgets

/*
Prompting variants:
(?>>)			:: prompt as long as task is active but not finished
(<<?)			:: as ?>>, except that prompt is displayed *after* task
*/

(?>>) infixr 5 	:: ![HtmlTag] !(Task a) 						-> Task a		| iData a
(<<?) infixl 5 	:: !(Task a) ![HtmlTag] 						-> Task a		| iData a