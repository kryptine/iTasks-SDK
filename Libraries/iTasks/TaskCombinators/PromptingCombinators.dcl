definition module PromptingCombinators
/**
* This module provides some combinators for displaying information along with tasks
*/
from TSt 	import :: Task
from Html	import :: HtmlTag
from iDataSettings	import class iPrint, class iParse, class iCreate, class iCreateAndPrint, class iSpecialStore, class iData

import iDataForms
/*
Prompting variants:
(?>>)			:: prompt as long as task is active but not finished
(<<?)			:: as ?>>, except that prompt is displayed *after* task
*/

(?>>) infixr 5 	:: ![HtmlTag] !(Task a) 						-> Task a		| iData a
(<<?) infixl 5 	:: !(Task a) ![HtmlTag] 						-> Task a		| iData a