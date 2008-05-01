definition module iTasksHtmlSupport

// *********************************************************************************************************************************
// This module contains some iTask combinators for html prompting
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksHandler

/*
Prompting variants:
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<<?)			:: as ?>>, except that prompt is displayed *after* task
(<<!)			:: as !>>, except that prompt is displayed *after* task

addHtml			:: add html code
*/

(?>>) infixr 5 	:: !HtmlCode !(Task a) 						-> Task a		| iCreate a
(!>>) infixr 5 	:: !HtmlCode !(Task a) 						-> Task a		| iCreate a
(<<?) infixl 5 	:: !(Task a) !HtmlCode 						-> Task a		| iCreate a
(<<!) infixl 5 	:: !(Task a) !HtmlCode 						-> Task a		| iCreate a

addHtml 		:: HtmlCode TSt 	-> TSt
mkTaskButtons 	:: !Bool !String !Int !TaskNr !Options ![String] *HSt -> ((Int,HtmlCode,String),*HSt)
iTaskButton 	:: String -> Button
