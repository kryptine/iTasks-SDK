definition module PromptingCombinators

// *********************************************************************************************************************************
// This module contains some iTask combinators for html prompting
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import TSt, iDataWidgets

/*
Prompting variants:
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<<?)			:: as ?>>, except that prompt is displayed *after* task
(<<!)			:: as !>>, except that prompt is displayed *after* task
*/

(?>>) infixr 5 	:: ![HtmlTag] !(Task a) 						-> Task a		| iCreate a
(!>>) infixr 5 	:: ![HtmlTag] !(Task a) 						-> Task a		| iCreate a
(<<?) infixl 5 	:: !(Task a) ![HtmlTag] 						-> Task a		| iCreate a
(<<!) infixl 5 	:: !(Task a) ![HtmlTag] 						-> Task a		| iCreate a


/**
* Creates a basic task that displays the given html and never finishes.
*/
displayTask		:: ![HtmlTag] -> Task a											| iCreateAndPrint a

/*
addHtml			:: to insert html code 
iTaskButton		:: a standard button as internally used
mkTaskButtons	:: vertical anIdentifier userId tasknr options list-of-button-names hst returning (idx of button chosen,the code,the name)
*/

addHtml 		:: ![HtmlTag] !*TSt 	-> *TSt
mkTaskButtons 	:: !Bool !String !Int !TaskNr !Options ![String] !*HSt -> (!(!Int,![HtmlTag],!String),!*HSt)
iTaskButton 	:: !String -> HtmlButton
