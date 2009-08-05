definition module GUITasks

from iTasks import class iTask(..)
import GenPrint, GenParse, GUICore

from TSt			import :: Task
from Html			import :: HtmlTag

import CoreCombinators

//HIGH LEVEL

//High level user interaction tasks

class vizHtml a //This class will also be obsolete soon 
where
	vizHtml :: a -> [HtmlTag]
	
instance vizHtml String
instance vizHtml [HtmlTag]

//Input tasks
requestInformation			:: question -> Task a			| vizHtml question & iTask a
requestInformationWD		:: question a -> Task a			| vizHtml question & iTask a 	//With default value

//requestInformationAbout		:: question b -> Task a			| vizHtml question & iData a & iTask b & iData b
//requestInformationAboutWD	:: question b a -> Task a		| vizHtml question & iData a & iTask b & iData b	//With default value

requestConfirmation			:: question -> Task Bool		| vizHtml question

requestChoice				:: question [a] -> Task a		| vizHtml question & iTask a
requestMultipleChoice		:: question [a] -> Task [a]		| vizHtml question & iTask a

//Output tasks
showMessage					:: message -> Task Void			| vizHtml message
showMessageAbout			:: message a -> Task Void		| vizHtml message & iTask a

//notifyUser					:: message UserId -> Task Void	| vizHtml message
//notifyGroup					:: message Role -> Task Void	| vizHtml message






//SOON TO BE OBSOLETE!

//editTask :: !String !a -> Task a | iData a
editTask s a :== requestInformationWD "Please edit this value" a 


/* 
editTask		:: create a task editor to edit a value of given type, and add a button with given name to finish the task
editTaskPred	:: create a task editor (with submit button) to edit a value of given type, finish only if predicate holds 
*/
//editTask 		:: !String 	!a 								-> Task a		| iData a 
//editTaskPred 	:: 			!a !(a -> (Bool, [HtmlTag]))	-> Task a		| iData a 

/**
* Creates a basic task that displays the given html and never finishes.
*/

displayHtml	msg	:== showMessage msg >>| return defaultValue
displayValue x	:== showMessageAbout "" x >>| return defaultValue

/**
* Tasks for offering choices to users
*/
selectWithButtons		:: ![String]										-> Task Int			
selectWithPulldown		:: ![String] !Int									-> Task Int
selectWithRadiogroup	:: ![[HtmlTag]]	!Int								-> Task Int
selectWithCheckboxes	:: ![(![HtmlTag], !Bool, !(Bool [Bool] -> [Bool]))]	-> Task [Int]

/**
* Common user interface tasks
*/
button			:: !String !a -> Task a | iTask a

ok				:: Task Void

yes				:: Task Bool
no				:: Task Bool