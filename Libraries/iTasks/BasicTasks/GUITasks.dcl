definition module GUITasks

from TSt	import :: Task
from Types	import :: Role
from Html	import :: HtmlTag
from iTasks	import class iTask(..)
import GenPrint, GenParse, GUICore

// This type class contains types that are
// easily displayed as static html
class html a  
where
	html :: a -> [HtmlTag]
	
instance html String
instance html [HtmlTag]

//Input tasks
requestInformation			:: question -> Task a			| html question & iTask a
requestInformationWD		:: question a -> Task a			| html question & iTask a 		//With default value

requestInformationAbout		:: question b -> Task a			| html question & iTask a & iTask b
requestInformationAboutWD	:: question b a -> Task a		| html question & iTask a & iTask b	//With default value

requestConfirmation			:: question -> Task Bool		| html question

requestChoice				:: question [a] -> Task a		| html question & iTask a
requestMultipleChoice		:: question [a] -> Task [a]		| html question & iTask a

//Output tasks
showMessage					:: message -> Task Void			| html message
showMessageAbout			:: message a -> Task Void		| html message & iTask a

notifyUser					:: message UserId -> Task Void	| html message
notifyGroup					:: message Role -> Task Void	| html message

//SOON TO BE OBSOLETE!
import CoreCombinators

//editTask :: !String !a -> Task a | iData a
editTask s a :== requestInformationWD "Please edit this value" a 

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