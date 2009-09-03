definition module InteractionTasks

from TSt	import :: Task
from Types	import :: UserId, :: Role
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
enterInformation			:: question -> Task a				| html question & iTask a
updateInformation			:: question a -> Task a				| html question & iTask a
enterInformationAbout		:: question b -> Task a				| html question & iTask a & iTask b
updateInformationAbout		:: question b a -> Task a			| html question & iTask a & iTask b

enterChoice					:: question [a] -> Task a			| html question & iTask a
updateChoice				:: question [a] Int -> Task a		| html question & iTask a
enterChoiceAbout			:: question b [a] -> Task a			| html question & iTask a & iTask b
updateChoiceAbout			:: question b [a] Int -> Task a		| html question & iTask a & iTask b

enterMultipleChoice			:: question [a] -> Task [a]			| html question & iTask a
updateMultipleChoice		:: question [a] [Int] -> Task [a]	| html question & iTask a
enterMultipleChoiceAbout	:: question b [a] -> Task [a]		| html question & iTask a & iTask b
updateMultipleChoiceAbout	:: question b [a] [Int] -> Task [a]	| html question & iTask a & iTask b

requestConfirmation			:: question -> Task Bool			| html question
requestConfirmationAbout	:: question a -> Task Bool			| html question & iTask a

//Output tasks

//Show a basic message to the current user. The user can end the task after reading the message.
showMessage					:: message -> Task Void				| html message
showMessageAbout			:: message a -> Task Void			| html message & iTask a

//Show a message to the current user. The user can not finish this task. It has to be made obsolete by another parallel task. 
showStickyMessage			:: message -> Task Void				| html message
showStickyMessageAbout		:: message a -> Task Void			| html message & iTask a

//Notify a user through external media. For example via e-mail or sms.
notifyUser					:: message UserId -> Task Void		| html message
notifyGroup					:: message Role -> Task Void		| html message