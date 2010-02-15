definition module InteractionTasks

from TSt	import :: Task
from Types	import :: UserId, :: Role
from Html	import :: HtmlTag
from iTasks	import class iTask(..)
import GenPrint, GenParse, GenVisualize, GenUpdate

// This type class contains types that may be used as
// messages and questions: plain strings and html.
// 
// It is for convenience only, do not create new instances yourself.
// Use the generic visualization instead.
class html a  
where
	html :: a -> [HtmlTag]
	
instance html String
instance html [HtmlTag]

//Action buttons, you can use those to specify interactions with multiple possible actions
:: Action
	= ActionLabel !String
	| ActionIcon !String !String
	| ActionOk
	| ActionCancel
	| ActionYes
	| ActionNo
	| ActionNext
	| ActionPrevious
	| ActionFinish

derive gVisualize	Action
derive gUpdate		Action
derive gPrint		Action
derive gParse		Action

//*** Input tasks ***//
enterInformation			:: question -> Task a											| html question & iTask a
enterInformationA			:: question [Action] [Action] -> Task (!Action,!a)				| html question & iTask a

updateInformation			:: question a -> Task a											| html question & iTask a
updateInformationA			:: question [Action] [Action] a  -> Task (!Action,!a)			| html question & iTask a

enterInformationAbout		:: question b -> Task a											| html question & iTask a & iTask b
enterInformationAboutA		:: question [Action] [Action] b -> Task (!Action,!a)			| html question & iTask a & iTask b

updateInformationAbout		:: question b a -> Task a										| html question & iTask a & iTask b
updateInformationAboutA		:: question [Action] [Action] b a  -> Task (!Action,!a)			| html question & iTask a & iTask b

requestConfirmation			:: question -> Task Bool										| html question
requestConfirmationAbout	:: question a -> Task Bool										| html question & iTask a

enterChoice					:: question [a] -> Task a										| html question & iTask a
enterChoiceA				:: question [Action] [Action] [a] -> Task (!Action,!a)			| html question & iTask a

updateChoice				:: question [a] Int -> Task a									| html question & iTask a
updateChoiceA 				:: question [Action] [Action] [a] Int -> Task (!Action,!a)		| html question & iTask a 

enterChoiceAbout			:: question b [a] -> Task a										| html question & iTask a & iTask b
enterChoiceAboutA			:: question [Action] [Action] b [a] -> Task (!Action,!a)		| html question & iTask a & iTask b

updateChoiceAbout			:: question b [a] Int -> Task a									| html question & iTask a & iTask b
updateChoiceAboutA			:: question [Action] [Action] b [a] Int -> Task (!Action,!a)	| html question & iTask a & iTask b

enterMultipleChoice			:: question [a] -> Task [a]										| html question & iTask a
enterMultipleChoiceA		:: question [Action] [a] -> Task (!Action,![a])					| html question & iTask a

updateMultipleChoice		:: question [a] [Int] -> Task [a]								| html question & iTask a
updateMultipleChoiceA		:: question [Action] [a] [Int] -> Task (!Action,![a])			| html question & iTask a

enterMultipleChoiceAbout	:: question b [a] -> Task [a]									| html question & iTask a & iTask b
enterMultipleChoiceAboutA	:: question [Action] b [a] -> Task (!Action,![a])				| html question & iTask a & iTask b

updateMultipleChoiceAbout	:: question b [a] [Int] -> Task [a]								| html question & iTask a & iTask b
updateMultipleChoiceAboutA	:: question [Action] b [a] [Int] -> Task (!Action,![a])			| html question & iTask a & iTask b

//*** Output tasks ***//

//Show a basic message to the current user. The user can end the task after reading the message.
showMessage					:: message -> Task Void											| html message
showMessageA				:: message [Action] -> Task Action								| html message

showMessageAbout			:: message a -> Task Void										| html message & iTask a
showMessageAboutA			:: message [Action] a -> Task Action							| html message & iTask a

//Show a message to the current user. The user can not finish this task. It has to be made obsolete by another parallel task. 
showStickyMessage			:: message -> Task Void											| html message
showStickyMessageAbout		:: message a -> Task Void										| html message & iTask a

//Notify a user through external media. For example via e-mail or sms.
notifyUser					:: message UserId -> Task Void							| html message
notifyGroup					:: message Role -> Task Void							| html message