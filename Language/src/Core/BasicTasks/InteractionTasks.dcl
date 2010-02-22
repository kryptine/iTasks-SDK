definition module InteractionTasks

from TSt		import :: Task
from Types		import :: Role
from Html		import :: HtmlTag
from iTasks		import class iTask(..)
from ProcessDB	import :: Action
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

:: ActionPredicate a :== a Bool -> Bool

always	:: ActionPredicate a | iTask a
ifValid	:: ActionPredicate a | iTask a

//*** Input tasks ***//
enterInformation			:: question -> Task a																		| html question & iTask a
enterInformationA			:: question [Action] [Action] [(Action,ActionPredicate a)] -> Task (!Action,!a)				| html question & iTask a

updateInformation			:: question a -> Task a																		| html question & iTask a
updateInformationA			:: question [Action] [Action] [(Action,ActionPredicate a)] a  -> Task (!Action,!a)			| html question & iTask a

enterInformationAbout		:: question b -> Task a																		| html question & iTask a & iTask b
enterInformationAboutA		:: question [Action] [Action] [(Action,ActionPredicate a)] b -> Task (!Action,!a)			| html question & iTask a & iTask b

updateInformationAbout		:: question b a -> Task a																	| html question & iTask a & iTask b
updateInformationAboutA		:: question [Action] [Action] [(Action,ActionPredicate a)] b a  -> Task (!Action,!a)		| html question & iTask a & iTask b

requestConfirmation			:: question -> Task Bool																	| html question
requestConfirmationAbout	:: question a -> Task Bool																	| html question & iTask a

enterChoice					:: question [a] -> Task a																	| html question & iTask a
enterChoiceA				:: question [Action] [Action] [(Action,ActionPredicate a)] [a] -> Task (!Action,!a)			| html question & iTask a

updateChoice				:: question [a] Int -> Task a																| html question & iTask a
updateChoiceA 				:: question [Action] [Action] [(Action,ActionPredicate a)] [a] Int -> Task (!Action,!a)		| html question & iTask a 

enterChoiceAbout			:: question b [a] -> Task a																	| html question & iTask a & iTask b
enterChoiceAboutA			:: question [Action] [Action] [(Action,ActionPredicate a)] b [a] -> Task (!Action,!a)		| html question & iTask a & iTask b

updateChoiceAbout			:: question b [a] Int -> Task a																| html question & iTask a & iTask b
updateChoiceAboutA			:: question [Action] [Action] [(Action,ActionPredicate a)] b [a] Int -> Task (!Action,!a)	| html question & iTask a & iTask b

enterMultipleChoice			:: question [a] -> Task [a]																	| html question & iTask a
enterMultipleChoiceA		:: question [Action] [(Action,ActionPredicate [a])] [a] -> Task (!Action,![a])				| html question & iTask a

updateMultipleChoice		:: question [a] [Int] -> Task [a]															| html question & iTask a
updateMultipleChoiceA		:: question [Action] [(Action,ActionPredicate [a])] [a] [Int] -> Task (!Action,![a])		| html question & iTask a

enterMultipleChoiceAbout	:: question b [a] -> Task [a]																| html question & iTask a & iTask b
enterMultipleChoiceAboutA	:: question [Action] [(Action,ActionPredicate [a])] b [a] -> Task (!Action,![a])				| html question & iTask a & iTask b

updateMultipleChoiceAbout	:: question b [a] [Int] -> Task [a]															| html question & iTask a & iTask b
updateMultipleChoiceAboutA	:: question [Action] [(Action,ActionPredicate [a])] b [a] [Int] -> Task (!Action,![a])		| html question & iTask a & iTask b

//*** Output tasks ***//

//Show a basic message to the current user. The user can end the task after reading the message.
showMessage					:: message -> Task Void																		| html message
showMessageA				:: message [Action ][(Action,ActionPredicate Void)] -> Task Action							| html message

showMessageAbout			:: message a -> Task Void																	| html message & iTask a
showMessageAboutA			:: message [Action] [(Action,ActionPredicate Void)] a -> Task Action						| html message & iTask a

//Show a message to the current user. The user can not finish this task. It has to be made obsolete by another parallel task. 
showStickyMessage			:: message -> Task Void											| html message
showStickyMessageAbout		:: message a -> Task Void										| html message & iTask a

//Notify a user through external media. For example via e-mail or sms.
notifyUser					:: message UserName -> Task Void								| html message
notifyGroup					:: message Role -> Task Void									| html message

//*** Utility Functions ***//
//Generate a set of action buttons by joining the buttons that are always shown and those only active when valid
makeButtons :: !String ![Action] ![Action] !Bool -> [(!Action,!String,!String,!String,!Bool)]	
taskPanel :: String [HtmlTag] (Maybe [HtmlTag]) (Maybe [TUIDef]) [(Action,String,String,String,Bool)] -> TUIDef