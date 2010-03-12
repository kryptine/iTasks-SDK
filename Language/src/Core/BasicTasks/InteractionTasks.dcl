definition module InteractionTasks

from TSt		import :: Task, :: SharedID
from Types		import :: Role
from Html		import :: HtmlTag
from iTasks		import class iTask(..)
from ProcessDB	import :: Action
import GenPrint, GenParse, GenVisualize, GenUpdate, GenMerge

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

:: TaskAction a = ButtonAction (ActionWithCond a) | MenuAction (ActionWithCond a) | ButtonAndMenuAction (ActionWithCond a) | MenuParamAction (String, ActionCondition a)
:: ActionWithCond a :== (Action, ActionCondition a)
:: ActionCondition a = Always | IfValid | Predicate ((EditorValue a) -> Bool)
:: EditorValue a = Invalid | Valid a

//*** Input tasks ***//
enterInformation			:: question -> Task a												| html question & iTask a
enterInformationA			:: question ![TaskAction a] -> Task (!Action,!a)					| html question & iTask a

updateInformation			:: question a -> Task a												| html question & iTask a
updateInformationA			:: question ![TaskAction a] a  -> Task (!Action,!a)					| html question & iTask a

enterInformationAbout		:: question b -> Task a												| html question & iTask a & iTask b
enterInformationAboutA		:: question ![TaskAction a] b -> Task (!Action,!a)					| html question & iTask a & iTask b

updateInformationAbout		:: question b a -> Task a											| html question & iTask a & iTask b
updateInformationAboutA		:: question ![TaskAction a] b a  -> Task (!Action,!a)				| html question & iTask a & iTask b

requestConfirmation			:: question -> Task Bool											| html question
requestConfirmationAbout	:: question a -> Task Bool											| html question & iTask a

enterChoice					:: question [a] -> Task a											| html question & iTask a
enterChoiceA				:: question ![TaskAction a] [a] -> Task (!Action,!a)				| html question & iTask a

updateChoice				:: question [a] Int -> Task a										| html question & iTask a
updateChoiceA 				:: question ![TaskAction a] [a] Int -> Task (!Action,!a)			| html question & iTask a 

enterChoiceAbout			:: question b [a] -> Task a											| html question & iTask a & iTask b
enterChoiceAboutA			:: question ![TaskAction a] b [a] -> Task (!Action,!a)				| html question & iTask a & iTask b

updateChoiceAbout			:: question b [a] Int -> Task a										| html question & iTask a & iTask b
updateChoiceAboutA			:: question ![TaskAction a] b [a] Int -> Task (!Action,!a)			| html question & iTask a & iTask b

enterMultipleChoice			:: question [a] -> Task [a]											| html question & iTask a
enterMultipleChoiceA		:: question ![TaskAction [a]] [a] -> Task (!Action,![a])			| html question & iTask a

updateMultipleChoice		:: question [a] [Int] -> Task [a]									| html question & iTask a
updateMultipleChoiceA		:: question ![TaskAction [a]] [a] [Int] -> Task (!Action,![a])		| html question & iTask a

enterMultipleChoiceAbout	:: question b [a] -> Task [a]										| html question & iTask a & iTask b
enterMultipleChoiceAboutA	:: question ![TaskAction [a]] b [a] -> Task (!Action,![a])			| html question & iTask a & iTask b

updateMultipleChoiceAbout	:: question b [a] [Int] -> Task [a]									| html question & iTask a & iTask b
updateMultipleChoiceAboutA	:: question ![TaskAction [a]] b [a] [Int] -> Task (!Action,![a])	| html question & iTask a & iTask b

//*** Output tasks ***//

//Show a basic message to the current user. The user can end the task after reading the message.
showMessage					:: message -> Task Void												| html message
showMessageA				:: message ![TaskAction Void] -> Task Action						| html message

showMessageAbout			:: message a -> Task Void											| html message & iTask a
showMessageAboutA			:: message ![TaskAction Void] a -> Task Action						| html message & iTask a

//Show a message to the current user. The user can not finish this task. It has to be made obsolete by another parallel task. 
showStickyMessage			:: message -> Task Void												| html message
showStickyMessageAbout		:: message a -> Task Void											| html message & iTask a

//Notify a user through external media. For example via e-mail or sms.
notifyUser					:: message UserId -> Task Void									| html message
notifyGroup					:: message Role -> Task Void										| html message

//*** Shared value tasks ***//
:: Editor s a	= {editorFrom :: s -> a, editorTo :: a s -> s}
:: Listener s a	= {listenerFrom :: s -> a}
:: View s

listener	:: !(Listener s a)	-> View s | iTask a & iTask s & gMerge{|*|} s
editor		:: !(Editor s a)	-> View s | iTask a & iTask s & gMerge{|*|} s

idEditor	:: View s	| iTask s & gMerge{|*|} s
idListener	:: View s	| iTask s & gMerge{|*|} s

createShared				:: a -> Task (SharedID a)													| iTask a
getShared					:: (SharedID a) -> Task a													| iTask a
setShared					:: (SharedID a) a -> Task Void												| iTask a
updateShared				:: question ![TaskAction s] !(SharedID s) ![View s] -> Task (!Action, !s)	| html question & iTask s & gMerge{|*|} s
updateSharedLocal			:: question ![TaskAction s] !s ![View s] -> Task (!Action, !s)				| html question & iTask s & gMerge{|*|} s

//*** Utility Functions ***//
//Generate a set of action buttons by joining the buttons that are always shown and those only active when valid
makeButtons :: !String ![(Action, Bool)] -> [(!Action,!String,!String,!String,!Bool)]
taskPanel :: String [HtmlTag] (Maybe [HtmlTag]) (Maybe [TUIDef]) [(Action,String,String,String,Bool)] -> TUIDef