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
enterInformation			:: question -> Task a			| html question & iTask a
updateInformation			:: question a -> Task a			| html question & iTask a

enterInformationAbout		:: question b -> Task a			| html question & iTask a & iTask b
updateInformationAbout		:: question b a -> Task a		| html question & iTask a & iTask b

enterChoice					:: question [a] -> Task a		| html question & iTask a
//requestChoiceWD				::

//requestChoiceAbout			::
//requestChoiceAboutWD		::

enterMultipleChoice			:: question [a] -> Task [a]		| html question & iTask a
//requestMultipleChoiceWD		::

//requestMultipleChoiceAbout	::
//requestMultipleChoiceAboutWD::

requestConfirmation			:: question -> Task Bool		| html question
requestConfirmationAbout	:: question a -> Task Bool		| html question & iTask a

//Output tasks
showMessage					:: message -> Task Void			| html message
showMessageAbout			:: message a -> Task Void		| html message & iTask a

notifyUser					:: message UserId -> Task Void	| html message
notifyGroup					:: message Role -> Task Void	| html message