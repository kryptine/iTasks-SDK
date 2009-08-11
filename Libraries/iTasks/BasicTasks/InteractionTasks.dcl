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
requestInformation			:: question -> Task a			| html question & iTask a
requestInformationWD		:: question a -> Task a			| html question & iTask a 		//With default value

requestInformationAbout		:: question b -> Task a			| html question & iTask a & iTask b
requestInformationAboutWD	:: question b a -> Task a		| html question & iTask a & iTask b	//With default value

requestChoice				:: question [a] -> Task a		| html question & iTask a
//requestChoiceWD				::

//requestChoiceAbout			::
//requestChoiceAboutWD		::

requestMultipleChoice		:: question [a] -> Task [a]		| html question & iTask a
//requestMultipleChoiceWD		::

//requestMultipleChoiceAbout	::
//requestMultipleChoiceAboutWD::

requestConfirmation			:: question -> Task Bool		| html question
//requestConfirmationAbout	::

//Output tasks
showMessage					:: message -> Task Void			| html message
showMessageAbout			:: message a -> Task Void		| html message & iTask a

notifyUser					:: message UserId -> Task Void	| html message
notifyGroup					:: message Role -> Task Void	| html message