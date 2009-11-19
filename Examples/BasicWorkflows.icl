implementation module BasicWorkflows

import iTasks
import CommonDomain

basicWorkflows :: [Workflow]
basicWorkflows =
	[ workflow "Basic/Basic task" basicTask
	, workflow "Basic/Information task" informationTask
	, workflow "Basic/Decision task" decisionTask
	, workflow "Basic/Notification task" notificationTask
	]
	
basicTask :: Task Void
basicTask = define >>= perform >>| report
where
	define :: Task Note
	define = enterInformation "What needs to be done?"
	
	perform :: Note -> Task Void
 	perform description = showMessage description

	report :: Task Void
	report = showMessage "The task has been done"

informationTask :: Task Void
informationTask = define >>= perform >>= report
where
	define :: Task Note
	define = enterInformation "What do you want to know?"
	
	perform :: Note -> Task Note
	perform question = enterInformation question
	
	report :: Note -> Task Void
	report answer = showMessageAbout "The answer to your question is" answer

decisionTask :: Task Void
decisionTask = define >>= perform >>= report
where
	define :: Task (Note,[Note])
	define = enterInformation "What needs to be decided?"
	         -&&-
	         enterInformation "What are the alternatives?"
	         
	perform :: (Note,[Note]) -> Task Note
	perform (question,options) = enterChoice question options 

	report :: Note -> Task Void
	report decision = showMessageAbout "The chosen alternative is" decision

notificationTask :: Task Void
notificationTask = define >>= perform >>| report
where
	define :: Task Note
	define = enterInformation "What message do you want to distribute?"
	
	perform :: Note -> Task Void
	perform message = showMessage message
	
	report :: Task Void
	report = showMessage "Your message has been read"