implementation module BasicWorkflows

import iTasks
import CommonDomain

basicWorkflows :: [Workflow]
basicWorkflows =
	[ {Workflow|name="Basic/Action task",label="New action task...",roles=[],mainTask=actionTask >>|return Void}
	, {Workflow|name="Basic/Information task",label="New information task",roles=[],mainTask=informationTask >>| return Void}
	, workflow "Basic/Decision task" decisionTask
	, workflow "Basic/Notification task" notificationTask
	]


:: AssignTo = Yourself | OtherUser UserId

:: ActionTaskInfo =
	{ taskDescription	:: TaskDescription
	, initialTaskProperties	:: InitialTaskProperties
	}
:: InitialTaskProperties =
	{ assignTo			:: AssignTo
	, priority			:: Maybe TaskPriority
	, deadline			:: Maybe Timestamp
	}

derive gPrint 		ActionTaskInfo, InitialTaskProperties, AssignTo, TaskDescription
derive gParse		ActionTaskInfo, InitialTaskProperties, AssignTo, TaskDescription
derive gVisualize	ActionTaskInfo, InitialTaskProperties, AssignTo, TaskDescription
derive gUpdate		ActionTaskInfo, InitialTaskProperties, AssignTo, TaskDescription

actionTask :: Task (ProcessRef Void)
actionTask = define >>= start
where
	define :: Task ActionTaskInfo
	define = enterInformation "Define a new action"
	
	start :: ActionTaskInfo -> Task (ProcessRef Void)
	start info 
		= case info.initialTaskProperties.assignTo of
			(Yourself)
				= getCurrentUser >>= \user -> spawnProcess user.userId True task
			(OtherUser uid)
				= spawnProcess uid True task
	where
		task		= showMessage info.taskDescription.TaskDescription.title <<@ info.taskDescription
		priority	= case info.priority of (Just p) = p; Nothing = NormalPriority
		deadline	= info.deadline

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