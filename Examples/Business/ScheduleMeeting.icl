implementation module ScheduleMeeting

import iTasks
import CommonDomain
import StdMisc

// (c) MJP 2007

// findDate will settle a date and time between two persons that want to meet
// first a person is chosen by the person taken the initiative, person 0
// then a date is settled by the two persons by repeatedly asking each other for a convenient date
// if such a date is found both have to confirm the date and the task is finished

npersons = 5

scheduleMeetingExample :: [Workflow]
scheduleMeetingExample
= [	{	name		= "Examples/Business/Schedule meeting"
	,	label		= "Schedule meeting"
	,	roles		= []
	,	mainTask	= findDate >>| return Void
	}
  ]

findDate :: Task (Date,Time)
findDate			
	=					chooseUser "Choose person you want to schedule a meeting with:"
	>>= \whom 		->	findDate` whom.User.userId ({Date|year = 2007, mon = 1, day = 1},{Time|hour = 9, min = 0, sec = 0}) 
	>>= \datetime	->	(confirm 0 whom.User.userId datetime -&&- confirm whom.User.userId 0 datetime)
	>>|					return datetime
where
	findDate` :: Int (Date,Time) -> Task (Date,Time)
	findDate` whom daytime
	=							proposeDateTime daytime 
		>>= \daytime ->			whom  @: ("Meeting Request",determineDateTime daytime) 
		>>= \(ok,daytime) ->	if ok 
									(return daytime)
									(					isOkDateTime daytime 
										>>= \ok ->		if ok 
															(return daytime)
										      				(findDate` whom daytime)
									)
	where
		proposeDateTime :: (Date,Time) -> Task (Date,Time)
		proposeDateTime (date,time)
		=							updateInformation [Text "Propose a new date and time for meeting:",BrTag [],BrTag []] input 
			>>= \(_,date,_,time) -> return (date,time)
		where
			input = (toString (Text "date: "), date, toString (Text "time: "), time)

		determineDateTime :: (Date,Time) -> Task (Bool,(Date,Time))
		determineDateTime daytime
		=					isOkDateTime daytime 
			>>= \ok ->		if ok 
								(return (ok,daytime))
								(					proposeDateTime daytime 
									>>= \daytime ->	return (ok,daytime)
								)

		isOkDateTime :: (Date,Time) -> Task Bool
		isOkDateTime (date,time)
		=	requestConfirmation ("Can we meet on the " <+++ date <+++ " at " <+++ time <+++ "?")

	confirm  :: Int Int (Date,Time) -> Task Void 
	confirm me you (date,time)
	= 	me @:	("Meeting confirmation", showMessage ("User " <+++ me <+++ " and " <+++ you <+++ " have a meeting on " <+++ date <+++ " at " <+++ time))
				
