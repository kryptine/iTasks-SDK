implementation module ScheduleMeeting

import iTasks, iDataTrivial, iDataFormlib
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

findDate :: Task (HtmlDate,HtmlTime)
findDate
=						[Text "Choose person you want to schedule a meeting with:",BrTag []] 
						?>>	editTask "Set" (HtmlSelect [(toString i,toString i) \\ i <- [1..npersons - 1]] (toString 1))
	=>> \(HtmlSelect _ whomPD) ->		
						let whom = toInt whomPD
						in
						[Text "Determining date:",BrTag [],BrTag []] 
						?>>	findDate` whom (HtmlDate 1 1 2007,HtmlTime 9 0 0) 
	=>> \datetime	->	[] 
						?>> confirm 0 whom datetime -&&- confirm whom 0 datetime 
	#>>					return datetime
where
	findDate` :: Int (HtmlDate,HtmlTime) -> Task (HtmlDate,HtmlTime)
	findDate` whom daytime
	=							proposeDateTime daytime 
		=>> \daytime ->			whom  @: ("Meeting Request",determineDateTime daytime) 
		=>> \(ok,daytime) ->	if ok 
									(return daytime)
									(					isOkDateTime daytime 
										=>> \ok ->		if ok 
															(return daytime)
										      				(newTask "findDate`" (findDate` whom daytime))
									)
	where
		proposeDateTime :: (HtmlDate,HtmlTime) -> Task (HtmlDate,HtmlTime)
		proposeDateTime (date,time)
		=							[Text "Propose a new date and time for meeting:",BrTag [],BrTag []] 
									?>>	editTask "Set" input 
			=>> \(_,date,_,time) -> return (date,time)
		where
			input = (toString (Text "date: "), date, toString (Text "time: "), time)

		determineDateTime :: (HtmlDate,HtmlTime) -> Task (Bool,(HtmlDate,HtmlTime))
		determineDateTime daytime
		=					isOkDateTime daytime 
			=>> \ok ->		if ok 
								(return (ok,daytime))
								(					proposeDateTime daytime 
									=>> \daytime ->	return (ok,daytime)
								)

		isOkDateTime :: (HtmlDate,HtmlTime) -> Task Bool
		isOkDateTime (date,time)
		=	chooseTask [Text ("Can we meet on the " <+++ date <+++ " at " <+++ time <+++ "?"),BrTag []] 
			[ ("Accept",return True)
			, ("Sorry", return False)
			]

	confirm  :: Int Int (HtmlDate,HtmlTime) -> Task Void 
	confirm me you (date,time)
	= 	me @::>	[Text ("User " <+++ me <+++ " and " <+++ you <+++ " have a meeting on " <+++ date <+++ " at " <+++ time),BrTag [],BrTag []] 
				?>>	editTask "OK" Void
