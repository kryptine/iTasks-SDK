module date

import StdEnv, StdiTasks, iDataTrivial

// (c) MJP 2007

// findDate will settle a date and time between two persons that want to meet
// first a person is chosen by the person taken the initiative, person 0
// then a date is settled by the two persons by repeatedly asking each other for a convenient date
// if such a date is found both have to confirm the date and the task is finished

npersons = 5

Start world = startTaskEngine (foreverTask findDate) world

findDate :: Task (HtmlDate,HtmlTime)
findDate
=						[Text "Choose person you want to date:",BrTag []] 
						?>>	editTask "Set" (HtmlSelect [(toString i,toString i) \\ i <- [1..npersons - 1]] (toString 1)) 
	=>> \(HtmlSelect _ sel) ->		let whom = toInt sel
						in
						[Text "Determining date:",BrTag [],BrTag []] 
						?>>	findDate` whom (HtmlDate 1 1 2007,HtmlTime 9 0 0) 
	=>> \datetime	->	[] 
						?>> confirm 0 whom datetime -&&- confirm whom 0 datetime 
	#>>					return_V datetime
where
	findDate` :: Int (HtmlDate,HtmlTime) -> Task (HtmlDate,HtmlTime)
	findDate` whom daytime
	=							proposeDateTime daytime 
		=>> \daytime ->			whom  @: ("Meeting Request",determineDateTime daytime) 
		=>> \(ok,daytime) ->	if ok 
									(return_V daytime)
									(					isOkDateTime daytime 
										=>> \ok ->		if ok 
															(return_V daytime)
										      				(newTask "findDate`" (findDate` whom daytime))
									)
	where
		proposeDateTime :: (HtmlDate,HtmlTime) -> Task (HtmlDate,HtmlTime)
		proposeDateTime (date,time)
		=							[Text "Propose a new date and time for meeting:",BrTag [],BrTag []] 
									?>>	editTask "Set" input 
			=>> \(_,date,_,time) -> return_V (date,time)
		where
			input = (toString (Text "date: "), date, toString (Text "time: "), time)

		determineDateTime :: (HtmlDate,HtmlTime) -> Task (Bool,(HtmlDate,HtmlTime))
		determineDateTime daytime
		=					isOkDateTime daytime 
			=>> \ok ->		if ok 
								(return_V (ok,daytime))
								(					proposeDateTime daytime 
									=>> \daytime ->	return_V (ok,daytime)
								)

		isOkDateTime :: (HtmlDate,HtmlTime) -> Task Bool
		isOkDateTime (date,time)
		=	chooseTask [Text ("Can we meet on the " <+++ date <+++ " at " <+++ time <+++ "?"),BrTag []] 
			[ ("Accept",return_V True)
			, ("Sorry", return_V False)
			]

	confirm  :: Int Int (HtmlDate,HtmlTime) -> Task Void 
	confirm me you (date,time)
	= 	me @::	[Text ("User " <+++ me <+++ " and " <+++ you <+++ " have a meeting on " <+++ date <+++ " at " <+++ time),BrTag [],BrTag []] 
				?>>	editTask "OK" Void
				
