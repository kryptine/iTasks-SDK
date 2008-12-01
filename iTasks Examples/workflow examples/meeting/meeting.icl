module meeting

import StdEnv, StdiTasks, iDataTrivial, iDataFormlib

// (c) MJP 2007

// findDate will settle a date and time between two persons that want to meet
// first a person is chosen by the person taken the initiative, person 0
// then a date is settled by the two persons by repeatedly asking each other for a convenient date
// if such a date is found both have to confirm the date and the task is finished

npersons = 5

Start world = startTaskEngine test world


//setupDates :: Task (FixedList [Int])
setupDates :: Task (FixedList (HtmlDate,HtmlTime))
setupDates
=						[Text "Initialize dates:",BrTag []] 
						?>>	editTask "Set" (VerList [(createDefault,createDefault) \\ i <- [1..2]])

taskClosure :: (Task a) -> Task (TCl a) | iData a
taskClosure task 
=			return_V (TCl task)


test
=						closureLzTask ("lazy",editTask "OK" 0)
	=>> \(TCl task) ->	(1 @: ("t1",task)) -&&- (2 @: ("t2",task)) 


findDate :: Task (HtmlDate,HtmlTime)
findDate
=						[Text "Choose person you want to date:",BrTag []] 
						?>>	editTask "Set" (HtmlSelect [(toString i,toString i) \\ i <- [1..npersons - 1]] (toString 1))
	=>> \(HtmlSelect _ whomPD) ->		
						let whom = toInt whomPD
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
				
:: FixedList a = HorList [a]
               | VerList [a]

gForm{|FixedList|} gFormA (init, formId) hst # (form, hst) = gFormList xs hst
                                             = ({form & value = constr form.value}, hst)
  where (f, constr, xs) = case formId.ival of
                              HorList xs -> ((<=>) , HorList, xs)
                              VerList xs -> ((<||>), VerList, xs)
                              
		gFormList :: [] *HSt -> (Form [Int],*HSt)
        gFormList []     hst = ( { changed = False
                                 , value   = []
                                 , form    = []
								 , inputs  = []                                 }
                               , hst)
        gFormList [x]    hst # (formx, hst) = gFormA (init, reuseFormId formId x) hst                 
                             = ( { changed = formx.changed
                                 , value   = [formx.value]
 								 , inputs  = []                                 
                                 , form    = formx.form							
                                 }
                               , hst)
        gFormList [x:xs] hst # (formx, hst)  = gFormA (init, reuseFormId formId x) hst
                             # (formxs, hst) = gFormList xs hst  
                             = ( { changed = formx.changed || formxs.changed
                                 , value   = [formx.value : formxs.value]
                                 , form    = [f [f formx.form [BrTag [], HrTag [], BrTag []]] formxs.form]
 								 , inputs  = []                                 
                                 }
                               , hst)  

derive gUpd []
gUpd{|FixedList|} gUpdA (UpdCreate x) _   
# (mode,nlist)	= gUpd{|* -> *|} gUpdA	(UpdCreate x) undef 
= (mode, VerList nlist)
gUpd{|FixedList|} gUpdA mode          val # (mode, xs) = gFormList mode xs
	                                      = (mode, constr xs)
  where (constr, xs) = case val of
                         HorList xs -> (HorList, xs)
                         VerList xs -> (VerList, xs)

        gFormList mode []     = (mode, [])
		gFormList mode [x:xs] # (mode, x)  = gUpdA mode x
                              # (mode, xs) = gFormList mode xs
                              = (mode, [x:xs])

derive gParse FixedList
derive gPrint FixedList