module quotation

import StdList, iTasks, iDataTrivial, StdBimap

// (c) 2007 MJP

// A task is given to user 0
// When finished the result of the task is reviewed by user 1
// He can comment on the task, or approve or cancel it
// When the result needs more work, the whole process is repeated
// Otherwise the task is completed
// The task itself in the example is a quotation form that needs to be filled in

derive gForm 	QForm, Review, Person, Gender
derive gUpd 	QForm, Review, Person, Gender
derive gParse 	QForm, Review, Person, Gender
derive gPrint 	QForm, Review, Person, Gender
//derive gerda 	QForm, Review, Person, Gender
//derive read 	QForm, Review, Person, Gender
//derive write 	QForm, Review, Person, Gender


:: PersonData	=	{ name		:: String
					, e_mail	:: String
					}
:: SurfaceMail	=	{ adress 	:: String
					, zipcode 	:: String
					, city	 	:: String
					}

Start world = startEngine [myWorkflow] world

:: QForm = 	{ toComp 			:: String
			, startDate 		:: HtmlDate
			, endDate 			:: HtmlDate
			, estimatedHours 	:: Int
			, description		:: HtmlTextarea
			, price				:: HtmlCurrency 	
			}
:: Person = { firstName			:: String
			 , surname			:: String
			 , dateOfBirth		:: HtmlDate
			 , gender			:: Gender
			 }
:: Gender = Male | Female
:: Review = Approved | Rejected | NeedsRework HtmlTextarea
:: UserID :== Int


myWorkflow
=	{	name		= "quotation"
	,	label		= "quotation"
	,	roles		= []
	,	mainTask	= reviewtask #>> return_V Void
	}

editTaskSA :: String a -> Task a | iData a
editTaskSA s a = UseAjax @>> editTask s a <<@ Submit

reviewtask :: Task (QForm,Review)
reviewtask = taskToReview 1 (createDefault, mytask)

mytask :: a -> (Task a) | iData a
mytask v =	[Text "Fill in Form:",BrTag [],BrTag []] 
			?>> editTaskSA "TaskDone" v 

taskToReview :: UserID (a,a -> Task a) -> Task (a,Review) | iData a 
taskToReview reviewer (v`,task) 
= newTask "taskToReview" taskToReview`
where
	taskToReview`
	=					task v`               
		=>> \v ->		reviewer @:: review v 
		=>> \r ->		[Text ("Reviewer " <+++ reviewer <+++ " says "),toHtml r,BrTag []] 
						?>> editTask "OK" Void 
		#>>				case r of
							(NeedsRework _) -> taskToReview reviewer (v,task) 	
							else            -> return_V (v,r)

review :: a -> Task Review | iData a 
review v
=	[toHtml v,BrTag [],BrTag []] 
	?>>	chooseTask []
			[ ("Rework",  editTaskSA "Done" (NeedsRework createDefault))
			, ("Approved",return_V Approved)
			, ("Reject",  return_V Rejected)
			]
