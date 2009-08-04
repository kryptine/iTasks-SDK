implementation module ReviewTask

import iTasks, iDataTrivial

// (c) 2007 MJP

// A task is given to user 0
// When finished the result of the task is reviewed by user 1
// He can comment on the task, or approve or cancel it
// When the result needs more work, the whole process is repeated
// Otherwise the task is completed
// The task itself in the example is a quotation form that needs to be filled in

derive gForm 		QForm, Review, Person, Gender
derive gUpd 		QForm, Review, Person, Gender
derive gVisualize 	QForm, Review, Person, Gender
derive gUpdate 		QForm, Review, Person, Gender
derive gParse 		QForm, Review, Person, Gender
derive gPrint 		QForm, Review, Person, Gender

:: PersonData	=	{ name		:: String
					, e_mail	:: String
					}
:: SurfaceMail	=	{ adress 	:: String
					, zipcode 	:: String
					, city	 	:: String
					}

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


reviewTaskExample :: [Workflow]
reviewTaskExample
= [	{	name		= "Examples/Higher order/Review task"
	,	label		= "Review the results of a task"
	,	roles		= []
	,	mainTask	= reviewtask >>| return Void
	}
  ]

editTaskSA :: String a -> Task a | iData a & iTask a
editTaskSA s a = editTask s a <<@ Submit

reviewtask :: Task (QForm,Review)
reviewtask = taskToReview 1 (createDefault, mytask)

mytask :: a -> (Task a) | iData a & iTask a
mytask v =	[Text "Fill in Form:",BrTag [],BrTag []] 
			?>> editTaskSA "TaskDone" v 

taskToReview :: UserId (a,a -> Task a) -> Task (a,Review) | iData a 
taskToReview reviewer (v`,task) 
= compound "taskToReview" taskToReview`
where
	taskToReview`
	=					task v`               
		>>= \v ->		reviewer @: ("Review", review v) 
		>>= \r ->		[Text ("Reviewer " <+++ reviewer <+++ " says "),toHtml r,BrTag []] 
						?>> editTask "OK" Void 
		>>|				case r of
							(NeedsRework _) -> taskToReview reviewer (v,task) 	
							else            -> return (v,r)

review :: a -> Task Review | iData a 
review v
=	[toHtml v,BrTag [],BrTag []] 
	?>>	chooseTask []
			[ ("Rework",   editTaskSA "Done" (NeedsRework createDefault))
			, ("Approved", return Approved)
			, ("Reject",   return Rejected)
			]
