implementation module ReviewTask

import iTasks
import CommonDomain

// (c) 2007 MJP

// A task is given to user 0
// When finished the result of the task is reviewed by user 1
// He can comment on the task, or approve or cancel it
// When the result needs more work, the whole process is repeated
// Otherwise the task is completed
// The task itself in the example is a quotation form that needs to be filled in

derive gPrint 		QForm, Review, Person, Gender
derive gParse 		QForm, Review, Person, Gender
derive gVisualize 	QForm, Review, Person, Gender
derive gUpdate 		QForm, Review, Person, Gender

:: PersonData	=	{ name		:: String
					, e_mail	:: String
					}
:: SurfaceMail	=	{ adress 	:: String
					, zipcode 	:: String
					, city	 	:: String
					}

:: QForm = 	{ toComp 			:: String
			, startDate 		:: Date
			, endDate 			:: Date
			, estimatedHours 	:: Int
			, description		:: Note
			, price				:: Currency 	
			}
:: Person = { firstName			:: String
			 , surname			:: String
			 , dateOfBirth		:: Date
			 , gender			:: Gender
			 }
:: Gender = Male | Female
:: Review = Approved | Rejected | NeedsRework Note


reviewTaskExample :: [Workflow]
reviewTaskExample
= [	{	name		= "Examples/Higher order/Review task"
	,	label		= "Review the results of a task"
	,	roles		= []
	,	mainTask	= reviewtask >>| return Void
	}
  ]

reviewtask :: Task (QForm,Review)
reviewtask = taskToReview 1 (defaultValue, mytask)

mytask :: a -> (Task a) | iTask a
mytask v =	updateInformation "Fill in Form:" v

taskToReview :: UserId (a,a -> Task a) -> Task (a,Review) | iTask a 
taskToReview reviewer (v`,task) 
	=					task v`               
		>>= \v ->		reviewer @: ("Review", review v) 
		>>= \r ->		showMessageAbout [Text ("Reviewer " <+++ reviewer <+++ " says ")] r 
		>>|				case r of
							(NeedsRework _) -> taskToReview reviewer (v,task) 	
							else            -> return (v,r)

review :: a -> Task Review | iTask a 
review v
	=	enterChoiceAbout "What is your verdict?" v
			[ updateInformation "Please add your comments" (NeedsRework defaultValue) <<@ "Rework"
			, return Approved <<@ "Approved"
			, return Rejected <<@ "Reject"
			]
	>>= \task -> task
