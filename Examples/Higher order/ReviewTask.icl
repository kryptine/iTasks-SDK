implementation module ReviewTask

import iTasks

// (c) 2007 MJP

// A task is given to user 0
// When finished the result of the task is reviewed by user 1
// He can comment on the task, or approve or cancel it
// When the result needs more work, the whole process is repeated
// Otherwise the task is completed
// The task itself in the example is a quotation form that needs to be filled in

derive class iTask	QForm, Review, Person, Gender
derive bimap (,), Maybe

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
= [workflow "Examples/Higher order/Review task" "Demo of an iterative process" (Description "Review the results of a task" @>> reviewtask)]

reviewtask :: Task (QForm,Review)
reviewtask = taskToReview AnyUser (defaultValue, mytask)

mytask :: a -> (Task a) | iTask a
mytask v =	updateInformation ("Form","Fill in Form:") [] v

taskToReview :: User (a,a -> Task a) -> Task (a,Review) | iTask a 
taskToReview reviewer (v`,task) 
	=					task v`               
		>>= \v ->		reviewer @: (Description "Review" @>> review v) 
		>>= \r ->		showInformation ("Review",[Text ("Reviewer " <+++ reviewer <+++ " says ")]) [About r] Void
		>>|				case r of
							(NeedsRework _) -> taskToReview reviewer (v,task) 	
							else            -> return (v,r)

review :: a -> Task Review | iTask a 
review v
	=	enterChoice ("Review","What is your verdict?") [About v]
			[ updateInformation ("Comments","Please add your comments") [] (NeedsRework (Note "")) <<@ Description "Rework"
			, return Approved <<@ Description "Approved"
			, return Rejected <<@ Description "Reject"
			]
	>>= \task -> task
