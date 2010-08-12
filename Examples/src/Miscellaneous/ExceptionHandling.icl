implementation module ExceptionHandling

import iTasks


//Two different exception types
:: NegativeValueException = NegativeValueException String
:: TooLargeValueException = TooLargeValueException String

derive class iTask	NegativeValueException, TooLargeValueException
derive bimap (,), Maybe

exceptionHandlingExample :: [Workflow]
exceptionHandlingExample
= [workflow "Examples/Higher order/Exception handling" exceptionTask]

exceptionTask :: Task Int
exceptionTask = Subject "Exception example" @>> (try (try normalTask (catchNegativeValueTask normalTask)) (catchTooLargeValueTask normalTask))

db :: (DBid Int)
db = mkDBid "MyIntDB"

normalTask :: Task Int
normalTask
	= forever (				readDB db
		>>= \initval 	->	updateInformation subj msg initval
		>>= \setval		->	inspectVal setval
		>>= \setval		->	writeDB db setval
		)
where
	subj= "Enter a number"
	msg = "Please enter only values between 0 and 100"
	
	inspectVal val
		| val < 0	= throw (NegativeValueException "Negative value entered")
		| val > 100	= throw (TooLargeValueException "Too large value entered")
		| otherwise	= return val


catchNegativeValueTask :: (Task Int) NegativeValueException  -> Task Int
catchNegativeValueTask task (NegativeValueException msg) 
	=	readDB db
	>>=	\curval ->
		showMessageAbout "Exception!"
			[Text "A NegativeValueException occurred: ",Text msg, BrTag []
			,Text "The current stored value is: "
			] curval
		

catchTooLargeValueTask :: (Task Int) TooLargeValueException  -> Task Int
catchTooLargeValueTask task (TooLargeValueException msg) 
	=	showMessage "Exception!" "A TooLargeValueException occurred, please try again" Void
	>>| try (try task (catchNegativeValueTask task)) (catchTooLargeValueTask task)	