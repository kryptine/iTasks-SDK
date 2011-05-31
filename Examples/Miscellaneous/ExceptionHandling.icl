implementation module ExceptionHandling

import iTasks


//Two different exception types
:: NegativeValueException = NegativeValueException String
:: TooLargeValueException = TooLargeValueException String

derive bimap Maybe,(,)
derive class iTask NegativeValueException, TooLargeValueException

instance toString NegativeValueException
where
	toString (NegativeValueException err) = err
	
instance toString TooLargeValueException
where
	toString (TooLargeValueException err) = err

exceptionHandlingExample :: [Workflow]
exceptionHandlingExample
= [workflow "Examples/Higher order/Exception handling" "Exception handling demo" exceptionTask]

exceptionTask :: Task Int
exceptionTask = Title "Exception example" @>> (try (try normalTask (catchNegativeValueTask normalTask)) (catchTooLargeValueTask normalTask))

db :: SymmetricShared Int
db = sharedStore "MyIntDB" 0

normalTask :: Task Int
normalTask
	= forever (				get db
		>>= \initval 	->	updateInformation (subj,msg) initval
		>>= \setval		->	inspectVal setval
		>>= \setval		->	set db setval
		)
where
	subj :: String
	subj= "Enter a number"
	msg = "Please enter only values between 0 and 100"
	
	inspectVal val
		| val < 0	= throw (NegativeValueException "Negative value entered")
		| val > 100	= throw (TooLargeValueException "Too large value entered")
		| otherwise	= return val


catchNegativeValueTask :: (Task Int) NegativeValueException -> Task Int
catchNegativeValueTask task (NegativeValueException msg)
	=	get db
	>>=	\curval ->
		showMessageAbout ("Exception!",
			[Text "A NegativeValueException occurred: ",Text msg, BrTag []
			,Text "The current stored value is: "
			]) curval
		

catchTooLargeValueTask :: (Task Int) TooLargeValueException  -> Task Int
catchTooLargeValueTask task (TooLargeValueException msg) 
	=	showMessage ("Exception!","A TooLargeValueException occurred, please try again") Void
	>>| try (try task (catchNegativeValueTask task)) (catchTooLargeValueTask task)	