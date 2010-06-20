implementation module ExceptionHandling

import iTasks


//Two different exception types
:: NegativeValueException = NegativeValueException String
:: TooLargeValueException = TooLargeValueException String

derive gVisualize	NegativeValueException, TooLargeValueException
derive gUpdate		NegativeValueException, TooLargeValueException
derive gPrint		NegativeValueException, TooLargeValueException
derive gParse		NegativeValueException, TooLargeValueException
derive gError		NegativeValueException, TooLargeValueException
derive gHint		NegativeValueException, TooLargeValueException

derive bimap (,), Maybe

exceptionHandlingExample :: [Workflow]
exceptionHandlingExample
= [workflow "Examples/Higher order/Exception handling" exceptionTask]

exceptionTask :: Task Void
exceptionTask = Subject "Exception example" @>> (try (try normalTask (catchNegativeValueTask normalTask)) (catchTooLargeValueTask normalTask))

db :: (DBid Int)
db = mkDBid "MyIntDB"

normalTask :: Task Void
normalTask
	= forever (				readDB db
		>>= \initval 	->	updateInformation msg initval
		>>= \setval		->	inspectVal setval
		>>= \setval		->	writeDB db setval
		>>|					return Void
		)
where
	msg = "Please enter only values between 0 and 100"
	
	inspectVal val
		| val < 0	= throw (NegativeValueException "Negative value entered")
		| val > 100	= throw (TooLargeValueException "Too large value entered")
		| otherwise	= return val


catchNegativeValueTask :: (Task Void) NegativeValueException  -> Task Void
catchNegativeValueTask task (NegativeValueException msg) 
	=	readDB db
	>>=	\curval ->
		showMessageAbout 
			[Text "A NegativeValueException occurred: ",Text msg, BrTag []
			,Text "The current stored value is: "
			] curval
		

catchTooLargeValueTask :: (Task Void) TooLargeValueException  -> Task Void
catchTooLargeValueTask task (TooLargeValueException msg) 
	=	showMessage "A TooLargeValueException occurred, please try again"
	>>| try (try task (catchNegativeValueTask task)) (catchTooLargeValueTask task)	