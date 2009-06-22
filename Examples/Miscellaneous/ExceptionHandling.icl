implementation module ExceptionHandling

import iTasks, iTasksDB


//Two different exception types
:: NegativeValueException = NegativeValueException String
:: TooLargeValueException = TooLargeValueException String

derive gForm	NegativeValueException, TooLargeValueException
derive gUpd		NegativeValueException, TooLargeValueException
derive gPrint	NegativeValueException, TooLargeValueException
derive gParse	NegativeValueException, TooLargeValueException

exceptionHandlingExample :: [Workflow]
exceptionHandlingExample
=	[{ name		= "Examples/Miscellaneous/Exception handling"
	 , label	= "Exception example"
	 , roles	= []
	 , mainTask	= exceptionTask
	 }]

exceptionTask :: Task Void
exceptionTask = try (try normalTask (catchNegativeValueTask normalTask)) (catchTooLargeValueTask normalTask)

db :: (DBid Int)
db = mkDBid "MyIntDB" LSTxtFile

normalTask :: Task Void
normalTask
	= forever (				readDB db
		>>= \initval 	->	(msg ?>> editTask "Set" initval)
		>>= \setval		->	inspectVal setval
		>>= \setval		->	writeDB db setval
		>>|					return Void
		)
where
	msg = [Text "Please enter only values between 0 and 100"]
	
	inspectVal val
		| val < 0	= throw (NegativeValueException "Negative value entered")
		| val > 100	= throw (TooLargeValueException "Too large value entered")
		| otherwise	= return val


catchNegativeValueTask :: (Task Void) NegativeValueException  -> Task Void
catchNegativeValueTask task (NegativeValueException msg) 
	=		readDB db
	>>=	\curval ->
		[Text "A NegativeValueException occurred: ",Text msg, BrTag []
		,Text "The current stored value is: ", Text (toString curval)
		] ?>> button "Ok" Void
		

catchTooLargeValueTask :: (Task Void) TooLargeValueException  -> Task Void
catchTooLargeValueTask task (TooLargeValueException msg) 
	=	[Text "A TooLargeValueException occurred, please try again"] ?>> button "Ok" Void
	>>| try (try task (catchNegativeValueTask task)) (catchTooLargeValueTask task)	