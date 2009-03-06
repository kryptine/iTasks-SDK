module exception

import iTasks
import StdInt, StdClass
import iTasksDB 

Start :: *World -> *World
Start world = startEngine [flow] world

//Two different exception types
:: NegativeValueException = NegativeValueException String
:: TooLargeValueException = TooLargeValueException String

derive gForm	NegativeValueException, TooLargeValueException
derive gUpd		NegativeValueException, TooLargeValueException
derive gPrint	NegativeValueException, TooLargeValueException
derive gParse	NegativeValueException, TooLargeValueException

flow :: Workflow
flow =	{ name		= "exception"
		, label		= "Exception example"
		, roles		= []
		, mainTask	= exceptionTask
		}

exceptionTask :: Task Void
exceptionTask = normalTask <^> catchNegativeValueTask <^> catchTooLargeValueTask

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
		| val < 0	= raise (NegativeValueException "Negative value entered")
		| val > 100	= raise (TooLargeValueException "Too large value entered")
		| otherwise	= return val


catchNegativeValueTask :: NegativeValueException (Task Void) -> Task Void
catchNegativeValueTask (NegativeValueException msg) task
	=		readDB db
	>>=	\curval ->
		[Text "A NegativeValueException occurred: ",Text msg, BrTag []
		,Text "The current stored value is: ", Text (toString curval)
		] ?>> button "Ok" Void
		

catchTooLargeValueTask :: TooLargeValueException (Task Void) -> Task Void
catchTooLargeValueTask (TooLargeValueException msg) task
	=	[Text "A TooLargeValueException occurred, please try again"] ?>> button "Ok" Void
	>>| (task <^> catchNegativeValueTask <^> catchTooLargeValueTask)
	