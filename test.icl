module test

import StdEnv, iTasks, iData

// Test program to experiment with the new ExtJS based Web-GUI
derive gForm TestRec, []
derive gUpd TestRec, []

derive gPrint TestRec, TestBox
derive gParse TestRec, TestBox

:: TestBox = TestBox Bool Bool Bool

gForm{|TestBox|} (init,formid) hst
	# (cbform, hst)				= ListFuncCheckBox (init, reuseFormId formid options) hst
	//# (html1,inputs1,hst)		= mkCheckBox (init,formid) "Bool" [] b1 hst
	//# (html2,inputs2,hst)		= mkCheckBox (init,formid) "Bool" [] b2 hst
	= ({ changed				= False
	   , value					= (fst cbform.Form.value) formid.ival
	   , form					= cbform.form
	   , inputs					= cbform.inputs
	   },hst)
where
	(TestBox b1 b2 b3)	= formid.ival
	options			= [(HtmlCheckbox [Text "Part 1"] b1, set1), (HtmlCheckbox [Text "Part 2"] b2, set2),(HtmlCheckbox [Text "Part 3"] b3, set3)]

	set1 b _ (TestBox b1 b2 b3) = (TestBox b b2 b3)
	set2 b _ (TestBox b1 b2 b3) = (TestBox b1 b b3)
	set3 b _ (TestBox b1 b2 b3) = (TestBox b1 b2 b)

derive gUpd TestBox

//ListFuncCheckBox	:: !(InIDataId [(HtmlCheckbox, Bool [Bool] a -> a)])	!*HSt -> (Form (a -> a,[Bool]),!*HSt)


:: TestRec = 	{ myint 	:: Int
				, myreal	:: Real
				, mystring	:: String
				, mybool	:: Bool
				, mymaybe	:: Maybe String
				}

Start :: *World -> *World
Start world = startEngine initialWorkflows world

initialWorkflows = [
	{ Workflow
	| name	= "spawn"
	, label = "Spawn a dynamic Process"
	, roles = []
	, mainTask	= spawnTask
	},	
	{ Workflow
	| name	= "simpleint"
	, label	= "Define an integer"
	, roles = []
	, mainTask	= (intTask #>> return_V Void)
	},
	{ Workflow
	| name	= "sum"
	, label = "Simple addition"
	, roles = []
	, mainTask = simpleSum
	},
	{ Workflow
	| name = "parallelSums"
	, label = "Parallel addition"
	, roles = []
	, mainTask = (simpleSum -&&- simpleSum) #>> return_V Void
	},
	{ Workflow
	| name	= "infinite"
	, label = "Test with infinite list"
	, roles = []
	, mainTask = infiniteListTask
	},
	{ Workflow
	| name	= "newtask"
	, label	= "Test for newTask"
	, roles = []
	, mainTask = newTaskTest
	}
	]

spawnTask :: Task Void
spawnTask = spawnProcess 0 True ("Give me an Int",intTask)	=>> \pid ->
			waitForProcess pid								=>> \result ->
			case result of
				Just i	= (
					[Text "The result was: ",Text (toString i)]
					?>>
					editTask "OK" Void
					)
				Nothing	= (
					[Text "There was no result"]
					?>>
					editTask "OK" Void
					)


intTask :: Task Int
intTask = editTask "Done" 42

simpleSum :: Task Void
simpleSum =
	intTask =>> \val1 ->
	intTask =>> \val2 ->
	[Text ("Sum is " +++ (toString (val1 + val2)))] ?>> editTask "Ok" Void

infiniteListTask :: Task Void
infiniteListTask =
	return_V [10..]	=>>	\val1 ->
	return_V (hd val1) =>> \val2 ->
	intTask		=>> \val3 ->
	[Text ("Sum = " +++ (toString (val2 + val3)))] ?>> editTask "Ok" Void

newTaskTest :: Task Void
newTaskTest =
	(newTask "TEST" intTask) #>> return_V Void 


myBoxTask :: Task TestBox
myBoxTask = editTask "Done" (TestBox True False True)

myTask6 :: Task TestRec
myTask6 = editTask "Ok" createDefault

myTask5
= seqTasks  
	[("Coffee: 100",    editTask "OK" (100,"Coffee"))
	,("Cappucino: 150", editTask "OK" (150,"Cappucino"))
	,("Tea: 50",        editTask "OK" (50, "Tea"))
	,("Chocolate: 100", editTask "OK" (100,"Chocolate"))
	] 
	=>> \v -> editTask "OK" v
	

myTask4 
=	seqTasks [("taak1",ed 0 0),("taak2", ed 1 1)] 

myTask3
=	myTask -||- myTask


myTask2 
= 				0 @:: ed 0 0
	=>> \v ->	0 @:: ed 1 v
	=>> \v ->	0 @:: ed 2 v


myTask 
= 				ed 0 0
	=>> \v ->	ed 1 v
	=>> \v ->	ed 2 v


ed i j = editTask ("OK " <+++ i) j