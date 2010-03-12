module ParallelTest

import iTasks

from TaskTree import ::TaskParallelType{..}

derive gPrint Either
derive gParse Either

derive bimap Maybe

orTest :: Task Int
orTest = (enterInformation "Value 1" -||- enterInformation "Value 2")

andTest :: Task (Int,String)
andTest = (enterInformation "Value 1" -&&- enterInformation "Value 2")

anyTest :: Task Note
anyTest = anyTask [enterInformation "Value 1",enterInformation "Value 2",enterInformation "Value 3"]

allTest :: Task [Note]
allTest = allTasks [enterInformation "Value 1",enterInformation "Value 2",enterInformation "Value 3"]

eitherTest :: Task (Either Note Int)
eitherTest = eitherTask (enterInformation "Value 1") (enterInformation "Value 2")

maybeTest :: Task (Maybe (Int,Note))
maybeTest = (enterInformation "Value 1" -&?&- enterInformation "Value 2")

parExtendTest :: Task Int
parExtendTest =
	parallel  "Extender" "Keeps adding tasks until user enters 0" extfunc id 0 [task]
where
	extfunc :: (Int,Int) Int -> (Int,ParallelAction Int)
	extfunc (val,idx) cnt
		| val == 0 = (cnt,Stop)
		# addTasks = repeatn val task
		= (cnt+val,Extend addTasks)

	task = enterInformation "How many new tasks do you want? (0 is stop)"

parOpenTest :: Task [Int]
parOpenTest = 
 	allTasksExt [
				("erik",  enterInformation "Please enter a number"),
				("rinus", enterInformation "Please enter a number")] Open

parClosedTest :: Task [Int]
parClosedTest =
		allTasksExt [
				("erik",  enterInformation "Please enter a number"),
				("rinus", enterInformation "Please enter a number")] Closed

parNestedTest1 :: Task [[Int]]
parNestedTest1 =
	allTasksExt [
		("erik",parOpenTest),
		("rinus",parOpenTest)
	] Open

parNestedTest2 :: Task [[Int]]
parNestedTest2 =
	allTasksExt [
		("erik",parClosedTest),
		("rinus",parClosedTest)
	] Open

parNestedTest3 :: Task [[String]]
parNestedTest3 =
	allTasksExt [
		("erik", subtask1),
		("rinus", subtask2),
		("erik", subtask3)
	] Open
where
	subtask1 :: Task [String]
	subtask1 = 
		getUsers >>= 
		enterChoice "Please select a user" >>= \user ->
		user @: ("Subtask 1", enterInformation "List of Strings")
		
	subtask2 :: Task [String]
	subtask2 =
		enterInformation "Sorted list of Strings" >>= \list ->
		return (sortBy (<) list)
		
	subtask3 :: Task [String]
	subtask3 =
		getUsers >>= \ulist ->
		enterMultipleChoice "Please select round-robin users" ulist >>= \users -> let usernames = map toUserId users in 
		parallelU "Weird string function" "Keeps extending until user types '.'" Open (func usernames) parse (0,[]) [(hd usernames,task)]
		where
			func :: ![UserId] !(String,Int) !(Int,[(Int,String)]) -> ((Int,[(Int,String)]),ParallelAction String)
			func usernames (result,pos) (idx,acc)
			| result == "." = ((idx,acc),Stop)
			# acc = [(pos,result):acc]
			# idx = ((idx+1) rem (length usernames))
			# usr = usernames !! idx
			= ((idx,acc),ExtendU [(usr,task)])
	
			task :: Task String
			task = enterInformation "Type String, '.' to stop"
			
			parse :: !(Int,[(Int,String)]) -> [String]
			parse b = snd (unzip (sortBy (\l -> \r -> (fst l) < (fst r))  (snd b)))
			

Start :: *World -> *World
Start world = startEngine [
			workflow "Or Test" (orTest  >>= showMessageAbout "Result"),
			workflow "And Test" (andTest  >>= showMessageAbout "Result"),
			workflow "Any Test" (anyTest  >>= showMessageAbout "Result"),
			workflow "All Test" (allTest  >>= showMessageAbout "Result"),
			workflow "Either Test" (eitherTest  >>= showMessageAbout "Result"),
			workflow "Maybe Test" (maybeTest >>= showMessageAbout "Result"),
			workflow "Extender Test" (parExtendTest >>= showMessageAbout "You created this many additional tasks:"),
			workflow "Open Test" (parOpenTest >>= showMessageAbout "Result"),
			workflow "Closed Test" (parClosedTest >>= showMessageAbout "Result"),
			workflow "Nested Test 1" (parNestedTest1 >>= showMessageAbout "Result"),
			workflow "Nested Test 2" (parNestedTest2 >>= showMessageAbout "Result"),
			workflow "Nested Test 3" (parNestedTest3 >>= showMessageAbout "Result")
		] world 