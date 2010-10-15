module ParallelTest

import iTasks

from TaskTree import ::TaskParallelType{..}

derive bimap Maybe

simpleTest :: Task Int
simpleTest =
	getCurrentUser >>=
	\user -> user @: enterInformation "Assigned Task" "Value 1"

orTest :: Task Int
orTest = (enterInformation "Value1" "Enter value 1" -||- enterInformation "Value2" "Enter value 2")

andTest :: Task (Int,String)
andTest = (enterInformation "Value 1" "Enter value 1" -&&- (enterInformation "Value 2" "Enter value 2" <<@ GBFloating))

anyTest :: Task Note
anyTest = anyTask [enterInformation "Value 1" "Enter value 1" ,enterInformation "Value 2" "Enter value 2",enterInformation "Value 3" "Enter value 3"]

allTest :: Task [Note]
allTest = allTasks [enterInformation "Value 1" "Enter value 1",enterInformation "Value 2" "Enter value 2",enterInformation "Value 3" "Enter value 3"]

eitherTest :: Task (Either Note Int)
eitherTest = eitherTask (enterInformation "Value 1" "Enter value 1") (enterInformation "Value 2" "Enter value 2")

maybeTest :: Task (Maybe (Int,Note))
maybeTest = (enterInformation "Value 1" "Enter value 1" -&?&- enterInformation "Value 2" "Enter value 2")

/*
parExtendTest :: Task Int
parExtendTest =
	group  "Extender" "" extfunc id 0 [task] []
where
	extfunc :: (Int,Int) Int -> (Int,PAction (Task Int))
	extfunc (val,idx) cnt
		| val == 0 = (cnt,Stop)
		# addTasks = repeatn val task
		= (cnt+val,Extend addTasks)

	task = enterInformation "How many new tasks do you want? (0 is stop)"
*/

parOpenTest :: Task [Int]
parOpenTest = 
 	allProc [ NamedUser "erik" @>> enterInformation "Number" "Please enter a number" 
 	        , NamedUser "rinus" @>> enterInformation "Number" "Please enter a number"
 	        ] Open

parClosedTest :: Task [Int]
parClosedTest =
	allProc [ NamedUser "erik" @>> enterInformation "Number" "Please enter a number"
	        , NamedUser "rinus" @>> enterInformation "Number" "Please enter a number"
	        ] Closed
		        
parNestedTest1 :: Task [[Int]]
parNestedTest1 =
	allProc [ NamedUser "erik" @>> parOpenTest
	        , NamedUser "rinus" @>> parOpenTest
	        ] Open

parNestedTest2 :: Task [[Int]]
parNestedTest2 =
	allProc [ NamedUser "erik" @>> parClosedTest
	        , NamedUser "rinus" @>> parClosedTest
	        ] Open



parNestedTest4 :: Task[[Note]]
parNestedTest4 =
	allProc [ NamedUser "erik" @>> allTest
	        , NamedUser "erik" @>> allTest
	        ] Open
	        
parNestedTest5 :: Task ([Int],[Int])
parNestedTest5 = 
	parOpenTest -&&- parOpenTest

Start :: *World -> *World
Start world = startEngine [
			workflow "Simple Test" "Simple Test" (simpleTest >>= showMessageAbout "Result" "The result is:"),
			workflow "Or Test" "Or Test" (orTest  >>= showMessageAbout "Result" "The result is:"),
			workflow "And Test" "And Test" (andTest  >>= showMessageAbout "Result" "The result is:"),
			workflow "Any Test" "Any Test" (anyTest  >>= showMessageAbout "Result" "The result is:"),
			workflow "All Test" "All Test" (allTest  >>= showMessageAbout "Result" "The result is:"),
			workflow "Either Test" "Either Test" (eitherTest  >>= showMessageAbout "Result" "The result is:"),
			workflow "Maybe Test" "Maybe Test" (maybeTest >>= showMessageAbout "Result" "The result is:"),
			workflow "Open Test" "Open Test" (parOpenTest >>= showMessageAbout "Result" "The result is:"),
			workflow "Closed Test" "Closed Test" (parClosedTest >>= showMessageAbout "Result" "The result is:"),
			workflow "Nested Test 1" "Nested Test 1" (parNestedTest1 >>= showMessageAbout "Result" "The result is:"),
			workflow "Nested Test 2" "Nested Test 2" (parNestedTest2 >>= showMessageAbout "Result" "The result is:"),
			workflow "Nested Test 4" "Nested Test 4" (parNestedTest4 >>= showMessageAbout "Result" "The result is:"),
			workflow "Nested Test 5" "Nested Test 5" (parNestedTest5 >>= showMessageAbout "Result" "The result is:")
		] world 