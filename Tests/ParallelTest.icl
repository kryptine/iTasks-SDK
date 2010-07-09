module ParallelTest

import iTasks

from TaskTree import ::TaskParallelType{..}

derive gPrint Either
derive gParse Either

derive bimap Maybe

simpleTest :: Task Int
simpleTest =
	getCurrentUser >>=
	\user -> user @: (Subject "Assigned Task" @>> enterInformation "Value 1")

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
 	allProc [ NamedUser "erik" @>> enterInformation "Please enter a number"
 	        , NamedUser "rinus" @>> enterInformation "Please enter a number"
 	        ] Open

parClosedTest :: Task [Int]
parClosedTest =
	allProc [ NamedUser "erik" @>> enterInformation "Please enter a number"
	        , NamedUser "rinus" @>> enterInformation "Please enter a number"
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
			workflow "Simple Test" (simpleTest >>= showMessageAbout "Result"),
			workflow "Or Test" (orTest  >>= showMessageAbout "Result"),
			workflow "And Test" (andTest  >>= showMessageAbout "Result"),
			workflow "Any Test" (anyTest  >>= showMessageAbout "Result"),
			workflow "All Test" (allTest  >>= showMessageAbout "Result"),
			workflow "Either Test" (eitherTest  >>= showMessageAbout "Result"),
			workflow "Maybe Test" (maybeTest >>= showMessageAbout "Result"),
			//workflow "Extender Test" (parExtendTest >>= showMessageAbout "You created this many additional tasks:"),
			workflow "Open Test" (parOpenTest >>= showMessageAbout "Result"),
			workflow "Closed Test" (parClosedTest >>= showMessageAbout "Result"),
			workflow "Nested Test 1" (parNestedTest1 >>= showMessageAbout "Result"),
			workflow "Nested Test 2" (parNestedTest2 >>= showMessageAbout "Result"),
			workflow "Nested Test 4" (parNestedTest4 >>= showMessageAbout "Result"),
			workflow "Nested Test 5" (parNestedTest5 >>= showMessageAbout "Result")
		] world 