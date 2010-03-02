module ParallelTest

import iTasks

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
	parallel  "Extender" extfunc id 0 [(Nothing,task)]
where
	extfunc :: (Int,Int) Int -> (Int,PAction Int)
	extfunc (val,idx) cnt
		| val == 0 = (cnt,Stop)
		# addTasks = repeatn val (Nothing,task)
		= (cnt+val,Extend addTasks)

	task = enterInformation "How many new tasks do you want? (0 is stop)"

Start :: *World -> *World
Start world = startEngine [
			workflow "Or Test" (orTest  >>= showMessageAbout "Result"),
			workflow "And Test" (andTest  >>= showMessageAbout "Result"),
			workflow "Any Test" (anyTest  >>= showMessageAbout "Result"),
			workflow "All Test" (allTest  >>= showMessageAbout "Result"),
			workflow "Either Test" (eitherTest  >>= showMessageAbout "Result"),
			workflow "Maybe Test" (maybeTest >>= showMessageAbout "Result"),
			workflow "Extender Test" (parExtendTest >>= showMessageAbout "You created this many additional tasks:")
		] world 