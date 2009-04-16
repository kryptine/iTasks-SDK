implementation module ChangeHandling

import iTasks, iDataTrivial
import StdMisc


//Start world = startEngine exceptionHandlingExample world

changeHandlingExample :: [Workflow]
changeHandlingExample
=	[{ name		= "Examples/Miscellaneous/Change handling"
	 , label	= "Change example"
	 , roles	= []
	 , mainTask	= doTest >>| return Void
	 }]

editSpecial :: Int -> Task Int
editSpecial i 
	= 				chooseTask []
						[( "normal",	mytask)
						, ("absurd",	pushChangeRequest (CC (pred 2)) ourList mytask)
						] >>= editSpecial
	where
		mytask = editTask ("OK" <+++ i) i  <\/> myChange (editSpecial i)

		pred _ tst =	({newCondition = Nothing, 				 changePred = False, makeChange = True},tst)
//		pred n tst = (True,Just (RC (pred (n-1))),tst)
		
ourList :: [Dynamic]
ourList
	=				[ dynamic ("Int task",		editTask "repair" 1)
					, dynamic ("Bool task",		editTask "repair" True)
					, dynamic ("String task", 	editTask "repair" "Hello world")
					]

myChange task ourList
	= chooseTask [] (tasksOfSameType task ourList)
where
	tasksOfSameType :: (Task a) [Dynamic] -> [LabeledTask a] | iData a
	tasksOfSameType t [] = [("Standard Task",t),("Simple Editor",editTask "Edit" createDefault)]
	tasksOfSameType t [(t` :: LabeledTask a^) : ts] = [t` : tasksOfSameType t ts]
	tasksOfSameType t [t` : ts] = tasksOfSameType t ts
	


edit` val = editTask ("Normal OK" <+++ val) val  <\/> alternative val
where
	alternative :: a Void -> Task a | iData a & toString a
	alternative val _ = 	editTask ("Alternative OK" <+++ val) val
	

myBunch 
	= 	parallel "andTasks"  (\_ -> False) id  [(toString i, edit` i) \\ i <- [0..5]] >>|
		parallel "andTasks"  (\_ -> False) id  [(toString i, edit` i) \\ i <- [0..5]]


doTest = pushChangeRequest (CC (pred 50)) Void myBunch
where
		pred 0 tst =	({newCondition = Nothing, 				 changePred = False, makeChange = False},tst)
		pred n tst =	({newCondition = Just (CC (pred (n-1))), changePred = True,  makeChange = isEven n},tst)



