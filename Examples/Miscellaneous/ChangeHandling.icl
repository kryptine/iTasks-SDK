module ChangeHandling

import iTasks, iDataTrivial
import StdMisc


Start world = startEngine exceptionHandlingExample world

exceptionHandlingExample :: [Workflow]
exceptionHandlingExample
=	[{ name		= "Change handling"
	 , label	= "Change example"
	 , roles	= []
	 , mainTask	= editSpecial 0 >>| return Void
	 }]

editSpecial :: Int -> Task Int
editSpecial i 
	= 				chooseTask []
						[( "normal",	mytask)
						, ("absurd",	raiseChange (RC (pred 2)) ourList mytask)
						] >>= editSpecial
	where
		mytask = editTask ("OK" <+++ i) i  <\/> myChange (editSpecial i)

		pred _ tst = (True,Nothing,tst)
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
