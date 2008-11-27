module test

import StdEnv, StdiTasks, iData

// Test program to experiment with the new ExtJS based Web-GUI
derive gForm []
derive gUpd []

Start :: *World -> *World
Start world = startTaskEngine  myTask world

myTask :: Task Void
myTask =
	2 @:: editTask "Get Started" Void												#>>
	3 @:: ([Text "What do you want to tell your boss?"] ?>> editTask "Shout" "")	=>> \msg ->
	2 @:: ([Text "Worker says: ",Text msg] ?>> editTask "Ok" Void)
	
/*
Start :: *World -> *World
Start world = startTaskEngine (( (0 @:: processTask) -&&-  (0 @:: myTask))  )world

myTask :: Task String
myTask = editTask "Done" "Enter your name..." -||- (0 @: ("nieuwe taak",editTask "Done" "Hoi Rinus"))

//Editor of a task which uses specialize to create a separate view
//processTask :: Task (Wid HtmlDate)
processTask
//= editTask "Done" 44
=   spawnWorkflow 0 True ("Date Process",editTask "Done" 5)
 =>> \w -> return_V True 

*/
/*
:: Fruit	= Apples | Oranges | Grapes  

:: MyRec	= { name			:: Maybe String
			  , age				:: Int
			  , favoriteFruit	:: Fruit
			  }

derive gForm  Fruit, MyRec
derive gUpd   Fruit, MyRec
derive gPrint Fruit, MyRec
derive gParse Fruit, MyRec

Start :: *World -> *World
Start world = startTaskEngine ( (0 @:: dateTask) -&&-  (0 @:: prTask) )world

myTask :: Task MyRec
myTask = editTask "Done" createDefault


prTask :: Task Bool
prTask 
=				spawnWorkflow 0 True ("nieuwe taak",myTask)
	=>> \_ -> 	return_V True

cbTask :: Task HtmlCheckbox
cbTask = editTask "I'm Done" (HtmlCheckbox [Text "Click me!"] False)

boolTask :: Task Bool
boolTask = editTask "Done with the bool" True

fruitTask :: Task Fruit
fruitTask = editTask "Done" createDefault

//Editor of a task which uses specialize to create a separate view
dateTask :: Task HtmlDate
dateTask = editTask "Done" createDefault
*/