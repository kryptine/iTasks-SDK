module test

import StdEnv, StdiTasks, iData

// Test program to experiment with the new ExtJS based Web-GUI

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
//Start world = singleUserTask [] ( (0 @:: dateTask) -&&-  (0 @:: myTask) )world
Start world = singleUserTask [] myTask world

myTask :: Task MyRec
myTask = editTask "Done" createDefault <<@ Submit


cbTask :: Task HtmlCheckbox
cbTask = editTask "I'm Done" (HtmlCheckbox [Text "Click me!"] False)

boolTask :: Task Bool
boolTask = editTask "Done with the bool" True

fruitTask :: Task Fruit
fruitTask = editTask "Done" createDefault

//Editor of a task which uses specialize to create a separate view
dateTask :: Task HtmlDate
dateTask = editTask "Done" createDefault