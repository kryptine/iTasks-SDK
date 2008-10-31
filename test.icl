module test

import StdEnv, StdiTasks, iData

// Test program to experiment with the new ExtJS based Web-GUI

Start :: *World -> *World
Start world = singleUserTask [] ( (0 @:: dateTask) -&&-  (0 @:: myTask) )world

myTask :: Task String
myTask = editTask "Done" "Enter your name..."

//Editor of a task which uses specialize to create a separate view
dateTask :: Task HtmlDate
dateTask = editTask "Done" createDefault