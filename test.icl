module test

import StdEnv, StdiTasks, StdiData

// Test program to experiment with the new ExtJS based Web-GUI

Start :: *World -> *World
Start world = singleUserTask [] myTask world

myTask :: Task String
myTask = editTask "Done" "Enter your name..."