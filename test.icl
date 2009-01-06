module test

import StdEnv, StdiTasks, iData

// Test program to experiment with the new ExtJS based Web-GUI
derive gForm []
derive gUpd []



Start :: *World -> *World
Start world = startTaskEngine ("My mian example", StartUp simples) 0 world

StartUp :: [Task a] -> Task Void | iData a
StartUp tasks = foreverTask selectOne
where
	selectOne
	=			chooseTask [Text "Startup a new task"] 
					[ ("Start Task " <+++ i, startProcess ("Start Task " <+++ i, task)) \\ task <- tasks & i <- [0..]]
		#>>		return_V Void



startProcess (label, task)
	=			spawnWorkflow 0 True (label, mytask)
		#>>		return_V Void
where
	mytask 
	=			task
//		#>>		deleteMe
//		#>> 	return_V Void


simples = [editTask "OK 0" 0, editTask "OK 1" 1, editTask "OK 2" 2]


myTask5
= andTasks  
	[("Coffee: 100",    editTask "OK" (100,"Coffee"))
	,("Cappucino: 150", editTask "OK" (150,"Cappucino"))
	,("Tea: 50",        editTask "OK" (50, "Tea"))
	,("Chocolate: 100", editTask "OK" (100,"Chocolate"))
	] 
	=>> \v -> editTask "OK" v
	

myTask4 
=	seqTasks [("taak1",ed 0 0),("taak2", ed 1 1)] 

myTask3
=	myTask -||- myTask


myTask2 
= 				0 @:: ed 0 0
	=>> \v ->	0 @:: ed 1 v
	=>> \v ->	0 @:: ed 2 v


myTask 
= 				ed 0 0
	=>> \v ->	ed 1 v
	=>> \v ->	ed 2 v


ed i j = editTask ("OK " <+++ i) j