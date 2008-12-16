module test

import StdEnv, StdiTasks, iData

// Test program to experiment with the new ExtJS based Web-GUI
derive gForm []
derive gUpd []



Start :: *World -> *World
Start world = startTaskEngine myTask5 world

myTask5
= seqTasks  
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