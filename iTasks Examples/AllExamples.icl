module AllExamples

import iTasks

//Miscellaneous examples
import Coffeemachine
import Newsgroups
import ExceptionHandling

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ coffeemachineExample
						, newsgroupsExample
						, exceptionHandlingExample
						]