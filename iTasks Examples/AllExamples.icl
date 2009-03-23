module AllExamples

import iTasks

//Business examples

import Vote

//Miscellaneous examples
import Coffeemachine
import Newsgroups
import ExceptionHandling

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = flatten [ voteExample
						, coffeemachineExample
						, newsgroupsExample
						, exceptionHandlingExample
						]