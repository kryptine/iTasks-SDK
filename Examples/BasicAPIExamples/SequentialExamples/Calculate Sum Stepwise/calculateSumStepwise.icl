module calculateSumStepwise

// Calculate the sum of two numbers given in sequence

import iTasks

Start :: *World -> *World
Start world 
	= startEngine calculateSumStepwise world
	
calculateSumStepwise :: Task Int
calculateSumStepwise
  	=   			enterInformation ("Number 1","Enter a number") []
  	>>= \num1 ->	enterInformation ("Number 2","Enter another number") []
  	>>= \num2 ->    viewInformation ("Sum","The sum of those numbers is:") [] (num1 + num2)
	>>= 			return