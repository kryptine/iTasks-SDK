implementation module BasicAPIExamples.SequentialExamples.CalculateSumStepwise

// Calculate the sum of two numbers given in sequence

import iTasks

wf :: String -> Workflow
wf a = workflow a "Calculate the Sum Stepwise" calculateSumStepwise

main :: Task ()
main = calculateSumStepwise @! ()

calculateSumStepwise :: Task Int
calculateSumStepwise
  	=   			enterInformation ("Number 1","Enter a number") []
  	>>= \num1 ->	enterInformation ("Number 2","Enter another number") []
  	>>= \num2 ->    viewInformation ("Sum","The sum of those numbers is:") [] (num1 + num2)
	>>= 			return
