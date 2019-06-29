implementation module BasicAPIExamples.SequentialExamples.CalculateSumStepwise

// Calculate the sum of two numbers given in sequence

import iTasks

wf :: String -> Workflow
wf a = workflow a "Calculate the Sum Stepwise" calculateSumStepwise

main :: Task ()
main = calculateSumStepwise @! ()

calculateSumStepwise :: Task Int
calculateSumStepwise
  	=   			Title "Number 1" @>> Hint "Enter a number" @>> enterInformation []
  	>>= \num1 ->	Title "Number 2" @>> Hint "Enter another number" @>> enterInformation []
  	>>= \num2 ->    Title "Sum" @>> Hint "The sum of those numbers is:" @>> viewInformation [] (num1 + num2)
	>>= 			return
