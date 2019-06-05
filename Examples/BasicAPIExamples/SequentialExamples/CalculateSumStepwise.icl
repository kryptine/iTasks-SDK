implementation module BasicAPIExamples.SequentialExamples.CalculateSumStepwise

// Calculate the sum of two numbers given in sequence

import iTasks

wf :: String -> Workflow
wf a = workflow a "Calculate the Sum Stepwise" calculateSumStepwise

main :: Task ()
main = calculateSumStepwise @! ()

calculateSumStepwise :: Task Int
calculateSumStepwise
  	=   			enterInformation [EnterWithTitle "Number 1", EnterWithHint "Enter a number"]
  	>>= \num1 ->	enterInformation [EnterWithTitle "Number 2", EnterWithHint "Enter another number"]
  	>>= \num2 ->    viewInformation [ViewWithTitle "Sum", ViewWithHint "The sum of those numbers is:"] (num1 + num2)
	>>= 			return
