implementation module BasicAPIExamples.SequentialExamples.CalculateSumStepwiseAndBack

// Calculate the sum of two numbers in steps with option to go back to previous result

import iTasks

wf :: String -> Workflow
wf a = workflow a "Calculate Sum Stepwise and Back" calculateSumSteps

main :: Task ()
main = calculateSumSteps @! ()

calculateSumSteps :: Task Int
calculateSumSteps = step1 0 0
where
	step1 n1 n2		=		Title "Number 1" @>> Hint "Enter the first number" @>> updateInformation [] n1
						>>*	[ OnAction ActionNext (hasValue (\n1 -> step2 n1 n2))
							]
	step2 n1 n2		=		Title "Number 2" @>> Hint "Enter the second number" @>> updateInformation [] n2
						>>*	[ OnAction ActionPrevious (always 	(step1 n1 n2))
							, OnAction ActionNext     (hasValue (\n2 -> step3 n1 n2))]
	step3 n1 n2		=		Title "Sum" @>> Hint "The sum of those numbers is:" @>> viewInformation [] (n1 + n2)
						>>*	[ OnAction ActionPrevious	(always 	(step2 n1 n2))
						  	, OnAction ActionOk  		(always  	(return (n1 + n2)))
						  	]
