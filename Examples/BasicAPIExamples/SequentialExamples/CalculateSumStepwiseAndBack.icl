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
	step1 n1 n2		=		updateInformation [UpdateWithTitle "Number 1", UpdateWithHint "Enter the first number"] n1
						>>*	[ OnAction ActionNext (hasValue (\n1 -> step2 n1 n2))
							]
	step2 n1 n2		=		updateInformation [UpdateWithTitle "Number 2", UpdateWithHint "Enter the second number"] n2
						>>*	[ OnAction ActionPrevious (always 	(step1 n1 n2))
							, OnAction ActionNext     (hasValue (\n2 -> step3 n1 n2))]
	step3 n1 n2		=		viewInformation [ViewWithTitle "Sum",ViewWithHint "The sum of those numbers is:"] (n1 + n2)
						>>*	[ OnAction ActionPrevious	(always 	(step2 n1 n2))
						  	, OnAction ActionOk  		(always  	(return (n1 + n2)))
						  	]
