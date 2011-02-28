implementation module SmallExamples

import iTasks

smallExamples :: [Workflow]
smallExamples = [workflow		"Examples/Miscellaneous/Calculate sum" "Calculate the sum of two numbers" calculateSum
				,workflow		"Examples/Miscellaneous/Calculate sum (with stepback)" "Calculate the sum of two numbers, with stepback possibility" calculateSumSteps
				,workflowParam	"Examples/Miscellaneous/Calculate sum (parameterised)" "Calculate the sum of two numbers given as parameter" calculateSumParam
				]

calculateSum :: Task Int
calculateSum
  =   enterInformation ("Number 1","Enter a number")
  >>= \num1 ->
      enterInformation ("Number 2","Enter another number")
  >>= \num2 ->
      showMessageAbout ("Sum","The sum of those numbers is:") (num1 + num2)
        
calculateSumSteps :: Task Int
calculateSumSteps = step1First
where
	step1First			= enterInformationA ("Number 1","Enter a number") id [(ActionNext, ifvalid)]
						  >>= \(_,Just num1) -> step2First num1
	step1Back num1		= updateInformationA ("Number 1","Enter a number") idView [(ActionNext, ifvalid)] num1
						  >>= \(_,Just num1`) -> step2First num1`
	
	step2First num1		= enterInformationA ("Number 2","Enter another number") id [(ActionPrevious, always), (ActionNext, ifvalid)]
						  >>= \res -> case appFst fst res of
						  	(ActionNext,Just num2)	= step3 num1 num2
						  	(ActionPrevious,_)		= step1Back num1
						  							
	step2Back num1 num2	= updateInformationA ("Number 2","Enter another number") idView [(ActionPrevious, always), (ActionNext, ifvalid)] num2
						  >>= \res -> case appFst fst res of
						  	(ActionNext,Just num2`)	= step3 num1 num2`
						  	(ActionPrevious,_)		= step1Back num1
	
	step3 num1 num2		= showMessageAboutA ("Sum","The sum of those numbers is:") id [(ActionPrevious, always), (ActionOk, always)] (num1 + num2)
						>>= \res -> case appFst fst res of
							(ActionOk,sum)		= return sum
							(ActionPrevious,_)	= step2Back num1 num2

calculateSumParam :: !(Int,Int) -> Task Int
calculateSumParam (num1,num2) = showMessageAbout ("Sum","The sum of those numbers is:") (num1 + num2)
