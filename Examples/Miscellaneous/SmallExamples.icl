implementation module SmallExamples

import iTasks

smallExamples :: [Workflow]
smallExamples = [workflow	"Examples/Miscellaneous/Calculate sum" "Calculate the sum of two numbers" calculateSum
				,workflow	"Examples/Miscellaneous/Calculate sum (with stepback)" "Calculate the sum of two numbers, with stepback possibility" calculateSumSteps
				,workflow	"Examples/Miscellaneous/Calculate sum (parameterised)" "Calculate the sum of two numbers given as parameter" calculateSumParam
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
	step1First			=		enterInformationA ("Number 1","Enter a number")
						  >?*	[(ActionNext, IfValid (\num -> step2First num))]
						  
	step1Back num1		=		updateInformationA ("Number 1","Enter a number") idView num1
						  >?*	[(ActionNext, IfValid (\num -> step2First num))]
	
	step2First num1		=		enterInformationA ("Number 2","Enter another number")
						  	>?*	[ (ActionPrevious,	Always	(step1Back num1))
						  		, (ActionNext,		IfValid (\num2 -> step3 num1 num2))
						  		]
						  							
	step2Back num1 num2	=		updateInformationA ("Number 2","Enter another number") idView num2
							>?*	[ (ActionPrevious,	Always	(step1Back num1))
								, (ActionNext,		IfValid	(\num2` -> step3 num1 num2`))
								]
	
	step3 num1 num2		= return (num1 + num2)
						>>= \sum -> showMessageAboutA ("Sum","The sum of those numbers is:") id sum
						>>*	[ (ActionPrevious,	step2Back num1 num2)
							, (ActionOk,		return sum)
							]

calculateSumParam :: !(Int,Int) -> Task Int
calculateSumParam (num1,num2) = showMessageAbout ("Sum","The sum of those numbers is:") (num1 + num2)
