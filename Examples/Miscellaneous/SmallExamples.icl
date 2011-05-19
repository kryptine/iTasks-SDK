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
	step1First			= enterInformationA ("Number 1","Enter a number") (\mbN -> [(ActionNext, mbN)])
						  >>= step2First
						  
	step1Back num1		= updateInformationA ("Number 1","Enter a number") idView (\mbN -> [(ActionNext, mbN)]) num1
						  >>= step2First
	
	step2First num1		= enterInformationA ("Number 2","Enter another number") (\mbN -> [(ActionPrevious, Just Nothing), (ActionNext, maybe Nothing Just mbN)])
						  >>= \mbN -> case mbN of
						  	Just num2			= step3 num1 num2
						  	_					= step1Back num1
						  							
	step2Back num1 num2	= updateInformationA ("Number 2","Enter another number") idView (\mbN -> [(ActionPrevious, Just Nothing), (ActionNext, fmap Just mbN)]) num2
						  >>= \res -> case res of
						  	(Just num2`)		= step3 num1 num2`
						  	_					= step1Back num1
	
	step3 num1 num2		= return (num1 + num2)
						>>= \sum -> showMessageAboutA ("Sum","The sum of those numbers is:") id [(ActionPrevious,ActionPrevious),(ActionOk,ActionOk)] sum
						>>= \res -> case res of
							ActionOk		= return sum
							ActionPrevious	= step2Back num1 num2

calculateSumParam :: !(Int,Int) -> Task Int
calculateSumParam (num1,num2) = showMessageAbout ("Sum","The sum of those numbers is:") (num1 + num2)
