implementation module SmallExamples

import iTasks

smallExamples :: [Workflow]
smallExamples = [workflow "Examples/Miscellaneous/Calculate sum" calculateSum
				,workflow "Examples/Miscellaneous/Calculate sum (with stepback)" calculateSumSteps
				]

calculateSum :: Task Int
calculateSum
  =   enterInformation "Enter a number"
  >>= \num1 ->
      enterInformation "Enter another number"
  >>= \num2 ->
      let sum = (num1 + num2) in 
            showMessageAbout "The sum of those numbers is:" sum
        >>| return sum
        
calculateSumSteps :: Task Int
calculateSumSteps = step1First
where
	step1First			= enterInformationA "Enter a number" [] [ActionNext]
						  >>= \(_,num1) -> step2First num1
	step1Back num1		= updateInformationA "Enter a number" [] [ActionNext] num1
						  >>= \(_,num1`) -> step2First num1`
	
	step2First num1		= enterInformationA "Enter another number" [ActionPrevious] [ActionNext] 
						  >>= \(action,num2) -> case action of
						  							ActionPrevious	= step1Back num1
						  							ActionNext		= step3 num1 num2
	step2Back num1 num2	= updateInformationA "Enter another number" [ActionPrevious] [ActionNext] num2
						  >>= \(action,num2`) -> case action of
						  							ActionPrevious	= step1Back num1
						  							ActionNext		= step3 num1 num2`
	
	step3 num1 num2		= let sum = (num1 + num2) in
							showMessageAboutA "The sum of those numbers is:" [ActionPrevious,ActionOk] sum
							>>= \action -> case action of
													ActionPrevious	= step2Back num1 num2
													ActionOk		= return sum

