implementation module SmallExamples

import iTasks

smallExamples :: [Workflow]
smallExamples = [workflow "Examples/Miscellaneous/Calculate sum" calculateSum
				,workflow "Examples/Miscellaneous/Calculate sum (with stepback)" calculateSumSteps
				]

calculateSum :: Task Int
calculateSum
  =   enterInformation "Number 1" "Enter a number"
  >>= \num1 ->
      enterInformation "Number 2" "Enter another number"
  >>= \num2 ->
      showMessageAbout "Sum" "The sum of those numbers is:" (num1 + num2)
        
calculateSumSteps :: Task Int
calculateSumSteps = step1First
where
	step1First			= enterInformationA "Number 1" "Enter a number" [ButtonAction (ActionNext, IfValid)]
						  >>= \(_,num1) -> step2First num1
	step1Back num1		= updateInformationA "Number 1" "Enter a number" [ButtonAction (ActionNext, IfValid)] num1
						  >>= \(_,num1`) -> step2First num1`
	
	step2First num1		= enterInformationA "Number 2" "Enter another number" [ButtonAction (ActionPrevious, Always), ButtonAction (ActionNext, IfValid)]
						  >>= \(action,num2) -> case action of
						  							ActionPrevious	= step1Back num1
						  							ActionNext		= step3 num1 num2
	step2Back num1 num2	= updateInformationA "Number 2" "Enter another number" [ButtonAction (ActionPrevious, Always), ButtonAction (ActionNext, IfValid)] num2
						  >>= \(action,num2`) -> case action of
						  							ActionPrevious	= step1Back num1
						  							ActionNext		= step3 num1 num2`
	
	step3 num1 num2		= let sum = (num1 + num2) in
							showMessageAboutA "Sum" "The sum of those numbers is:" [ButtonAction (ActionPrevious, Always), ButtonAction (ActionOk, Always)] sum
							>>= \(action,_) -> case action of
													ActionPrevious	= step2Back num1 num2
													ActionOk		= return sum

