implementation module SmallExamples

import iTasks

smallExamples :: [Workflow]
smallExamples = [workflow "Examples/Miscellaneous/Calculate sum" calculateSum]
	
calculateSum :: Task Int
calculateSum
  =   enterInformation "Enter a number"
  >>= \num1 ->
      enterInformation "Enter another number"
  >>= \num2 ->
      let sum = (num1 + num2) in 
            showMessageAbout "The sum of those numbers is:" sum
        >>| return sum