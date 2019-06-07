implementation module BasicAPIExamples.SequentialExamples.CalculateSumInShare

// calculate sum in a shared record

import iTasks

wf :: String -> Workflow
wf a = workflow a "Calculate sum in share" calculateSumInRecord

main :: Task ()
main = calculateSumInRecord @! ()

:: MySum = {firstNumber :: Int, secondNumber :: Int, sum :: Int}

derive class iTask MySum


calculateSumInRecord :: Task Int
calculateSumInRecord
  = 				withShared (0,0)
  (\sum ->			updateSharedInformation
						[UpdateSharedWithTitle "Sum of 2 numbers, with view"
  						,UpdateSharedAs (\(i,j) -> {firstNumber = i, secondNumber = j, sum = (i+j)})
  						          (\_ res -> (res.firstNumber,res.secondNumber)) const] sum
  )
  >>= \(i,j) ->		return (i+j)
