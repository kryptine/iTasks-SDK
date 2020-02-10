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
  (\sum ->			Title "Sum of 2 numbers, with view" @>> updateSharedInformation
  						[UpdateSharedAs (\(i,j) -> {firstNumber = i, secondNumber = j, sum = (i+j)})
  						          (\_ res -> Just (res.firstNumber,res.secondNumber)) (const o Just)] sum
  )
  >>= \(i,j) ->		return (i+j)
