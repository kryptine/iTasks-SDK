implementation module BasicAPIExamples.SequentialExamples.CalculateSumInShare

// calculate sum in a shared record

import iTasks

wf :: String -> Workflow
wf a = workflow a "Calculate sum in share" calculateSumInRecord

Start :: *World -> *World
Start world
	= startEngine calculateSumInRecord world

:: MySum = {firstNumber :: Int, secondNumber :: Int, sum :: Int}

derive class iTask MySum


calculateSumInRecord :: Task Int
calculateSumInRecord
  = 				withShared (0,0)
  (\sum ->			updateSharedInformation ("Sum of 2 numbers, with view","")
  						[UpdateAs (\(i,j) -> {firstNumber = i, secondNumber = j, sum = (i+j)})
  						          (\_ res -> (res.firstNumber,res.secondNumber))] sum
  )
  >>= \(i,j) ->		return (i+j)
