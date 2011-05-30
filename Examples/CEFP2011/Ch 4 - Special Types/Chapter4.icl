implementation module Chapter4

// Examples showing the usage of special types

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine flows4 world

flows4 :: [Workflow]
flows4 =  [w1, w2, w3, w4, w8]

w1 = workflow "CEFP/Chap 4/1. Question"    		"Question" 								(show (question "Do you like iTask?" 42))
w2 = workflow "CEFP/Chap 4/2. List Choices" 	"Different ways to select a list" 		(show (selectList [1..5]))
w3 = workflow "CEFP/Chap 4/3. Text editor"  	"Simple way to enter a piece of text" 	(show (textEditor "some text"))
w4 = workflow "CEFP/Chap 4/4. Google map"  	    "Edit a Google Map" 					(show googleMap)
w8 = workflow "CEFP/Chap 4/5: Specialized type only accepting an odd number" "Type in an odd number" (show getOddNumber)

show :: (Task a) -> Task a | iTask a
show task = task >>= showMessageAbout "The result is:"

// Hello World

:: Approve = Yes | No

::  Questionnaire
    = { question :: Display  String
      , qid      :: Hidden   QID
      , answer   :: Editable Approve
      }
:: QID :== Int

derive class iTask Approve, Questionnaire

pose :: String QID -> Questionnaire
pose q i = { question = toDisplay q, qid = toHidden i, answer = toEditable Yes }

question :: String QID -> Task (QID,Approve)
question txt i
    =     ask txt i
      >>= showMessageAbout "The answer was:"

ask :: String QID -> Task (QID,Approve)
ask q i
    =           updateInformation "Please answer the question" (pose q i)
      >>= \r -> return (i,response r)

response :: Questionnaire -> Approve
response {answer} = fromEditable answer

// Different ways to edit a list

selectList list = updateInformation "Strange" 
						( list
						, Display list
						, Choice list 1
						, MultipleChoice list []
						) 

// Edit a piece of text

textEditor text = updateInformation "Enter text" (Note text)

// google map

import GoogleMaps

googleMap :: Task GoogleMap
googleMap = enterInformation "Google map "

// guarantee that a type has values with a certain property specializing gVerify


:: Odd = Odd Int

derive gVisualize 	Odd
derive gUpdate 		Odd
derive gDefaultMask Odd
derive JSONEncode 	Odd
derive JSONDecode 	Odd
derive gEq 			Odd

gVerify{|Odd|} mba st
	= wrapperVerify (Just "Type in an odd number") (\(Odd v) -> isOdd v) (\(Odd v) -> v +++> " is not an odd number") mba st

getOddNumber :: Task Int
getOddNumber 
	=						enterInformation "Type in an odd number" 
		>>= \(Odd n) ->		return n
