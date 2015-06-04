module DynamicBPs

import MultiUser
import iTasks
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

import StdArray

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> dynamicBPs1 (viewStep 42))
                          , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
                          , publish "/test" (WebApp []) (\_-> test 5)	

                          , publish "/view" (WebApp []) (\_-> viewStep 5)	
                          , publish "/seq" (WebApp []) (\_-> seqTest 5)	
                          , publish "/or" (WebApp []) (\_-> orTest )	
                          , publish "/and" (WebApp []) (\_-> andTest )	
                          , publish "/andor" (WebApp []) (\_-> andOrTest )	
                          , publish "/any" (WebApp []) (\_-> anyTest )	
                          , publish "/anyall" (WebApp []) (\_-> anyAllTest 4 )	
                          , publish "/highin" (WebApp []) (\_-> highInTest seqTest )	
                          , publish "/highout" (WebApp []) (\_-> highOutTest seqTest)	
                          , publish "/highinout" (WebApp []) (\_-> highInOutTest )	

                          , publish "/updown" (WebApp []) (\_-> shareTest )	

                          , publish "/step" (WebApp []) (\_-> stepTest 5)	
                          ] world

viewStep :: Int -> Task Int
viewStep n = viewInformation ("Step " +++ toString n) [] n

seqTest :: Int -> Task Int
seqTest n 
| n>0 = updateInformation (toString n) [] n >>= \n -> seqTest (n-1)
| otherwise = return n

orTest :: Task Int
orTest = seqTest 3 -||- seqTest 2

andTest :: Task Int
andTest = seqTest 3 -&&- seqTest 2 >>= \(v1,v2) -> return (v1 + v2)

andOrTest :: Task Int
andOrTest = andTest -||- andTest

anyTest :: Task Int
anyTest = anyTask [viewStep 1, viewStep 2, viewStep 3]

anyAllTest :: Int -> Task Int
anyAllTest n 
	=			allTasks [updateInformation ("task " +++ toString n) [] i \\ i <- [0..n] ] 
	>>= \list -> 	anyTask [viewInformation ("task " +++ toString n) [] i \\ i <-list]

highInTest :: (Int -> Task Int) -> Task Int
highInTest ifun
	=			enterInformation "enter a number:" []
	>>= \i ->	if (i > 0) (ifun i) (return i)

highOutTest :: (Int -> Task Int) -> Task Int
highOutTest itask 
	= 				enterInformation "enter a number:" []
	>>= \n ->		itask n
	>>= \m ->		itask m
	>>= \o -> 		return (itask (n+m+o))
	>>= \task -> 	task

highInOutTest :: Task Int
highInOutTest 
	= 				highInOut [viewStep 7, seqTest 2, andOrTest, anyTest, anyAllTest 3, highInOutTest] 
	>>= \task ->	task

highInOut:: [Task Int] -> Task (Task Int)
highInOut funs
	=			enterChoice "choose a task" [ChooseWith (AutoChoice (\t -> toSingleLineText t))] funs
	>>= \fun ->	return fun
 
stepTest :: Int -> Task Int
stepTest n
    =            updateInformation "step test" [] n
    >>*         [ OnAction ActionYes (always (stepTest n))                               
                , OnAction ActionNo  (always (stepTest n))
                , OnAction (Action "Pos" []) (ifValue ((<=) 0) stepTest)
                , OnAction (Action "Neg" []) (ifValue ((>=) 0) (\n -> stepTest n))
                , OnAction (Action "Stable" []) (ifStable         stepTest)
                , OnAction (Action "Unstable" []) (ifUnstable         stepTest)
                , OnAction (Action "Cond" []) (ifCond (n>10)   (return n))
				, OnValue  (ifCond (n>1000) (return n))
				, OnException  (\n -> return n)
                ]

enterInt n
	= updateInformation "enter an integer value" [] n

shareTest :: Task Int
shareTest
	=		withShared 0 (\s -> up s -||- down s)
where
	up s 	=  viewSharedInformation "up" [] s >>= \n -> set (n+1) s >>| up s
	down s 	=  viewSharedInformation "down" [] s >>= \n -> set (n-1) s >>| down s





test :: Int -> Task Int
test n
	= do (enterInt n) >>= \number -> allTasks [do (enterInt n) \\ n <- [1..number]] >>= \s -> return (sum s) 

do :: (Task a) -> Task a | iTask a
do task
 =				task
 >>= \res ->	viewInformation "result is:" [] res
 >>|			return res

dynamicBPs1 :: (Task Int) -> Task Int
dynamicBPs1 vs42
  =   viewStep 1
  >>| if True (viewStep 14) (viewStep 15)
  >>| viewStep 10
  >>| (      viewStep 11
       -&&- (viewStep 12 >>| viewStep 13))
  >>| viewStep 2
  >>| vs42
  >>| viewStep 3
  >>| allTasks restOfSteps
  >>| viewStep 8
  >>| viewStep 9
  where
  restOfSteps = map viewStep [4, 5, 6, 7]
  
// sheets




myTask :: Task Int
myTask = enterInformation "Enter an integer" []

palindrome :: Task (Maybe String)
palindrome 
	=   		enterInformation "Enter a palindrome" []
    	>>* 	[ OnAction  ActionOk     (ifValue isPalindrome  (\v -> return (Just v)))
               	, OnAction  ActionCancel (always	            (return Nothing))
               	]
where
	isPalindrome s = l == reverse l where l = [e \\ e <-: s]



:: Person 	=  { name :: String, gender :: Gender, dateOfBirth :: Date}
:: Gender 	= Male | Female

derive class iTask Person, Gender

/*
person1by1 :: [Person] -> Task [Person]
person1by1 persons
	=		enterInformation "Add a person" []
		    -|| 
		    viewInformation "List so far.." [] persons
	 >>*  	[ OnAction (Action "Add" [])    (hasValue  (\v -> person1by1 [v : persons]))  
		    , OnAction (Action "Finish" []) (always (return persons))
		    , OnAction ActionCancel 	    (always (return []))
	        ]
*/

calculateSum :: Task Int
calculateSum
  =   			enterInformation ("Number 1","Enter a number") []
  	>>= \num1 ->	enterInformation ("Number 2","Enter another number") []
 	>>= \num2 ->	viewInformation   ("Sum","The sum of those numbers is:") [] (num1 + num2)
 	


twoTasks :: User User  -> Task a      	   | iTask a 
twoTasks user1 user2   
= withShared defaultValue (\share -> (user1  @: updateSharedInformation  ("Shared with" <+++ user2) [] share)
		              		                 -||  
		              		         ( user2  @: viewSharedInformation   ("Shared with" <+++ user1)  [] share) )











