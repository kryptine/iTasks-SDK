module DynamicBPs

import MultiUser
import iTasks
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> dynamicBPs1 (viewStep 42))
                          , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
                          , publish "/test" (WebApp []) (\_-> test 5)	

                          , publish "/view" (WebApp []) (\_-> viewStep 5)	
                          , publish "/seq" (WebApp []) (\_-> seqTest 5)	
                          , publish "/or" (WebApp []) (\_-> orTest )	
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
  


test :: Int -> Task Int
test n
	= do (enterInt n) >>= \number -> allTasks [do (enterInt n) \\ n <- [1..number]] >>= \s -> return (sum s) 



stepTest :: Int -> Task Int
stepTest n
    =            updateInformation "step test" [] n
    >>*            [ OnAction ActionYes (always (stepTest n))                               
                , OnAction ActionNo  (always (stepTest n))
                , OnAction (Action "Pos" []) (ifValue ((<=) 0) stepTest)
                , OnAction (Action "Neg" []) (ifValue ((>=) 0) (\n -> stepTest n))
                , OnAction (Action "Stable" []) (ifStable         stepTest)
                , OnAction (Action "Unstable" []) (ifUnstable         stepTest)
                , OnAction (Action "Cond" []) (ifCond (n>10)   (return n))
                ]

enterInt n
	= updateInformation "enter an integer value" [] n

do :: (Task a) -> Task a | iTask a
do task
 =				task
 >>= \res ->	viewInformation "result is:" [] res
 >>|			return res

