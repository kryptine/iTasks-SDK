module DynamicBPs

import MultiUser
import iTasks
import iTasks.Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> dynamicBPs1 (viewStep 42))
                          , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
                          , publish "/test" (WebApp []) (\_-> test 5)	
                          , publish "/test2" (WebApp []) (\_-> test2 5)	
                          ] world

viewStep :: Int -> Task Int
viewStep n = viewInformation ("Step " +++ toString n) [] n

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

test2 :: Int -> Task Int
test2 n
    =            updateInformation "step test" [] n
    >>*            [ OnAction ActionYes (always (test2 n))                               
                , OnAction ActionNo  (always (test2 n))
                , OnAction (Action "Pos" []) (ifValue ((<=) 0) test2)
                , OnAction (Action "Neg" []) (ifValue ((>=) 0) (\n -> test2 n))
                , OnAction (Action "Stable" []) (ifStable         test2)
                , OnAction (Action "Unstable" []) (ifUnstable         test2)
                , OnAction (Action "Cond" []) (ifCond (n>10)   (return n))
                ]

enterInt n
	= updateInformation "enter an integer value" [] n

do :: (Task a) -> Task a | iTask a
do task
 =				task
 >>= \res ->	viewInformation "result is:" [] res
 >>|			return res

