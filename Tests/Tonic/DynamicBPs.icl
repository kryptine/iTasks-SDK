module DynamicBPs

import MultiUser
import iTasks
import iTasks.Framework.Tonic

Start :: *World -> *World
Start world = StartMultiUserTasks [ workflow "Dynamic Blueprints 1" "Dynamic Blueprints 1" dynamicBPs1
                                  , tonicStaticWorkflow []
                                  , tonicDynamicWorkflow []
                                  ] world

viewStep :: Int -> Task Int
viewStep n = viewInformation ("Step " +++ toString n) [] n

dynamicBPs1 :: Task Int
dynamicBPs1 = viewStep 1 >>| viewStep 2 >>| allTasks restOfSteps >>| viewStep 7 >>| viewStep 8
  where
  restOfSteps = map viewStep [3, 4, 5, 6]

