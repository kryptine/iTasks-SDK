module DynamicBPs

import MultiUser
import iTasks
import iTasks.Framework.Tonic

Start :: *World -> *World
Start world = StartMultiUserTasks [ workflow "Dynamic Blueprints 1" "Dynamic Blueprints 1" (dynamicBPs1 (viewStep 42))
                                  , tonicStaticWorkflow []
                                  , tonicDynamicWorkflow []
                                  ] world

viewStep :: Int -> Task Int
viewStep n = viewInformation ("Step " +++ toString n) [] n

dynamicBPs1 :: (Task Int) -> Task Int
dynamicBPs1 vs42 = viewStep 1 >>| (viewStep 11 -&&- (viewStep 12 >>| viewStep 13)) >>| viewStep 2 >>| vs42 >>| viewStep 3 >>| allTasks restOfSteps >>| viewStep 8 >>| viewStep 9
  where
  restOfSteps = map viewStep [4, 5, 6, 7]

