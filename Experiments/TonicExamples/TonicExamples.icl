module TonicExamples

//import MoreExamples
import iTasks
import iTasks.Framework.Tonic

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_ -> enterViewTask)
                          , tonicPubTask "TonicExamples"] world

test1 :: Task Int
test1 = return 42

test2 :: Task Int
test2 = return 42 >>= \x -> return (x + 6)

enterViewTask :: Task Int
enterViewTask = enterInformation "Enter a number" [] >>= \x -> viewInformation "You have entered" [] x

