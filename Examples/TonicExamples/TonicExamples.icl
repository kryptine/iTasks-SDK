module TonicExamples

//import MoreExamples
import iTasks
import iTasks.Framework.Tonic

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_ -> workAsRoot testTaskWithTaskArgs)
                          , tonicPubTask "TonicExamples"] world

workAsRoot :: (Task a) -> Task a | iTask a
workAsRoot t
  =       authenticateUser (Username "root") (Password "root") >>-
  \mbu -> case mbu of
            Just u -> workAs u t
            _      -> throw "Should not happen"

test1 :: Task Int
test1 = return 42

test2 :: Task Int
test2 = return 42 >>= \x -> return (x + 6)

enterViewTask :: Task Int
enterViewTask = enterInformation "Enter a number" [] >>= \x -> viewInformation "You have entered" [] x

testTaskWithTaskArgs :: Task Int
testTaskWithTaskArgs = taskWithTaskArgs test1

taskWithTaskArgs :: (Task Int) -> Task Int
taskWithTaskArgs t = t

testTaskWithTaskArgs` :: Task Int
testTaskWithTaskArgs` = taskWithTaskArgs` [test1, test2]

taskWithTaskArgs` :: [Task Int] -> Task Int
taskWithTaskArgs` ts = anyTask ts
