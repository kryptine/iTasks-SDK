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

view42 :: Task Int
view42 = viewInformation "Here's a number" [] 42

enterViewTask :: Task Int
enterViewTask = enterInformation "Enter a number" [] >>= \x -> viewInformation "You have entered" [] x

testTaskWithTaskArgs :: Task Int
testTaskWithTaskArgs = taskWithTaskArgs view42

taskWithTaskArgs :: (Task Int) -> Task Int
taskWithTaskArgs t = t

/*
taskWithTaskArgs t = t

will be

taskWithTaskArgs t = tonicReflection "TonicExamples" "taskWithTaskArgs" t

and

testTaskWithTaskArgs = taskWithTaskArgs view42

will be

testTaskWithTaskArgs = tonicReflection "TonicExamples" "testTaskWithTaskArgs" (taskWithTaskArgs view42)


tonicReflection "TonicExamples" "testTaskWithTaskArgs" (taskWithTaskArgs view42)

=>

tonicreflection "tonicexamples" "testTaskWithTaskArgs" (tonicreflection "tonicexamples" "taskwithtaskargs" view42)

=>

tune (ModuleTaskName "tonicexamples" "testTaskWithTaskArgs") (tune (ModuleTaskName "tonicexamples" "taskwithtaskargs") view42)

=>

...

=>

tune (ModuleTaskName "tonicexamples" "testTaskWithTaskArgs") (tune (ModuleTaskName "tonicexamples" "taskwithtaskargs") (Task mtn eval))

=>

tune (ModuleTaskName "tonicexamples" "testTaskWithTaskArgs") (Task (Just (ModuleTaskName "tonicexamples" "taskwithtaskargs")) eval)

=>

tune (ModuleTaskName "tonicexamples" "testTaskWithTaskArgs") (Task (Just (ModuleTaskName "tonicexamples" "taskwithtaskargs")) eval)

=>

Task (Just (ModuleTaskName "tonicexamples" "testTaskWithTaskArgs")) eval

thus

testTaskWithTaskArgs = Task (Just (ModuleTaskName "tonicexamples" "testTaskWithTaskArgs")) eval

*/

testTaskWithTaskArgs` :: Task Int
testTaskWithTaskArgs` = taskWithTaskArgs` [view42]

taskWithTaskArgs` :: [Task Int] -> Task Int
taskWithTaskArgs` ts = anyTask ts

