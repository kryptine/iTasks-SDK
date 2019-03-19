module TestMoveSubs
import iTasks

test :: Task ()
test = taskToLayout "Test for moving a sub ui" <<@ ApplyLayout layout
where
    layout = moveSubUIs (SelectByPath [1]) [0] 0

Start world = doTasks test world
