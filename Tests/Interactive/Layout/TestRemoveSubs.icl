module TestRemoveSubs
import iTasks

test :: Task ()
test = (updateInformation () [] "Test for removing a sub ui" @! () >>= return) <<@ ApplyLayout layout
where
    layout = removeSubUIs (SelectByPath [1])

Start world = doTasks test world
