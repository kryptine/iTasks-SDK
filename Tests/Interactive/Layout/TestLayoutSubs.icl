module TestLayoutSubs
import iTasks

test :: Task ()
test = (updateInformation [] "Test for layouting a sub ui" @! () >>! return) <<@ ApplyLayout layout
where
    layout = layoutSubUIs (SelectByPath [0]) (setUIType UIDebug)

Start world = doTasks test world
