module TestInsertSub
import iTasks

test :: Task ()
test = (updateInformation () [] "Test for inserting a sub ui" @! () >>= return) <<@ ApplyLayout layout
where
    layout = insertChildUI 1 (ui UIDebug)

Start world = startEngine test world
