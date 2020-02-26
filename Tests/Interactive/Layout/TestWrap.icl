module TestWrap
import iTasks

test :: Task ()
test = (updateInformation [] "Test for wrapping a ui" @! () >>! return) <<@ ApplyLayout layout
where
    layout = wrapUI UIDebug

Start world = doTasks test world
