module TestUnwrap
import iTasks

test :: Task ()
test = (updateInformation [] "Test for unwrapping a ui" @! () >>= return) <<@ ApplyLayout layout
where
    layout = unwrapUI

Start world = doTasks test world
