module TestSetType
import iTasks

test :: Task ()
test = (updateInformation () [] "Test for setting a UI type" @! () >>= return) <<@ ApplyLayout layout
where
    layout = setUIType UIDebug

Start world = startEngine test world
