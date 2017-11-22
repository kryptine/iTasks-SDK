module TestCopyAttributes
import iTasks

test :: Task ()
test = (updateInformation () [] "Test for copying an attribute" @! () >>= return) <<@ ApplyLayout layout
where
    layout = copySubUIAttributes SelectAll [0] [1]

Start world = startEngine test world 
