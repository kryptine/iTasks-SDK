module TestSequenceLayouts
import iTasks

test :: Task ()
test = taskToLayout "Test for sequencing layouts" <<@ ApplyLayout layout
where
    layout = sequenceLayouts (setUIType UIStep) (setUIAttributes (styleAttr "background: #ff0"))

Start world = startEngine test world
