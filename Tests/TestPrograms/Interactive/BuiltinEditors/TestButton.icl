module TestButton
import iTasks, iTasks.Internal.Test.Definition

test :: Task Bool
test = testEditor (button <<@ (textAttr "Click")) False Update

Start world = startEngine test world
