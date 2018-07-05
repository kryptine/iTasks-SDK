module TestIcon
import iTasks, iTasks.Internal.Test.Definition 

test :: Task (String,Maybe String)
test = testEditor icon ("icon-valid",Just "Icon with a tooltip!") Update

Start world = startEngine test world
