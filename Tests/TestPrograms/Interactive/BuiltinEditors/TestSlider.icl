module TestSlider
import iTasks, iTasks.Internal.Test.Definition 

test :: Task Int
test = testEditor (slider <<@ ('DM'.union (minAttr 1) (maxAttr 5))) 3 Update

Start world = startEngine test world
