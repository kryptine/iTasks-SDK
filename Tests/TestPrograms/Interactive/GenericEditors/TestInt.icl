module TestInt
import iTasks, iTasks.Internal.Test.Definition 

test :: Task Int
test = testCommonInteractions "Int"

Start world = startEngine test world
