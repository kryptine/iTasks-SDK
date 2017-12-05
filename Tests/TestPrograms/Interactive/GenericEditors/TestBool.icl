module TestBool
import iTasks, iTasks.Internal.Test.Definition 

test :: Task Bool
test = testCommonInteractions "Bool"

Start world = startEngine test world
