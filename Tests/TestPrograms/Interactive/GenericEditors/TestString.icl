module TestString
import iTasks, iTasks.Internal.Test.Definition

test :: Task String
test = testCommonInteractions "String"

Start world = startEngine test world
