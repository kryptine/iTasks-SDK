module TestIntList
import iTasks, iTasks.Internal.Test.Definition 

test :: Task [Int]
test = testCommonInteractions "Int list"

Start world = startEngine test world

