module TestChar
import iTasks, iTasks.Internal.Test.Definition

test :: Task Char
test = testCommonInteractions "Char"

Start world = startEngine test world
