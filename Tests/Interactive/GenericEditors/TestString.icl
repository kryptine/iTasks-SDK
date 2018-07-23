module TestString
import iTasks, iTasks.Util.Testing

test :: Task String
test = testCommonInteractions "String"

Start world = startEngine test world
