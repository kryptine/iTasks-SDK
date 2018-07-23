module TestBool
import iTasks, iTasks.Util.Testing

test :: Task Bool
test = testCommonInteractions "Bool"

Start world = startEngine test world
