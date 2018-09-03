module TestReal
import iTasks, iTasks.Util.Testing

test :: Task Real
test = testCommonInteractions "Real"

Start world = doTasks test world
