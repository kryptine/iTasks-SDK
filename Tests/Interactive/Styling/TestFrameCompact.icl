module TestFrameCompact
import iTasks

test :: Task Int
test = viewInformation [] 23 <<@ ApplyLayout frameCompact

Start world = doTasks test world
