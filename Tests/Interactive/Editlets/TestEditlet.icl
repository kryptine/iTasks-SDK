module TestEditlet
import iTasks
import iTasks.Extensions.Clock

test = viewSharedInformation "Clock" [ViewAs (\t -> AnalogClock t)] currentTime

Start world = doTasks test world
