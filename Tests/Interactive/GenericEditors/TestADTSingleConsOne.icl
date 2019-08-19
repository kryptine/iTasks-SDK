module TestADTSingleConsOne
import iTasks, iTasks.Util.Testing

:: ADTSingleCons = ADTSingleCons Int
derive class iTask ADTSingleCons
derive gDefault ADTSingleCons

test :: Task ADTSingleCons
test = testCommonInteractions "ADTSingleCons"

Start world = doTasks test world

