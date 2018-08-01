module TestADTSingleConsMulti
import iTasks, iTasks.Util.Testing

:: ADTSingleConsMulti = ADTSingleConsMulti Int String
derive class iTask ADTSingleConsMulti

test :: Task ADTSingleConsMulti
test = testCommonInteractions "ADTSingleConsMulti"

Start world = doTasks test world

