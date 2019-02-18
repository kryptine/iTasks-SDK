module TestADTSingleConsMany
import iTasks, iTasks.Util.Testing

:: ADTSingleConsMany = ADTSingleConsMany String Int String Int String
derive class iTask ADTSingleConsMany

test :: Task ADTSingleConsMany
test = testCommonInteractions "ADTSingleConsMany"

Start world = doTasks test world

