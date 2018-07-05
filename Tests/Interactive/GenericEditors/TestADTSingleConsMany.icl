module TestADSingleConsMany
import iTasks, iTasks.Internal.Test.Definition 

:: ADTSingleConsMany = ADTSingleConsMany String Int String Int String
derive class iTask ADTSingleConsMany

test :: Task ADTSingleConsMany
test = testCommonInteractions "ADTSingleConsMany"

Start world = startEngine test world

