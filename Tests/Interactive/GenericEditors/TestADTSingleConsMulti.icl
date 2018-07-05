module TestADTSingleConsMulti
import iTasks, iTasks.Internal.Test.Definition 

:: ADTSingleConsMulti = ADTSingleConsMulti Int String
derive class iTask ADTSingleConsMulti

test :: Task ADTSingleConsMulti
test = testCommonInteractions "ADTSingleConsMulti"

Start world = startEngine test world

