module TestADTMultiCons

import iTasks, iTasks.Util.Testing

:: ADTMultiCons
    = ADTMultiConsNone
    | ADTMultiConsSingle Int
    | ADTMultiConsMulti Int String

derive class iTask ADTMultiCons
derive gDefault ADTMultiCons

test :: Task ADTMultiCons
test = testCommonInteractions "ADTMultiCons"

Start world = doTasks test world

