module TestSingleRecord

import iTasks, iTasks.Internal.Test.Definition 

:: TwoFieldRecord =
    { first     :: Int
    , second    :: String
    }
derive class iTask TwoFieldRecord

test :: Task TwoFieldRecord
test = testCommonInteractions "TwoFieldRecord"

Start world = startEngine test world

