module TestNestedRecord
import iTasks, iTasks.Internal.Test.Definition 

:: TwoFieldRecord =
    { first     :: Int
    , second    :: String
    }

:: NestedRecord =
    { firstTwo  :: TwoFieldRecord
    , third     :: Bool
    }
derive class iTask TwoFieldRecord, NestedRecord

test :: Task NestedRecord
test = testCommonInteractions "NestedRecord"

Start world = startEngine test world

