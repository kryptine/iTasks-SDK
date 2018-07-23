module TestNestedRecord
import iTasks, iTasks.Util.Testing

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

