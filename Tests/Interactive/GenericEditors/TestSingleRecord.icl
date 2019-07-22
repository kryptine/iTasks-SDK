module TestSingleRecord
import iTasks, iTasks.Util.Testing

:: TwoFieldRecord =
    { first     :: Int
    , second    :: String
    }
derive class iTask TwoFieldRecord
derive gDefault TwoFieldRecord

test :: Task TwoFieldRecord
test = testCommonInteractions "TwoFieldRecord"

Start world = doTasks test world

