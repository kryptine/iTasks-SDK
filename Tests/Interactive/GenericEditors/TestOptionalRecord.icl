module TestOptionalRecord
import iTasks, iTasks.Util.Testing

:: TwoFieldRecord =
    { first     :: Int
    , second    :: String
    }
derive class iTask TwoFieldRecord
derive gDefault TwoFieldRecord

test :: Task (Maybe TwoFieldRecord)
test = testCommonInteractions "Optional record"

Start world = doTasks test world
