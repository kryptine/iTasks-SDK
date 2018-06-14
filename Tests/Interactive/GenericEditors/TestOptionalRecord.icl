module TestOptionalRecord
import iTasks, iTasks.Internal.Test.Definition 

:: TwoFieldRecord =
    { first     :: Int
    , second    :: String
    }
derive class iTask TwoFieldRecord

test :: Task (Maybe TwoFieldRecord)
test = testCommonInteractions "Optional record"

Start world = startEngine test world
