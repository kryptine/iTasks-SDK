module TestRecordWithADT
import iTasks, iTasks.Internal.Test.Definition 

:: ADTMultiCons
    = ADTMultiConsNone
    | ADTMultiConsSingle Int
    | ADTMultiConsMulti Int String

:: RecordWithADT =
    { first  :: String
    , second :: ADTMultiCons
    }

derive class iTask ADTMultiCons, RecordWithADT

test :: Task RecordWithADT
test = testCommonInteractions "Record with ADT"

Start world = startEngine test world
