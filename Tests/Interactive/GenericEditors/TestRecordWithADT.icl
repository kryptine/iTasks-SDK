module TestRecordWithADT
import iTasks, iTasks.Util.Testing

:: ADTMultiCons
    = ADTMultiConsNone
    | ADTMultiConsSingle Int
    | ADTMultiConsMulti Int String

:: RecordWithADT =
    { first  :: String
    , second :: ADTMultiCons
    }

derive class iTask ADTMultiCons, RecordWithADT
derive gDefault ADTMultiCons, RecordWithADT

test :: Task RecordWithADT
test = testCommonInteractions "Record with ADT"

Start world = doTasks test world
