definition module StressTestFramework
import iTasks

:: StressTestSuite =
	{ name        :: String
	, description :: String
	, tests       :: [StressTestContainer]
	}
:: StressTestContainer = E.st: StressTestContainer (StressTest st) & iTask st

:: StressTest st =
    { name          :: String
    , description   :: String
    , taskUnderTest :: Task ()
    , testStep      :: [ActionWithTaskId] [EditorId] st -> (TestStepEvent, st)
    , initState     :: st
    }

:: TestStepEvent = DoAction ActionWithTaskId | Edit EditorId JSONNode
:: ActionWithTaskId     :== (String, Action)
:: EditorId             :== (String, String) // (taskId, editorId)

stestState :: String String (Task a) ([ActionWithTaskId] [EditorId] st -> (TestStepEvent, st)) st -> StressTestContainer | iTask a & iTask st
stest      :: String String (Task a) ([ActionWithTaskId] [EditorId]    -> TestStepEvent)          -> StressTestContainer | iTask a

tsAction :: ActionWithTaskId -> TestStepEvent
tsEdit   :: EditorId v       -> TestStepEvent | JSONEncode{|*|} v

runStressTests         :: [StressTestSuite] -> Task ()
exposedStressTestTasks :: [StressTestSuite] -> [PublishedTask]

