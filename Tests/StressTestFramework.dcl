definition module StressTestFramework
import iTasks

:: StressTestSuite =
	{ name        :: String
	, description :: String
	, tests       :: [StressTest]
	}

:: StressTest =
    { name          :: String
    , description   :: String
    , taskUnderTest :: Task ()
    , testStep      :: [ActionWithTaskId] -> TestStepEvent
    }

:: ActionWithTaskId :== (String, Action)
:: TestStepEvent = DoAction ActionWithTaskId //| Edit

stest :: String String (Task a) ([ActionWithTaskId] -> TestStepEvent) -> StressTest | iTask a

runStressTests :: [StressTestSuite] -> Task ()
exposedStressTestTasks :: [StressTestSuite] -> [PublishedTask]

