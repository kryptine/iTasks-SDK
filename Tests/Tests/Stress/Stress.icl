implementation module Tests.Stress.Stress 

import StressTestFramework

stressTest :: StressTestSuite
stressTest = { name        = "Stress tests"
	         , description = "General stress tests..."
	         , tests       = [ recursiveTask
                             , foreverTask
                             , recursiveTaskGrowingInp
                             , recursiveTaskGrowingRes
                             ]
	         }

recursiveTask = stest
    "Single recursive task"
    "A task that runs infinitely using recursion."
    t
    (\[cont] -> DoAction cont)
where
    t :: Task ()
    t = viewInformation () [] () >>| t

foreverTask = stest
    "Single forever task"
    "A task that runs infinitely using 'forever'."
    (forever (viewInformation () [] () >>| return ()))
    (\[cont] -> DoAction cont)

recursiveTaskGrowingInp = stest
    "Single recursive task (growing input)"
    "A task that runs infinitely using recursion and gets a list as input that increases at each iteration."
    (t [])
    (\[cont] -> DoAction cont)
where
    t :: [()] -> Task ()
    t l = viewInformation () [] () >>| t [():l]

recursiveTaskGrowingRes = stest
    "Single recursive task (growing result)"
    "A task that runs infinitely using recursion and returns a list that increases at each iteration."
    (t 0)
    (\[cont] -> DoAction cont)
where
    t :: Int -> Task ()
    t n = viewInformation () [] () @! repeatn n () >>| t (inc n)

