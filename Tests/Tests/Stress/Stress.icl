implementation module Tests.Stress.Stress 

import StressTestFramework

stressTest :: StressTestSuite
stressTest = { name        = "Stress tests"
	         , description = "General stress tests..."
	         , tests       = [ recursiveTask
                             , foreverTask
                             , recursiveTaskGrowingInp
                             , recursiveTaskGrowingRes
                             , editInt
                             ]
	         }

recursiveTask = stest
    "Single recursive task"
    "A task that runs infinitely using recursion."
    t
    (\[cont] _ -> DoAction cont)
where
    t :: Task ()
    t = viewInformation () [] () >>| t

foreverTask = stest
    "Single forever task"
    "A task that runs infinitely using 'forever'."
    (forever (viewInformation () [] () >>| return ()))
    (\[cont] _ -> DoAction cont)

recursiveTaskGrowingInp = stest
    "Single recursive task (growing input)"
    "A task that runs infinitely using recursion and gets a list as input that increases at each iteration."
    (t [])
    (\[cont] _ -> DoAction cont)
where
    t :: [()] -> Task ()
    t l = viewInformation () [] () >>| t [():l]

recursiveTaskGrowingRes = stest
    "Single recursive task (growing result)"
    "A task that runs infinitely using recursion and returns a list that increases at each iteration."
    (t 0)
    (\[cont] _ -> DoAction cont)
where
    t :: Int -> Task ()
    t n = viewInformation () [] () @! repeatn n () >>| t (inc n)

editInt = stestState
    "Edit single integer"
    "An integer editor task of which the value is changed repeatedly."
    (updateInformation () [] 0)
    (\[] [intEditor] i -> (tsEdit intEditor i, inc i))
    0

