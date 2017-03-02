implementation module Tests.Stress.Stress 

import StressTestFramework
import iTasks.API.Extensions.SVG.SVGEditor

stressTest :: StressTestSuite
stressTest = { name        = "Stress tests"
	         , description = "General stress tests..."
	         , tests       = [ recursiveTask
                             , recursiveTaskComplex
                             , foreverTask
                             , recursiveTaskGrowingInp
                             , recursiveTaskGrowingRes
                             , editInt
                             , editIntReset
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


recursiveTaskComplex = stest
    "Complex recursive task"
    "A compley task (SVG showing Dutch flag) that runs infinitely using recursion."
    tut
    (\[cont] _ -> DoAction cont)
where
    tut :: Task ()
    tut = ((get currentTimestamp >>= viewInformation () []) ||- updateInformation "SVG image" [UpdateUsing id (const id) (fromSVGEditor svgeditor)] 42) >>| tut
	svgeditor = {SVGEditor|initView=const (),renderImage = \_ _ _ -> nederland, updView = \m v -> v, updModel = \m v -> m}

	nederland :: Image m
	nederland = banden (H *. 3 /. 2,H) [toSVGColor {r=174,g=28,b=40},toSVGColor "white",toSVGColor {r=33,g=70,b=139}]

	banden (w,h) kleuren = above [] [] [] [rect w (h /. (length kleuren)) <@< {fill = kleur} <@< {stroke = toSVGColor "none"} \\ kleur <- kleuren] NoHost

	H = px 32.0
	W = H *. 1.5

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

editIntReset = stestState
    "Edit single integer and reset"
    "An integer editor task of which the value is changed repeatedly. The task is recursively continued after 50 iterations."
    t
    step
    0
where
    t :: Task ()
    t = updateInformation () [] 0 >>| t

    step [cont] _           50 = (tsAction cont, 0)
    step _      [intEditor]  i = (tsEdit intEditor i, inc i)

