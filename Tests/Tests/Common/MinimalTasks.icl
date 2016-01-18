implementation module Tests.Common.MinimalTasks
import TestFramework
import iTasks.UI.Editor
import Data.Tuple

minimalEditor :: Task String
minimalEditor = updateInformation "Minimal String editor" [] "Hello World"

minimalEditlet :: Task String
minimalEditlet = updateInformation "Minimal String editlet" [UpdateUsing id const (fromEditlet editlet)] "Hello World"
where
	//Simple button
	editlet = { genUI      = \cid val w -> ({ComponentHTML|width=WrapSize,height=WrapSize,html=html cid},w)
			  , initClient = initClient
			  , appDiffClt = \_ cid n _ w -> (n,w)
			  , genDiffSrv = \o n -> if (o == n) Nothing (Just n) 
              , appDiffSrv = \n _ -> n
			  }

	html cid = ButtonTag [IdAttr (cid +++ "-button")] [Text "Click me"]

	//Register eventhandler for clicking the button
	initClient sv mkHandler cid world
		# (button, world) 	= .? (getElementById (cid +++ "-button")) world
		# world 			= jsSetObjectAttr "onclick" (toJSVal (mkHandler onClick cid)) button world
		= (sv,world)
	
 	onClick cid event cv world
		= (cv,Diff "Click" rollback,world)

	rollback _ cv world = (cv,NoDiff,world)

minimalStep :: Task String
minimalStep = enterInformation "Minimal Step combinator" []
            >>* [OnAction ActionOk (hasValue (updateInformation "Result" []))]

minimalParallel :: Task (String,String)
minimalParallel =  	 updateInformation "Edit string 1" [] "A"
				-&&- updateInformation "Edit string 2" [] "B"

//Minimal task to test adding and removing tasks from a parallel
minimalParallelOperations :: Task [Int] 
minimalParallelOperations 
	= parallel [(Embedded,editItem 0)] [pushAction,popAction] @? \(Value items s) -> Value [i \\ (_,Value i _) <- items] s
where
	editItem i list = updateInformation ("INITIAL: "<+++ i) [] i

	//Add an item
	pushAction = OnAction (Action "Push" []) (hasValue (\list -> (Embedded,editItem (length list))))
	//Remove the last item
	popAction = OnAction (Action "Pop" []) (ifValue (\list -> not (isEmpty list)) (const (Embedded, popItem)))

	popItem list //Kinda ugly that we need to add a task to remove one...
		= 	get (sdsFocus filter list) @ appSnd reverse
		>>- \(_,[remover,topofstack:_]) -> //We want to remove the one that was originally last, and the current task
			removeTask remover.TaskListItem.taskId list -&&- removeTask topofstack.TaskListItem.taskId list
		@?	const NoValue
	where
		filter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}

minimalForever :: Task String
minimalForever = forever (viewInformation () [] "Forever..." >>= return)
